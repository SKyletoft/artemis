use std::{
	cmp::Ordering,
	collections::HashMap,
	fmt,
	hash::Hash,
	ops::{Deref, DerefMut},
};

use anyhow::Result;
use smallvec::SmallVec;
use variantly::Variantly;

use crate::simplify::{
	Block as SimpleBlock, BlockId, Register as SimpleRegister, SSAConstruct, SimpleBinOp,
	SimpleExpression, SimpleFunctionCall, SimpleOp, SimpleUnOp, Source,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Configuration {
	pub general_purpose_registers: u64,
	pub floating_point_registers: u64,
	pub argument_registers: u64,
	pub temporary_registers: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CodeConstruct {
	Function {
		name: SmallString,
		blocks: Vec<Block>,
	},
	Variable {
		name: SmallString,
		value: u64,
	},
	ImmediateExpression {
		name: SmallString,
		value: Vec<Block>,
	},
}

#[derive(Clone, PartialEq, Eq, Copy, Debug, Hash, Variantly)]
pub enum Register {
	Literal(u64),
	FloatingPoint(u64),
	GeneralPurpose(u64),
	StackPointer,
	FramePointer,
	ProgramCounter,
}

impl fmt::Display for Register {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Register::Literal(v) => write!(f, "{v}"),
			Register::FloatingPoint(v) => write!(f, "$F{v}"),
			Register::GeneralPurpose(v) => write!(f, "${v}"),
			Register::StackPointer => write!(f, "$SP"),
			Register::FramePointer => write!(f, "$FP"),
			Register::ProgramCounter => write!(f, "$PC"),
		}
	}
}

#[derive(Clone, PartialEq, Eq, Copy, Debug)]
pub enum Op {
	Add,
	Sub,
	Abs,
	Mul,
	Div,
	FAdd,
	FSub,
	FAbs,
	FMul,
	FDiv,
	And,
	Or,
	Xor,
	Not,
	StoreMem,
	LoadMem,
	Move,
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Op::Add => write!(f, "+"),
			Op::Sub => write!(f, "-"),
			Op::Abs => write!(f, "±"),
			Op::Mul => write!(f, "×"),
			Op::Div => write!(f, "÷"),
			Op::FAdd => write!(f, "+."),
			Op::FSub => write!(f, "-."),
			Op::FAbs => write!(f, "±."),
			Op::FMul => write!(f, "×."),
			Op::FDiv => write!(f, "÷."),
			Op::And => write!(f, "Λ"),
			Op::Or => write!(f, "V"),
			Op::Xor => write!(f, "⊕"),
			Op::Not => write!(f, "¬"),
			Op::Move => write!(f, ""),
			Op::StoreMem => write!(f, "s"),
			Op::LoadMem => write!(f, "l"),
		}
	}
}

impl From<&SimpleOp> for Op {
	fn from(op: &SimpleOp) -> Self {
		match op {
			SimpleOp::Add => Op::Add,
			SimpleOp::Sub => Op::Sub,
			SimpleOp::Abs => Op::Abs,
			SimpleOp::Mul => Op::Mul,
			SimpleOp::Div => Op::Div,
			SimpleOp::FAdd => Op::FAdd,
			SimpleOp::FSub => Op::FSub,
			SimpleOp::FAbs => Op::FAbs,
			SimpleOp::FMul => Op::FMul,
			SimpleOp::FDiv => Op::FDiv,
			SimpleOp::And => Op::And,
			SimpleOp::Or => Op::Or,
			SimpleOp::Xor => Op::Xor,
			SimpleOp::Not => Op::Not,
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PhiEdge {
	pub from: BlockId,
	pub value: Register,
}

impl fmt::Debug for PhiEdge {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let PhiEdge { from, value } = self;
		write!(f, "{from}:{value}")
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct PhiNode {
	pub target: Register,
	pub value: SmallVec<[PhiEdge; 2]>,
}

impl fmt::Debug for PhiNode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let PhiNode { target, value } = self;
		write!(f, "{target} ← φ{value:?}")
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
	pub block: SmallVec<[Expression; 4]>,
	pub out: BlockEnd,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BinOp {
	pub target: Register,
	pub op: Op,
	pub lhs: Register,
	pub rhs: Register,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnOp {
	pub target: Register,
	pub op: Op,
	pub lhs: Register,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub target: Register,
	pub function: SmallString,
	pub args: SmallVec<[Register; 4]>,
}

#[derive(Clone, PartialEq)]
pub enum Expression {
	BinOp(BinOp),
	UnOp(UnOp),
	FunctionCall(FunctionCall),
}

/// Vec<Option<Source>>;
#[derive(Debug, Clone, PartialEq)]
pub struct RegisterSet(Vec<Option<Source>>);

impl Deref for RegisterSet {
	type Target = [Option<Source>];

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl DerefMut for RegisterSet {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl RegisterSet {
	pub fn new(width: u64) -> Self {
		Self(vec![None; width as usize])
	}

	pub fn index_of(&self, source: &Source) -> Option<usize> {
		self.0.iter().position(|x| *x == Some(*source))
	}
}

#[derive(Debug, Clone, PartialEq)]
struct State {
	general_purpose: RegisterSet,
	floating_point: RegisterSet,
	stack: Vec<SimpleRegister>,
	max_stack_size: usize,
}

impl From<&Configuration> for State {
	fn from(config: &Configuration) -> Self {
		Self {
			general_purpose: RegisterSet::new(config.general_purpose_registers),
			floating_point: RegisterSet::new(config.floating_point_registers),
			stack: Vec::new(),
			max_stack_size: 0,
		}
	}
}

impl fmt::Debug for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::BinOp(BinOp {
				target,
				op,
				lhs,
				rhs,
			}) => write!(f, "{target} ← {lhs} {op} {rhs}"),
			Expression::UnOp(UnOp { target, op, lhs }) => {
				write!(f, "{target} ← {op} {lhs}")
			}
			Expression::FunctionCall(FunctionCall {
				target,
				function,
				args,
			}) => write!(f, "{target} ← {function}{args:?}"),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Variantly)]
pub enum BlockEnd {
	#[variantly(rename = "ret")]
	Return(Register),
	One(BlockId),
	Two(Register, BlockId, BlockId),
}

impl fmt::Debug for BlockEnd {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Return(reg) => write!(f, "ret {reg}"),
			Self::One(target) => write!(f, "{target}"),
			Self::Two(reg, left, right) => write!(f, "{reg} ? {left}, {right}"),
		}
	}
}

impl Default for BlockEnd {
	fn default() -> Self {
		BlockEnd::Return(Register::GeneralPurpose(0))
	}
}

pub fn register_allocate(
	ssa: &[SSAConstruct],
	config: &Configuration,
) -> Result<Vec<CodeConstruct>> {
	ssa.iter()
		.map(|construct| {
			let res = match construct {
				SSAConstruct::Function { name, blocks } => {
					CodeConstruct::Function {
						name: name.clone(),
						blocks: allocate_for_block(blocks, config)?,
					}
				}
				SSAConstruct::Variable { .. } => todo!(),
				SSAConstruct::ImmediateExpression { .. } => todo!(),
			};
			Ok(res)
		})
		.collect()
}

fn compare_block_line(
	(l_block, l_line): (usize, usize),
	(r_block, r_line): (usize, usize),
) -> bool {
	match r_block.cmp(&l_block) {
		Ordering::Less => false,
		Ordering::Equal => r_line < l_line,
		Ordering::Greater => true,
	}
}

fn select_register(
	is_floating_point: bool,
	pos @ (block_idx, line_idx): (usize, usize),
	state: &State,
	last_use: &HashMap<Source, (usize, usize)>,
	protected_registers: &[Register],
) -> Register {
	assert!(!is_floating_point, "Todo: floating point");

	if let Some(first_unused) =
		state.general_purpose
			.iter()
			.enumerate()
			.position(|(idx, &reg)| {
				// Completely unused register
				let inner_reg = if let Some(r) = reg {
					r
				} else {
					return true;
				};

				// Don't select anything we need in the same operation
				if protected_registers
					.contains(&Register::GeneralPurpose(idx as u64))
				{
					return false;
				}

				// Register that has been used for the last time
				last_use.get(&inner_reg)
					.map(|last| compare_block_line(pos, *last))
					.unwrap_or(true)
			}) {
		return Register::GeneralPurpose(first_unused as u64);
	}

	todo!()
}

fn collect_last_use_of_registers_in_block(
	scope: &[SimpleBlock],
) -> HashMap<Source, (usize, usize)> {
	let mut map: HashMap<Source, (usize, usize)> = HashMap::new();
	for (block_idx, block) in scope.iter().enumerate() {
		for (line_idx, line) in block.block.iter().enumerate() {
			match line {
				SimpleExpression::BinOp(SimpleBinOp { lhs, rhs, target, .. }) => {
					map.insert(*lhs, (block_idx, line_idx));
					map.insert(*rhs, (block_idx, line_idx));
					map.insert(Source::Register(*target), (block_idx, line_idx));
				}
				SimpleExpression::UnOp(SimpleUnOp { lhs, target, .. }) => {
					map.insert(*lhs, (block_idx, line_idx));
					map.insert(Source::Register(*target), (block_idx, line_idx));
				}
				SimpleExpression::FunctionCall(SimpleFunctionCall {
					args, target, ..
				}) => {
					for arg in args.iter() {
						map.insert(*arg, (block_idx, line_idx));
					}
					map.insert(Source::Register(*target), (block_idx, line_idx));
				}
			}
		}
	}
	map
}

/// Find a suitable register for `source`, potentially unloading and throwing existing values on the stack
fn get_or_load_and_get_value(
	source: &Source,
	state: &mut State,
	pos: (usize, usize),
	is_floating: bool,
	block: &mut Block,
	last_use: &HashMap<Source, (usize, usize)>,
	protected_registers: &[Register],
) -> Register {
	assert!(!is_floating);
	if let Some(idx) = state.general_purpose.index_of(source) {
		return Register::GeneralPurpose(idx as u64);
	}

	let new_register = select_register(is_floating, pos, state, last_use, protected_registers);
	// dbg!(source, &new_register);
	let (register_idx, register_set) = match new_register {
		Register::GeneralPurpose(r) => (r, &mut state.general_purpose),
		Register::FloatingPoint(r) => (r, &mut state.floating_point),
		_ => unreachable!("`select_register` should only return gp and fp registers?"),
	};

	// If there's something already there **AND** it's going to be used again **AND** is not a literal value
	// TODO: Replace with if let when if let chains are stabilised
	match register_set[register_idx as usize] {
		Some(r @ Source::Register(reg)) if compare_block_line(last_use[&r], pos) => {
			block.block.push(Expression::BinOp(BinOp {
				target: new_register,
				op: Op::StoreMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(state.stack.len() as u64),
			}));
			state.stack.push(reg);
		}
		_ => {}
	}

	let expr = match source {
		Source::Value(v) => Expression::UnOp(UnOp {
			target: new_register,
			op: Op::LoadMem,
			lhs: Register::Literal(*v),
		}),
		Source::Register(r) => {
			// Push to stack

			eprintln!("hi");
			dbg!(pos, &source, &state.stack, &register_set, &block);
			// If we're here, we've already checked current registers, meaning search the stack immediately
			let stack_position = state
				.stack
				.iter()
				.position(|x| x == r)
				.expect("Old value wasn't in registers OR on the stack?");
			Expression::BinOp(BinOp {
				target: new_register,
				op: Op::LoadMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(stack_position as u64),
			})
		}
	};
	block.block.push(expr);

	register_set[register_idx as usize] = Some(*source);
	new_register
}

fn allocate_for_block(scope: &[SimpleBlock], config: &Configuration) -> Result<Vec<Block>> {
	let mut state = State::from(config);
	let mut blocks = Vec::with_capacity(scope.len());

	let last_use_of_register = collect_last_use_of_registers_in_block(scope);

	for (block_idx, SimpleBlock { intro, block, out }) in scope.iter().enumerate() {
		assert!(
			intro.len() <= config.argument_registers as usize,
			"Cannot have phi-nodes not in registers: figure this out later"
		);
		let mut new_block = Block {
			block: SmallVec::new(),
			out: BlockEnd::Return(Register::GeneralPurpose(0)),
		};

		for (value, register) in intro.iter().zip(state.general_purpose.iter_mut()) {
			*register = Some(Source::Register(value.target));
		}

		for (line_idx, line) in block.iter().enumerate() {
			match line {
				SimpleExpression::BinOp(SimpleBinOp {
					target,
					op,
					lhs,
					rhs,
				}) if !op.is_floating_point() => {
					let lhs_register = get_or_load_and_get_value(
						lhs,
						&mut state,
						(block_idx, line_idx),
						false,
						&mut new_block,
						&last_use_of_register,
						&[],
					);
					let rhs_register = get_or_load_and_get_value(
						rhs,
						&mut state,
						(block_idx, line_idx),
						false,
						&mut new_block,
						&last_use_of_register,
						&[lhs_register],
					);
					let recommended_register = select_register(
						false,
						(block_idx, line_idx),
						&state,
						&last_use_of_register,
						&[lhs_register, rhs_register],
					);
					let expr = Expression::BinOp(BinOp {
						target: recommended_register,
						op: op.into(),
						lhs: lhs_register,
						rhs: rhs_register,
					});

					let gp_idx = recommended_register.unwrap_general_purpose()
						as usize;
					state.general_purpose[gp_idx] =
						Some(Source::Register(*target));

					new_block.block.push(expr);
				}
				SimpleExpression::UnOp(_) => todo!(),
				SimpleExpression::FunctionCall(_) => todo!(),
				_ => todo!(),
			}
		}

		// TODO: Move stuff into correct registers for the next block
		// TODO: Set jump target

		blocks.push(new_block);
	}
	Ok(blocks)
}
