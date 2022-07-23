use std::{
	collections::{HashMap, HashSet},
	fmt,
	hash::Hash,
};

use anyhow::{bail, Result};
use derive_more::{Deref, DerefMut};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	simplify::{
		Block as SimpleBlock, BlockEnd as SimpleBlockEnd, BlockId, PhiNode,
		Register as SimpleRegister, SSAConstruct, SimpleBinOp, SimpleExpression,
		SimpleFunctionCall, SimpleOp, SimpleUnOp, Source,
	},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Configuration {
	general_purpose_registers: usize,
	floating_point_registers: usize,
	argument_registers: usize,
	temporary_registers: usize,
}
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
	FloatingPoint(usize),
	GeneralPurpose(usize),
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
#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct RegisterSet(Vec<Option<Source>>);

impl RegisterSet {
	pub fn new(width: usize) -> Self {
		Self(vec![None; width])
	}

	pub fn index_of(&self, source: &Source) -> Option<usize> {
		self.0.iter().position(|x| *x == Some(*source))
	}

	pub fn reset(&mut self) {
		for reg in self.iter_mut() {
			*reg = None;
		}
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
						blocks: allocate_for_blocks(blocks, config)?,
					}
				}
				SSAConstruct::Variable { .. } => todo!(),
				SSAConstruct::ImmediateExpression { .. } => todo!(),
			};
			Ok(res)
		})
		.collect()
}

fn select_register(
	is_floating_point: bool,
	pos: (usize, usize),
	state: &State,
	last_use_of_register: &HashMap<Source, (usize, usize)>,
	protected_registers: &[Register],
) -> Register {
	assert!(!is_floating_point, "Todo: floating point");

	// Try to find an unused register
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
				inner_reg.has_been_used_for_the_last_time(pos, last_use_of_register)
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
				SimpleExpression::BinOp(SimpleBinOp {
					lhs, rhs, target, ..
				}) => {
					map.insert(*lhs, (block_idx, line_idx));
					map.insert(*rhs, (block_idx, line_idx));
					map.insert(
						Source::Register(*target),
						(block_idx, line_idx),
					);
				}
				SimpleExpression::UnOp(SimpleUnOp { lhs, target, .. }) => {
					map.insert(*lhs, (block_idx, line_idx));
					map.insert(
						Source::Register(*target),
						(block_idx, line_idx),
					);
				}
				SimpleExpression::FunctionCall(SimpleFunctionCall {
					args,
					target,
					..
				}) => {
					for arg in args.iter() {
						map.insert(*arg, (block_idx, line_idx));
					}
					map.insert(
						Source::Register(*target),
						(block_idx, line_idx),
					);
				}
			}
		}
	}
	map
}

/// Finds the first block that both paths of a split will both reach
fn find_merge(scope: &[SimpleBlock], start: BlockId) -> Option<BlockId> {
	let mut visited_left = HashSet::new();
	let mut visited_right = HashSet::new();

	let (_, lhs, rhs) = scope[usize::from(start)].out.two()?;
	if lhs == rhs {
		return Some(lhs);
	}

	let mut next_id = lhs;
	let mut next = scope.get(usize::from(next_id));

	// Search through the left branch until it ends or becomes an infinite loop
	while let Some(block) = next {
		next_id = block
			.out
			.one()
			.or_else(|| block.out.two().and_then(|_| find_merge(scope, next_id)))?;
		if visited_left.insert(next_id) {
			break;
		}
		next = scope.get(usize::from(lhs));
	}

	// Then search through the right branch until we find somewhere we've been on the left side,
	// or the right branch ends or becomes an infinite loop
	next_id = rhs;
	next = scope.get(usize::from(next_id));
	while let Some(block) = next {
		next_id = block
			.out
			.one()
			.or_else(|| block.out.two().and_then(|_| find_merge(scope, next_id)))?;
		if visited_left.contains(&next_id) {
			return Some(next_id);
		}
		if visited_right.insert(next_id) {
			break;
		}
		next = scope.get(usize::from(lhs));
	}

	None
}

/// Swaps two registers of the same type. Returns Err if lhs and rhs are not of the same register type.
/// Short circuits if they're the same, even if they're of a forbidden type
fn swap_registers(
	block: &mut SmallVec<[Expression; 4]>,
	state: &mut State,
	lhs: Register,
	rhs: Register,
) -> Result<()> {
	if lhs == rhs {
		return Ok(());
	}

	match (lhs, rhs) {
		(Register::GeneralPurpose(l), Register::GeneralPurpose(r)) => {
			state.general_purpose.swap(l as usize, r as usize)
		}
		(Register::FloatingPoint(l), Register::FloatingPoint(r)) => {
			state.floating_point.swap(l as usize, r as usize)
		}
		_ => bail!(Error::MismatchedRegisterTypes),
	}

	// https://en.wikipedia.org/wiki/XOR_swap_algorithm
	block.push(Expression::BinOp(BinOp {
		target: lhs,
		op: Op::Xor,
		lhs,
		rhs,
	}));
	block.push(Expression::BinOp(BinOp {
		target: rhs,
		op: Op::Xor,
		lhs: rhs,
		rhs: lhs,
	}));
	block.push(Expression::BinOp(BinOp {
		target: lhs,
		op: Op::Xor,
		lhs,
		rhs,
	}));

	Ok(())
}

/// Helper function to get two mutable references from the same slice.
/// Panics if left and right are the same or out of bounds.
fn get_two_references_from_slice<T>(
	slice: &mut [T],
	left: usize,
	right: usize,
) -> (&mut T, &mut T) {
	// Safety: Bounds checks are asserted.
	// This could also be done in safe code with a bunch of hacks with
	// `[T]::split_at_mut` but that is just unreadable when we don't know
	// if left is less than right

	assert_ne!(left, right);
	assert!(left < slice.len());
	assert!(right < slice.len());

	let ptr = slice.as_mut_ptr();
	unsafe { (&mut *ptr.add(left), &mut *ptr.add(right)) }
}

/// Find a suitable register for `source`, potentially unloading and throwing existing values on the stack
fn get_or_load_and_get_value(
	source: &Source,
	state: &mut State,
	pos: (usize, usize),
	is_floating: bool,
	block: &mut Block,
	last_use_of_register: &HashMap<Source, (usize, usize)>,
	protected_registers: &[Register],
) -> Register {
	assert!(!is_floating);
	if let Some(idx) = state.general_purpose.index_of(source) {
		return Register::GeneralPurpose(idx as u64);
	}

	let new_register = select_register(
		is_floating,
		pos,
		state,
		last_use_of_register,
		protected_registers,
	);
	// dbg!(source, &new_register);
	let (register_idx, register_set) = match new_register {
		Register::GeneralPurpose(r) => (r, &mut state.general_purpose),
		Register::FloatingPoint(r) => (r, &mut state.floating_point),
		_ => unreachable!("`select_register` should only return gp and fp registers?"),
	};

	// If there's something already there **AND** it's going to be used again **AND** is not a literal value
	// TODO: Replace with if let when if let chains are stabilised
	match register_set[register_idx as usize] {
		// TODO: has_been_used_for_the_last_time
		Some(r @ Source::Register(reg))
			if r.has_been_used_for_the_last_time(pos, last_use_of_register) =>
		{
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

fn allocate_for_blocks(scope: &[SimpleBlock], config: &Configuration) -> Result<Vec<Block>> {
	let last_use_of_register = collect_last_use_of_registers_in_block(scope);
	let mut state = State::from(config);
	let mut blocks = vec![None; scope.len()];

	allocate_for_blocks_with_end(
		scope,
		&mut blocks,
		config,
		&mut state,
		0.into(),
		usize::MAX.into(),
		&last_use_of_register,
	)?;

	dbg!(&blocks);
	let converted = blocks
		.into_iter()
		.collect::<Option<Vec<_>>>()
		.ok_or(Error::UnconvertedBlock)?;
	Ok(converted)
}

/// Converts blocks until it hits the end id (exclusive) or a block that returns from the function.
/// Returns the last processed block before the end id
fn allocate_for_blocks_with_end<'a>(
	scope: &[SimpleBlock],
	out: &'a mut Vec<Option<Block>>,
	config: &Configuration,
	state: &mut State,
	start: BlockId,
	end: BlockId,
	last_use_of_register: &HashMap<Source, (usize, usize)>,
) -> Result<BlockId> {
	let mut next_id = start;
	let mut old_id = start;
	let mut next = scope.get(usize::from(next_id));

	while let Some(block) = next {
		// Break on hitting a block already done by a different path
		// Break on the end of the relevant scope
		if out[usize::from(next_id)].is_some() || next_id == end {
			break;
		}

		old_id = next_id;
		let new = handle_single_block(
			block,
			next_id.into(),
			config,
			state,
			last_use_of_register,
		)?;
		// TODO: Move stuff into correct registers for the next block
		// TODO: Set jump target
		out[usize::from(old_id)] = Some(new);

		next_id = match block.out {
			SimpleBlockEnd::Return(_) => usize::MAX.into(),
			SimpleBlockEnd::One(id) => id,
			SimpleBlockEnd::Two(_, left_start, right_start) => {
				let merge_point =
					find_merge(scope, next_id).ok_or(Error::PathsDoNotMerge)?;

				let mut left_state = state.clone();
				let mut right_state = state.clone();

				let left_end = allocate_for_blocks_with_end(
					scope,
					out,
					config,
					&mut left_state,
					left_start,
					merge_point,
					last_use_of_register,
				)?;
				let right_end = allocate_for_blocks_with_end(
					scope,
					out,
					config,
					&mut right_state,
					right_start,
					merge_point,
					last_use_of_register,
				)?;

				let target_state = &scope[usize::from(merge_point)].intro;

				let (left_end_block, right_end_block) =
					{
						let (l, r) = get_two_references_from_slice(
							out,
							left_end.into(),
							right_end.into(),
						);
						(
							&mut l.as_mut().ok_or(Error::ReturnedNonExistantBlock)?.block,
							&mut r.as_mut().ok_or(Error::ReturnedNonExistantBlock)?.block,
						)
					};
				state.general_purpose.reset();

				// Merge the phi nodes by moving the right path value to
				// whatever register it's in on the left side
				for PhiNode {
					target,
					value: [left, right],
				} in target_state.iter()
				{
					assert_eq!(left.from, left_end);
					assert_eq!(right.from, right_end);

					// TODO: This can error if the value has been pushed to the stack
					let left_position = left_state
						.general_purpose
						.index_of(&left.value)
						.ok_or(Error::MissingRegister)?;
					let right_position = right_state
						.general_purpose
						.index_of(&right.value)
						.ok_or(Error::MissingRegister)?;

					state.general_purpose[left_position] =
						Some(Source::Register(*target));
					if left_position != right_position {
						// TODO: Only swap if right value actually needs preserving
						swap_registers(
							right_end_block,
							&mut right_state,
							Register::GeneralPurpose(
								left_position as u64,
							),
							Register::GeneralPurpose(
								right_position as u64,
							),
						)?;
					}
				}

				// And keep whatever values are the same still in both paths
				for (idx, (left_register, _)) in left_state
					.general_purpose
					.iter()
					.zip(right_state.general_purpose.iter())
					.enumerate()
					.filter(|(_, (a, b))| a == b)
				{
					state.general_purpose[idx] = *left_register;
				}

				merge_point
			}
		};
		next = scope.get(usize::from(next_id));
	}

	// dbg!(&out);

	Ok(old_id)
}

fn handle_single_block(
	SimpleBlock {
		intro,
		block,
		out: _,
	}: &SimpleBlock,
	block_idx: usize,
	config: &Configuration,
	state: &mut State,
	last_use_of_register: &HashMap<Source, (usize, usize)>,
) -> Result<Block> {
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
					state,
					(block_idx, line_idx),
					false,
					&mut new_block,
					last_use_of_register,
					&[],
				);
				let rhs_register = get_or_load_and_get_value(
					rhs,
					state,
					(block_idx, line_idx),
					false,
					&mut new_block,
					last_use_of_register,
					&[lhs_register],
				);
				let recommended_register = select_register(
					false,
					(block_idx, line_idx),
					state,
					last_use_of_register,
					&[lhs_register, rhs_register],
				);
				let expr = Expression::BinOp(BinOp {
					target: recommended_register,
					op: op.into(),
					lhs: lhs_register,
					rhs: rhs_register,
				});

				let gp_idx = recommended_register
					.general_purpose()
					.ok_or(Error::WrongRegisterType)? as usize;
				state.general_purpose[gp_idx] = Some(Source::Register(*target));

				new_block.block.push(expr);
			}
			SimpleExpression::UnOp(_) => todo!(),
			SimpleExpression::FunctionCall(_) => todo!(),
			_ => todo!(),
		}
	}

	Ok(new_block)
}
