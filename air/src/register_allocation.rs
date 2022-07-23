use std::{cmp::Ordering, collections::HashSet, fmt, hash::Hash};

use anyhow::{bail, Result};
use derive_more::{Deref, DerefMut};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	simplify::{
		Block as SimpleBlock, BlockEnd as SimpleBlockEnd, BlockId, PhiNode,
		Register as SimpleRegister, SSAConstruct, SimpleBinOp, SimpleExpression, SimpleOp,
		Source,
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

impl Configuration {
	pub const AARCH64: Configuration = Configuration {
		general_purpose_registers: 31,
		floating_point_registers: 32,
		argument_registers: 31,
		temporary_registers: 0,
	};
	pub const X86_64: Configuration = Configuration {
		general_purpose_registers: 14,
		floating_point_registers: 8,
		argument_registers: 6,
		temporary_registers: 9,
	};

	pub fn new(
		general_purpose_registers: usize,
		floating_point_registers: usize,
		argument_registers: usize,
		temporary_registers: usize,
	) -> Self {
		assert!(general_purpose_registers > 0);
		// assert!(floating_point_registers > 0);
		Self {
			general_purpose_registers,
			floating_point_registers,
			argument_registers,
			temporary_registers,
		}
	}
}

impl Default for Configuration {
	fn default() -> Self {
		#[cfg(target_arch = "x86_64")]
		return Configuration::X86_64;
		#[cfg(target_arch = "aarch64")]
		return Configuration::AARCH64;
		#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
		return Configuration {
			general_purpose_registers: 8,
			floating_point_registers: 8,
			argument_registers: 0,
			temporary_registers: 0,
		};
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
	UDiv,
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
			Op::UDiv => write!(f, "u÷"),
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
			SimpleOp::UDiv => Op::UDiv,
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
	pub scope: SmallString,
	pub args: SmallVec<[Register; 4]>,
}

#[derive(Clone, PartialEq)]
pub enum Expression {
	BinOp(BinOp),
	UnOp(UnOp),
	FunctionCall(FunctionCall),
}

/// `Vec<Option<Source>>`
#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
#[deref(forward)]
#[repr(transparent)]
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
	stack: Vec<Option<Source>>,
	max_stack_size: usize,
}

impl State {
	/// Checks if a Register is currently in the register banks or on the stack
	pub fn contains(&self, source: SimpleRegister) -> bool {
		let wrapped = Some(Source::Register(source));
		self.general_purpose.contains(&wrapped)
			|| self.floating_point.contains(&wrapped)
			|| self.stack.contains(&wrapped)
	}

	/// Finds the location of a source in either of the register banks but **NOT** on the stack
	pub fn find(&self, source: Source) -> Option<Register> {
		let wrapped = Some(source);
		self.general_purpose
			.iter()
			.position(|r| r == &wrapped)
			.map(Register::GeneralPurpose)
			.or_else(|| {
				self.floating_point
					.iter()
					.position(|r| r == &wrapped)
					.map(Register::FloatingPoint)
			})
	}
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
				scope,
				args,
			}) => write!(f, "{target} ← {scope}{args:?}"),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Variantly)]
pub enum BlockEnd {
	#[variantly(rename = "ret")]
	Return,
	One(BlockId),
	Two(Register, BlockId, BlockId),
}

impl fmt::Debug for BlockEnd {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Return => write!(f, "ret"),
			Self::One(target) => write!(f, "{target}"),
			Self::Two(reg, left, right) => write!(f, "{reg} ? {left}, {right}"),
		}
	}
}

impl Default for BlockEnd {
	fn default() -> Self {
		BlockEnd::Return
	}
}

pub fn register_allocate(
	ssa: &[SSAConstruct],
	config: &Configuration,
) -> Result<Vec<CodeConstruct>> {
	ssa.iter()
		.map(|construct| {
			let res = match construct {
				SSAConstruct::Function { name, blocks, args } => {
					CodeConstruct::Function {
						name: name.clone(),
						blocks: allocate_for_blocks(blocks, config, *args)?,
					}
				}
				SSAConstruct::Variable { .. } => todo!(),
				SSAConstruct::ImmediateExpression { .. } => todo!(),
			};
			Ok(res)
		})
		.collect()
}

// TODO: Prioritise going into a φ-node immediately
fn find_empty_slot(
	pos: (usize, usize),
	registers: &[Option<Source>],
	scope: &[SimpleBlock],
	protected_registers: &[Source],
) -> Option<usize> {
	registers.iter().position(|&reg| {
		// Completely unused register
		let inner_reg = if let Some(r) = reg {
			r
		} else {
			return true;
		};

		// Don't select anything we need in the same operation
		if protected_registers.contains(&inner_reg) {
			return false;
		}

		// Register that has been used for the last time
		inner_reg.lines_till_last_use(scope, pos).is_none()
	})
}
fn select_register(
	is_floating_point: bool,
	pos: (usize, usize),
	state: &mut State,
	scope: &[SimpleBlock],
	protected_registers: &[Source],
) -> (Register, bool) {
	assert!(!is_floating_point, "Todo: floating point");

	// Try to find an unused register
	if let Some(first_unused) =
		find_empty_slot(pos, &state.general_purpose, scope, protected_registers)
	{
		return (Register::GeneralPurpose(first_unused), false);
	}

	// Select which register to push to the stack instead
	let (slot, store_old_value) = state
		.general_purpose
		.iter()
		.enumerate()
		.filter(|(_, r)| !r.map(|s| protected_registers.contains(&s)).unwrap_or(false))
		.map(|(idx, register)| {
			let reg = register.expect("Any empty register slots should've been found in the above attempt")
				.lines_till_last_use(scope, pos);
			(idx, reg)
		})
		.min_by(|&lhs, &rhs| match (lhs.1, rhs.1) {
			(None, _) => Ordering::Less,
			(_, None) => Ordering::Greater,
			(Some(x), Some(y)) => x.cmp(&y),
		})
		.map(|(x, y)| (x, y.is_some()))
		.expect("There cannot be 0 registers");

	(Register::GeneralPurpose(slot), store_old_value)
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

fn save_on_stack(
	register_set: &mut RegisterSet,
	pos: (usize, usize),
	idx: usize,
	block: &mut Block,
	scope: &[SimpleBlock],
	new_register: Register,
	stack: &mut Vec<Option<Source>>,
) {
	// TODO: Replace with if let when if let chains are stabilised
	if let Some(Source::Register(reg)) = register_set[idx] {
		block.block.push(Expression::BinOp(BinOp {
			target: new_register,
			op: Op::StoreMem,
			lhs: Register::StackPointer,
			rhs: Register::Literal(stack.len() as u64),
		}));

		let stack_value = Some(Source::Register(reg));
		if let Some(empty_idx) = find_empty_slot(pos, stack, scope, &[]) {
			stack[empty_idx] = stack_value;
		} else {
			stack.push(stack_value);
		}
	}
}

/// Find a suitable register for `source`, potentially unloading and throwing existing values on the stack
fn get_or_load_and_get_value(
	source: &Source,
	state: &mut State,
	pos: (usize, usize),
	is_floating: bool,
	block: &mut Block,
	scope: &[SimpleBlock],
	protected_registers: &[Source],
) -> Register {
	assert!(!is_floating);
	if let Some(idx) = state.general_purpose.index_of(source) {
		return Register::GeneralPurpose(idx);
	}

	let (new_register, needs_to_be_saved) =
		select_register(is_floating, pos, state, scope, protected_registers);

	let (register_idx, register_set) = match new_register {
		Register::GeneralPurpose(r) => (r, &mut state.general_purpose),
		Register::FloatingPoint(r) => (r, &mut state.floating_point),
		_ => unreachable!("`select_register` should only return gp and fp registers?"),
	};

	if needs_to_be_saved {
		save_on_stack(
			register_set,
			pos,
			register_idx as usize,
			block,
			scope,
			new_register,
			&mut state.stack,
		);
	}

	let expr = match source {
		Source::Value(v) => Expression::UnOp(UnOp {
			target: new_register,
			op: Op::LoadMem,
			lhs: Register::Literal(*v),
		}),
		Source::Register(r) => {
			// Push to stack

			eprintln!("--------------------------------------------------------");
			// If we're here, we've already checked current registers, meaning search the stack immediately
			let stack_position = state
				.stack
				.iter()
				.position(|x| x == &Some(Source::Register(*r)))
				.expect("Old value wasn't in registers OR on the stack?");
			state.stack[stack_position] = None;
			Expression::BinOp(BinOp {
				target: new_register,
				op: Op::LoadMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(stack_position as u64),
			})
		}
	};
	block.block.push(expr);

	eprintln!(
		"[{}]: Replacing {:?} with {:?} (saving: {needs_to_be_saved})",
		line!(),
		register_set[register_idx as usize],
		*source
	);

	register_set[register_idx as usize] = Some(*source);
	new_register
}

fn allocate_for_blocks(
	scope: &[SimpleBlock],
	config: &Configuration,
	args: usize,
) -> Result<Vec<Block>> {
	let mut state = State::from(config);
	let mut blocks = vec![None; scope.len()];

	// Copy in the arguments into registers
	for (register, arg) in state
		.general_purpose
		.iter_mut()
		.zip((0..args).map(|x| Some(Source::Register(x.into()))))
		.take(config.argument_registers as usize)
	{
		*register = arg;
	}
	// And copy those that don't fit into registers onto the stack
	for arg in (config.argument_registers as usize)..args {
		state.stack.push(Some(Source::Register(arg.into())));
	}

	eprintln!(
		"[{}]: Start\nRegs: {:#?}\nStack: {:#?}\n",
		line!(),
		&state.general_purpose,
		&state.stack
	);

	allocate_for_blocks_with_end(
		scope,
		&mut blocks,
		config,
		&mut state,
		0.into(),
		usize::MAX.into(),
	)?;

	let converted = blocks
		.into_iter()
		.collect::<Option<Vec<_>>>()
		.ok_or(Error::UnconvertedBlock)?;
	Ok(converted)
}

/// Converts blocks until it hits the end id (exclusive) or a block that returns from the scope.
/// Returns the last processed block before the end id
fn allocate_for_blocks_with_end(
	scope: &[SimpleBlock],
	out: &mut Vec<Option<Block>>,
	config: &Configuration,
	state: &mut State,
	start: BlockId,
	end: BlockId,
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
		let new = handle_single_block(block, next_id.into(), config, state, scope)?;
		out[usize::from(old_id)] = Some(new);

		next_id = match block.out {
			SimpleBlockEnd::Return(_) => usize::MAX.into(),
			SimpleBlockEnd::One(id) => {
				// TODO: Take care of φ-nodes if the next block is already generated
				id
			}
			SimpleBlockEnd::Two(_, left_start, right_start) => {
				let merge_point =
					find_merge(scope, next_id).ok_or(Error::PathsDoNotMerge)?;

				let mut left_state = state.clone();
				let mut right_state = state.clone();

				log::info!("-- BRANCHING --");
				let left_end = allocate_for_blocks_with_end(
					scope,
					out,
					config,
					&mut left_state,
					left_start,
					merge_point,
				)?;
				log::info!("-- RETURNING TO BRANCH --");
				let right_end = allocate_for_blocks_with_end(
					scope,
					out,
					config,
					&mut right_state,
					right_start,
					merge_point,
				)?;
				log::info!("-- CONTINUING AFTER MERGE -- ");
				dbg!(&left_state.general_purpose, &right_state.general_purpose);

				let target_state = &scope[usize::from(merge_point)].intro;

				let (left_end_block, right_end_block) =
					{
						let (l, r) = get_two_references_from_slice(
							out,
							left_end.into(),
							right_end.into(),
						);
						// If these ever fail there needs to be an extra block added at the end of
						// that block where handling of phi-node stuff can happen
						assert!(matches!(
							l,
							Some(Block {
								out: BlockEnd::One(_),
								..
							})
						));
						assert!(matches!(
							r,
							Some(Block {
								out: BlockEnd::One(_),
								..
							})
						));
						(
							&mut l.as_mut().ok_or(Error::ReturnedNonExistantBlock)?.block,
							&mut r.as_mut().ok_or(Error::ReturnedNonExistantBlock)?.block,
						)
					};

				// Clear old information
				state.general_purpose.iter_mut().for_each(|x| *x = None);
				state.floating_point.iter_mut().for_each(|x| *x = None);
				state.stack.iter_mut().for_each(|x| *x = None);

				// Fill out stack to the longest stack for mergability
				let max_stack = left_state.stack.len().max(right_state.stack.len());
				for _ in state.stack.len()..max_stack {
					state.stack.push(None);
				}

				eprintln!("-----------------------------------------------------");
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
					// TODO: This can error if a branch returned early due to it being
					//       generated by a different path (should be caught by assert)
					let left_position = left_state
						.general_purpose
						.index_of(&left.value)
						.ok_or(Error::MissingRegister)
						.unwrap();
					let right_position = right_state
						.general_purpose
						.index_of(&right.value)
						.ok_or(Error::MissingRegister)
						.unwrap();

					state.general_purpose[left_position] =
						Some(Source::Register(*target));
					if left_position != right_position {
						// TODO: Only swap if right value actually needs preserving
						swap_registers(
							right_end_block,
							&mut right_state,
							Register::GeneralPurpose(left_position),
							Register::GeneralPurpose(right_position),
						)?;
					}
				}

				let merge_remaining =
					|left: &[Option<Source>],
					 right: &[Option<Source>],
					 state: &mut [Option<Source>]| {
						for (&left_reg, merge_reg) in left
							.iter()
							.zip(right.iter())
							.zip(state.iter_mut())
							.filter(|((a, b), _)| a == b)
							.map(|((a, _), b)| (a, b))
						{
							*merge_reg = left_reg;
						}

						// TODO: There might be values that end up on the stack in
						// both the left and right branches but end up on the
						// stack in different positions.
						// This will crash early in those cases
						assert!(
							left.iter().enumerate().all(
								|(idx, val)| val.is_none()
									|| right.iter()
										.position(|r_val| {
											r_val == val
										})
										.map(|r_idx| idx
											== r_idx)
										.unwrap_or(true)
							),
							"\n{left:?}\n{right:?}"
						);
					};
				// And keep whatever values are the same still in both paths
				merge_remaining(
					&left_state.general_purpose,
					&right_state.general_purpose,
					&mut state.general_purpose,
				);
				merge_remaining(
					&left_state.floating_point,
					&right_state.floating_point,
					&mut state.floating_point,
				);
				merge_remaining(
					&left_state.stack,
					&right_state.stack,
					&mut state.stack,
				);

				merge_point
			}
		};
		next = scope.get(usize::from(next_id));
	}

	Ok(old_id)
}

fn handle_single_block(
	SimpleBlock { intro, block, out }: &SimpleBlock,
	block_idx: usize,
	config: &Configuration,
	state: &mut State,
	scope: &[SimpleBlock],
) -> Result<Block> {
	assert!(
		intro.len() <= config.argument_registers as usize,
		"Cannot have phi-nodes not in registers: figure this out later"
	);
	let mut new_block = Block {
		block: SmallVec::new(),
		out: BlockEnd::Return,
	};

	assert!(intro.iter().all(|reg| state.contains(reg.target)));

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
					scope,
					&[*lhs, *rhs],
				);
				let rhs_register = get_or_load_and_get_value(
					rhs,
					state,
					(block_idx, line_idx),
					false,
					&mut new_block,
					scope,
					&[*lhs, *rhs],
				);
				let (recommended_register, need_to_save_old_value) =
					select_register(
						false,
						(block_idx, line_idx),
						state,
						scope,
						&[],
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

				if need_to_save_old_value {
					save_on_stack(
						&mut state.general_purpose,
						(block_idx, line_idx),
						gp_idx,
						&mut new_block,
						scope,
						recommended_register,
						&mut state.stack,
					);
				}

				eprintln!(
					"[{}]: Replacing {:?} (at: {gp_idx}) with {:?} (saving: {need_to_save_old_value})",
					line!(),
					state.general_purpose[gp_idx],
					Source::Register(*target),
				);
				state.general_purpose[gp_idx] = Some(Source::Register(*target));

				new_block.block.push(expr);
			}
			SimpleExpression::UnOp(_) => todo!(),
			SimpleExpression::FunctionCall(_) => todo!(),
			_ => todo!(),
		}
		eprintln!(
			"[{}]: {line:?}\nRegs: {:#?}\nStack: {:#?}\n",
			line!(),
			&state.general_purpose,
			&state.stack
		);
	}
	let new_out = match *out {
		SimpleBlockEnd::Return(_) => BlockEnd::Return,
		SimpleBlockEnd::One(o) => BlockEnd::One(o),
		SimpleBlockEnd::Two(target, l, r) => {
			dbg!(target, &state);
			let condition = state.find(target).unwrap_or_else(|| {
				// TODO: Load value from stack
				todo!();
			});
			BlockEnd::Two(condition, l, r)
		}
	};
	new_block.out = new_out;

	Ok(new_block)
}
