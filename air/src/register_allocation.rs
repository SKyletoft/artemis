use std::{cmp::Ordering, collections::HashSet, fmt, hash::Hash};

use anyhow::Result;
use derive_more::{Deref, DerefMut};
use once_cell::sync::Lazy;
// use rayon::prelude::*;
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	ir::{
		Block as SimpleBlock, BlockEnd as SimpleBlockEnd, BlockId, PhiNode,
		Register as SimpleRegister, SSAConstruct, SimpleBinOp, SimpleExpression,
		SimpleFunctionCall, SimpleOp, Source,
	},
	simplify,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

// Invariant with codegen: Argument registers are the first registers
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Configuration {
	general_purpose_registers: u64,
	argument_registers: u64,
	unprotected_registers: Vec<Register>,
	return_register: u64,
}

impl Configuration {
	pub fn new(
		general_purpose_registers: u64,
		argument_registers: u64,
		protected_registers: Vec<Register>,
		return_register: u64,
	) -> Self {
		assert!(general_purpose_registers > 0);
		assert!(general_purpose_registers >= argument_registers);
		assert!(general_purpose_registers > return_register);
		Self {
			general_purpose_registers,
			argument_registers,
			unprotected_registers: protected_registers,
			return_register,
		}
	}
}

pub static AARCH64: Lazy<Configuration> = Lazy::new(|| Configuration {
	general_purpose_registers: 31,
	argument_registers: 31,
	unprotected_registers: vec![],
	return_register: u64::MAX, // TODO
});

pub static X86_64: Lazy<Configuration> = Lazy::new(|| Configuration {
	general_purpose_registers: 13,
	argument_registers: 6,
	unprotected_registers: (0..=7).map(Register::GeneralPurpose).collect(),
	return_register: 6, // RAX
});

impl Default for Configuration {
	fn default() -> Self {
		#[cfg(target_arch = "x86_64")]
		return X86_64.clone();
		#[cfg(target_arch = "aarch64")]
		return AARCH64.clone();
		#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
		return Configuration {
			general_purpose_registers: 8,
			argument_registers: 0,
			temporary_registers: 0,
		};
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodeConstruct {
	Function {
		name: SmallString,
		blocks: Vec<Block>,
		arguments_on_stack: u64,
		frame_size: u64,
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
	GeneralPurpose(usize),
	StackPointer,
	FramePointer,
	ProgramCounter,
}

impl fmt::Display for Register {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Register::Literal(v) => write!(f, "{}", *v as i64),
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
	Swap,
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
			Op::Swap => write!(f, "⇄"),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	pub block: SmallVec<[Expression; 4]>,
	pub out: BlockEnd,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BinOp {
	pub target: Register,
	pub op: Op,
	pub lhs: Register,
	pub rhs: Register,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnOp {
	pub target: Register,
	pub op: Op,
	pub lhs: Register,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
	pub function_name: SmallString,
	pub args: usize,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Expression {
	BinOp(BinOp),
	UnOp(UnOp),
	FunctionCall(FunctionCall),
}

/// `Vec<Option<Source>>`
#[derive(Debug, Clone, PartialEq, Eq, Deref, DerefMut)]
#[deref(forward)]
#[repr(transparent)]
pub struct RegisterSet(Vec<Option<Source>>);

impl RegisterSet {
	pub fn new(width: u64) -> Self {
		Self(vec![None; width as usize])
	}

	pub fn index_of(&self, source: &Source) -> Option<usize> {
		self.0.iter()
			.position(|x| matches!(x, Some(y) if y == source))
	}

	pub fn reset(&mut self) {
		for reg in self.iter_mut() {
			*reg = None;
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
struct State {
	registers: RegisterSet,
	stack: Vec<Option<Source>>,
}

impl State {
	/// Checks if a Register is currently in the register banks or on the stack
	pub fn contains(&self, source: SimpleRegister) -> bool {
		let wrapped = Some(Source::Register(source));
		self.registers.contains(&wrapped) || self.stack.contains(&wrapped)
	}

	/// Finds the location of a source in either of the register banks but **NOT** on the stack
	pub fn find(&self, source: Source) -> Option<Register> {
		let wrapped = Some(source);
		self.registers
			.iter()
			.position(|r| r == &wrapped)
			.map(Register::GeneralPurpose)
	}
}

impl From<&Configuration> for State {
	fn from(config: &Configuration) -> Self {
		Self {
			registers: RegisterSet::new(config.general_purpose_registers),
			stack: Vec::new(),
		}
	}
}

impl fmt::Debug for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::BinOp(BinOp {
				target,
				op: Op::StoreMem,
				lhs,
				rhs,
			}) => write!(f, "{target} → {lhs} + {rhs}"),
			Expression::BinOp(BinOp {
				target,
				op,
				lhs,
				rhs,
			}) => write!(f, "{target} ← {lhs} {op} {rhs}"),
			Expression::UnOp(UnOp {
				target,
				op: Op::StoreMem,
				lhs,
			}) => {
				write!(f, "{target} → {lhs}")
			}
			Expression::UnOp(UnOp {
				target,
				op: Op::Swap,
				lhs,
			}) => {
				write!(f, "{target} {} {lhs}", Op::Swap)
			}
			Expression::UnOp(UnOp { target, op, lhs }) => {
				write!(f, "{target} ← {op} {lhs}")
			}
			Expression::FunctionCall(FunctionCall {
				function_name,
				args,
			}) => write!(f, "RET_REG ← {function_name}[{args}]"),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Variantly)]
pub enum BlockEnd {
	#[variantly(rename = "ret")]
	Return(Register),
	One(BlockId),
	Two(Register, BlockId, BlockId),
}

impl fmt::Debug for BlockEnd {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Return(target) => write!(f, "ret {target}"),
			Self::One(target) => write!(f, "{target}"),
			Self::Two(reg, left, right) => write!(f, "{reg} ? {left}, {right}"),
		}
	}
}

pub fn register_allocate(
	ssa: &[SSAConstruct],
	config: &Configuration,
) -> Result<Vec<CodeConstruct>> {
	ssa.iter()
		// .par_bridge()
		.map(|construct| {
			let res = match construct {
				SSAConstruct::Function { name, blocks, args } => {
					let (blocks, frame_size) =
						allocate_for_blocks(blocks, config, *args)?;
					CodeConstruct::Function {
						name: name.clone(),
						blocks,
						frame_size: frame_size as u64,
						arguments_on_stack: args
							.saturating_sub(config.argument_registers),
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
	registers.iter().position(|reg| {
		// Completely unused register
		let inner_reg = if let Some(r) = reg {
			r.clone()
		} else {
			return true;
		};

		// Don't select anything we need in the same operation
		if protected_registers.contains(&inner_reg) {
			return false;
		}

		// Register that has been used for the last time
		simplify::lines_till_last_use(&inner_reg, scope, pos).is_none()
	})
}

fn select_register(
	pos: (usize, usize),
	state: &mut State,
	scope: &[SimpleBlock],
	protected_registers: &[Source],
) -> (Register, bool) {
	// Try to find an unused register
	if let Some(first_unused) =
		find_empty_slot(pos, &state.registers, scope, protected_registers)
	{
		return (Register::GeneralPurpose(first_unused), false);
	}

	// Select which register to push to the stack instead
	let (slot, store_old_value) = state
		.registers
		.iter()
		.enumerate()
		.filter(|(_, r)| {
			if let Some(rr) = r {
				protected_registers.contains(rr)
			} else {
				false
			}
		})
		.map(|(idx, register)| {
			let unwrapped_register = register.clone()
				.expect("Any empty register slots should've been found in the above attempt");
			let reg = simplify::lines_till_last_use(&unwrapped_register, scope, pos);
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

	let (_, lhs, rhs) = scope[usize::from(start)].clone().out.two()?;
	if lhs == rhs {
		return Some(lhs);
	}

	let mut next_id = lhs;
	let mut next = scope.get(usize::from(next_id));

	// Search through the left branch until it ends or becomes an infinite loop
	while let Some(block) = next {
		next_id = block.out.clone().one().or_else(|| {
			block.out
				.clone()
				.two()
				.and_then(|_| find_merge(scope, next_id))
		})?;
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
		next_id = block.out.clone().one().or_else(|| {
			block.out
				.clone()
				.two()
				.and_then(|_| find_merge(scope, next_id))
		})?;
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

// new_register is the same as idx, just wrapped in GeneralPurpose or FloatingPoint depending on the RegisterSet
fn save_on_stack(
	register_set: &mut RegisterSet,
	pos: (usize, usize),
	idx: usize,
	block: &mut Block,
	scope: &[SimpleBlock],
	new_register: Register,
	stack: &mut Vec<Option<Source>>,
) {
	if let Some(Source::Register(reg)) = register_set[idx] {
		let stack_value = Some(Source::Register(reg));
		// Don't store the value it it's already on the stack
		if stack.iter().any(|v| v == &stack_value) {
			return;
		}

		// TODO: If there's a free slot on the stack, pre-add and use an unop store
		// TODO: Replace with push

		if let Some(empty_idx) = find_empty_slot(pos, stack, scope, &[]) {
			block.block.push(Expression::BinOp(BinOp {
				target: new_register,
				op: Op::StoreMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(empty_idx as u64),
			}));
			stack[empty_idx] = stack_value;
		} else {
			block.block.push(Expression::BinOp(BinOp {
				target: new_register,
				op: Op::StoreMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(stack.len() as u64),
			}));
			stack.push(stack_value);
		}
	}
}

/// Find a suitable register for `source`, potentially unloading and throwing existing values on the stack
fn get_or_load_and_get_value(
	source: Source,
	state: &mut State,
	pos: (usize, usize),
	block: &mut Block,
	scope: &[SimpleBlock],
	protected_registers: &[Source],
) -> Register {
	if let Some(idx) = state.registers.index_of(&source) {
		return Register::GeneralPurpose(idx);
	}

	let (new_register, needs_to_be_saved) =
		select_register(pos, state, scope, protected_registers);

	let (register_idx, register_set) = match new_register {
		Register::GeneralPurpose(r) => (r, &mut state.registers),
		_ => unreachable!("`select_register` should only return gp registers?"),
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

	load_value(
		source.clone(),
		new_register,
		&mut state.stack,
		&mut block.block,
	);

	log::trace!(
		"[{}]: Replacing {:?} with {:?} (saving: {needs_to_be_saved})",
		line!(),
		register_set[register_idx as usize],
		&source
	);

	register_set[register_idx as usize] = Some(source);
	new_register
}

// Load a value into a specific register. Either swapping with another register if it's already loaded or loading it from the stack
fn switch_or_load_value(
	target_position: usize,
	value: Source,
	bank: &mut RegisterSet,
	block: &mut SmallVec<[Expression; 4]>,
	stack: &mut [Option<Source>],
) {
	// Check if there's nothing to do
	if bank[target_position] == Some(value.clone()) {
		return;
	}

	let current_position = bank.index_of(&value);
	if let Some(idx) = current_position {
		let swap = Expression::UnOp(UnOp {
			target: Register::GeneralPurpose(target_position),
			op: Op::Swap,
			lhs: Register::GeneralPurpose(idx),
		});
		block.push(swap);
	} else {
		load_value(
			value.clone(),
			Register::GeneralPurpose(target_position),
			stack,
			block,
		);
	}
	bank[target_position] = Some(value);
}

/// Load a value to a register from the stack (or if it's a constant). Saving the old value should
/// already be handled, and updating state should be done immediately after this
fn load_value(
	value: Source,
	register: Register,
	stack: &mut [Option<Source>],
	block: &mut SmallVec<[Expression; 4]>,
) {
	let expr = match value {
		Source::Value(v) => Expression::UnOp(UnOp {
			target: register,
			op: Op::LoadMem,
			lhs: Register::Literal(v),
		}),
		Source::Register(r) => {
			let stack_position = stack
				.iter()
				.position(|x| x == &Some(Source::Register(r)))
				.expect("Old value wasn't in registers OR on the stack?");
			// stack[stack_position] = None;
			Expression::BinOp(BinOp {
				target: register,
				op: Op::LoadMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(stack_position as u64),
			})
		}
		Source::LinkerValue(_) => todo!("Linker values in reg alloc"),
	};
	block.push(expr);
}

/// Find a suitable register to store results in. Will save whatever was already there on the stack if it's needed again
fn select_and_save_old(
	pos: (usize, usize),
	state: &mut State,
	scope: &[SimpleBlock],
	block: &mut Block,
) -> Result<Register> {
	let (recommended_register, need_to_save_old_value) =
		select_register(pos, state, scope, &[]);

	let gp_idx = recommended_register
		.general_purpose()
		.ok_or(Error::WrongRegisterType(line!()))? as usize;

	if need_to_save_old_value {
		save_on_stack(
			&mut state.registers,
			pos,
			gp_idx,
			block,
			scope,
			recommended_register,
			&mut state.stack,
		);
	}

	Ok(recommended_register)
}

fn allocate_for_blocks(
	scope: &[SimpleBlock],
	config: &Configuration,
	args: u64,
) -> Result<(Vec<Block>, usize)> {
	let mut state = State::from(config);
	let mut blocks = vec![None; scope.len()];

	// Copy in the arguments into registers
	for (register, arg) in state
		.registers
		.iter_mut()
		.zip((0..args).map(|x| Some(Source::Register(x.into()))))
		.take(config.argument_registers as usize)
	{
		*register = arg;
	}
	// And copy those that don't fit into registers onto the stack
	for arg in config.argument_registers..args {
		state.stack.push(Some(Source::Register(arg.into())));
	}

	log::trace!(
		"[{}]: Start\nRegs: {:#?}\nStack: {:#?}\n",
		line!(),
		&state.registers,
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
		.ok_or(Error::UnconvertedBlock(line!()))?;
	Ok((converted, state.stack.len()))
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
				assert!(out[usize::from(id)].is_none());
				id
			}
			SimpleBlockEnd::Two(_, left_start, right_start) => {
				let merge_point = find_merge(scope, next_id)
					.ok_or(Error::PathsDoNotMerge(line!()))?;

				let mut left_state = state.clone();
				let mut right_state = state.clone();

				log::trace!("-- BRANCHING --");
				let left_end = allocate_for_blocks_with_end(
					scope,
					out,
					config,
					&mut left_state,
					left_start,
					merge_point,
				)?;
				log::trace!("-- RETURNING TO BRANCH --");
				let right_end = allocate_for_blocks_with_end(
					scope,
					out,
					config,
					&mut right_state,
					right_start,
					merge_point,
				)?;
				log::trace!("-- CONTINUING AFTER MERGE -- ");

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
							&mut l.as_mut().ok_or(Error::ReturnedNonExistantBlock(line!()))?.block,
							&mut r.as_mut().ok_or(Error::ReturnedNonExistantBlock(line!()))?.block,
						)
					};

				// Clear old tracermation
				state.registers.iter_mut().for_each(|x| *x = None);
				state.stack.iter_mut().for_each(|x| *x = None);

				// Fill out stack to the longest stack for mergability
				let max_stack = left_state.stack.len().max(right_state.stack.len());
				for _ in state.stack.len()..max_stack {
					state.stack.push(None);
				}

				log::trace!(
					"After merge φ:\n{:#?}\n{:#?}\n{:#?}",
					&left_state.registers,
					&right_state.registers,
					&state.registers
				);

				merge_old_registers(
					&mut left_state,
					&mut right_state,
					state,
					right_end_block,
				);

				// Merge the phi nodes by moving the right path value to
				// whatever register it's in on the left side
				for PhiNode {
					target,
					value: [left, right],
				} in target_state.iter()
				{
					assert_eq!(left.from, left_end);
					assert_eq!(right.from, right_end);

					let resulting_register = merge_registers(
						&mut left_state,
						&mut right_state,
						state,
						left.value.clone(),
						right.value.clone(),
						right_end_block,
						left_end_block,
					)?;
					log::trace!(
						"Merging {} and {} to {}",
						left.from,
						right.from,
						resulting_register
					);
					state.registers[resulting_register] =
						Some(Source::Register(*target));
				}

				log::trace!(
					"After merge all:\n{:#?}\n{:#?}\n{:#?}",
					&left_state,
					&right_state,
					&state,
				);

				merge_point
			}
		};
		next = scope.get(usize::from(next_id));
	}

	Ok(old_id)
}

fn merge_old_registers(
	left_state: &mut State,
	right_state: &mut State,
	state: &mut State,
	right_end_block: &mut SmallVec<[Expression; 4]>,
) {
	// And keep whatever values are the same still in both paths
	merge_untouched(
		&left_state.registers,
		&right_state.registers,
		&mut state.registers,
	);
	merge_untouched(&left_state.stack, &right_state.stack, &mut state.stack);

	// And then move stuff that's been moved
	merge_moved(
		&left_state.registers,
		&mut right_state.registers,
		&mut state.registers,
		right_end_block,
	);
	// TODO: Merge moved on the stack

	check_merges(
		&left_state.registers,
		&right_state.registers,
		&state.registers,
	);
	check_merges(&left_state.stack, &right_state.stack, &state.stack);
}

fn check_merges(left: &[Option<Source>], right: &[Option<Source>], state: &[Option<Source>]) {
	let is_none_or_in_same_place_as_right = |(idx, val): (usize, &Option<Source>)| -> bool {
		val.is_none()
			|| right.iter()
				.position(|r| r == val)
				.map(|r| r == idx)
				.unwrap_or(true)
	};
	let is_none_or_in_same_place_as_left = |(idx, val): (usize, &Option<Source>)| -> bool {
		val.is_none()
			|| left.iter()
				.position(|l| l == val)
				.map(|l| l == idx)
				.unwrap_or(true)
	};
	assert_eq!(left.len(), right.len());
	assert!(
		left.iter()
			.enumerate()
			.all(is_none_or_in_same_place_as_right),
		"\n{left:#?}\n{right:#?}"
	);
	assert!(
		right.iter()
			.enumerate()
			.all(is_none_or_in_same_place_as_left),
		"\n{left:#?}\n{right:#?}"
	);

	assert!(state
		.iter()
		.zip(left.iter())
		.zip(right.iter())
		.filter(|((s, _), _)| s.is_some())
		.all(|((s, l), r)| s == l && s == r));
}

fn merge_untouched(
	left: &[Option<Source>],
	right: &[Option<Source>],
	state: &mut [Option<Source>],
) {
	for (left_reg, merge_reg) in left
		.iter()
		.zip(right.iter())
		.zip(state.iter_mut())
		.filter(|((a, b), _)| a == b)
		.map(|((a, _), b)| (a, b))
	{
		*merge_reg = left_reg.clone();
	}
}

fn merge_moved(
	left: &[Option<Source>],
	right: &mut [Option<Source>],
	state: &mut [Option<Source>],
	block: &mut SmallVec<[Expression; 4]>,
) {
	for (idx, val) in left.iter().enumerate().filter(|(_, val)| val.is_some()) {
		let right_position = right.iter().position(|x| x == val);
		// Skip those that are already in the same place or don't exist on the right side
		if right_position.map(|x| x == idx).unwrap_or(true) {
			continue;
		}
		let right_idx = right_position.unwrap();

		// If swaps are horribly more expensive than simple loads, this can be optionally a load
		block.push(Expression::UnOp(UnOp {
			target: Register::GeneralPurpose(idx),
			op: Op::Swap,
			lhs: Register::GeneralPurpose(right_idx),
		}));
		right.swap(idx, right_idx);
		assert!(state[idx].is_none());
		state[idx] = val.clone()
	}
}

fn load_value_to_branch(
	block: &mut SmallVec<[Expression; 4]>,
	state: &mut State,
	value: Source,
	idx: usize,
) -> Result<()> {
	// Save the value we're overwriting
	// TODO: switch this to `save_on_stack` to have a chance of reusing old stack values
	let cmp_val = Some(value.clone());
	if state.registers[idx].is_some() && !state.stack.iter().any(|v| v == &cmp_val) {
		// TODO: If there's a free slot on the stack, pre-add and use an unop store
		let reg_idx = state.registers[idx].clone();
		if let Some(stack_position) = state.stack.iter().position(Option::is_none) {
			block.push(Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(idx),
				op: Op::StoreMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(stack_position as u64),
			}));
			state.stack[stack_position] = reg_idx;
		} else {
			block.push(Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(idx),
				op: Op::StoreMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(state.stack.len() as u64),
			}));
			state.stack.push(reg_idx);
		}
	}
	//load_value(value, Register::GeneralPurpose(idx), &mut state.stack, block);
	// TODO: Remove below
	match &value {
		Source::Value(v) => {
			block.push(Expression::UnOp(UnOp {
				target: Register::GeneralPurpose(idx),
				op: Op::LoadMem,
				lhs: Register::Literal(*v),
			}));
		}
		Source::Register(_) => {
			let stack_position = state
				.stack
				.iter()
				.position(|r| r == &cmp_val)
				.ok_or(Error::MissingRegister(line!()))?;
			block.push(Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(idx),
				op: Op::LoadMem,
				lhs: Register::StackPointer,
				rhs: Register::Literal(stack_position as u64),
			}));
		}
		Source::LinkerValue(_) => todo!("Reg alloc linker value"),
	}
	state.registers[idx] = Some(value);
	Ok(())
}

fn merge_registers(
	left_state: &mut State,
	right_state: &mut State,
	state: &mut State,
	left_from: Source,
	right_from: Source,
	right_end_block: &mut SmallVec<[Expression; 4]>,
	left_end_block: &mut SmallVec<[Expression; 4]>,
) -> Result<usize> {
	let left_gp = left_state.registers.index_of(&left_from);
	let right_gp = right_state.registers.index_of(&right_from);
	let left_empty = left_gp
		.map(|idx| state.registers.get(idx).is_some())
		.unwrap_or(false);
	let right_empty = right_gp
		.map(|idx| state.registers.get(idx).is_some())
		.unwrap_or(false);

	let resulting_register = match (left_gp, left_empty, right_gp, right_empty) {
		// Both are available, left is free, default to left branch
		(Some(l), true, Some(r), _) => {
			// move to left, -> left
			if l != r {
				right_state.registers.swap(l, r);
				right_end_block.push(Expression::UnOp(UnOp {
					target: Register::GeneralPurpose(l),
					op: Op::Swap,
					lhs: Register::GeneralPurpose(r),
				}));
			}
			l
		}
		// Left is available, right is not, go for left
		(Some(l), true, None, _) => {
			// load @ left, -> left
			todo!()
		}
		// Both are available, only right is free, go for right
		(Some(l), false, Some(r), true) => {
			// move to right, -> right
			if l != r {
				left_state.registers.swap(l, r);
				left_end_block.push(Expression::UnOp(UnOp {
					target: Register::GeneralPurpose(r),
					op: Op::Swap,
					lhs: Register::GeneralPurpose(l),
				}));
			}
			r
		}
		// Right is available and free, left is not, go for right
		(None, _, Some(r), true) => {
			// load @ right, -> right
			todo!()
		}
		// Neither left nor right are available and free, go for first empty slot
		_ => {
			// any free
			let suggested_register = state.registers.iter().position(Option::is_none);
			if let Some(idx) = suggested_register {
				load_value_to_branch(left_end_block, left_state, left_from, idx)?;
				load_value_to_branch(
					right_end_block,
					right_state,
					right_from,
					idx,
				)?;
				idx
			} else {
				todo!("Select a value to throw out")
			}
		}
	};

	Ok(resulting_register)
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
		out: BlockEnd::Return(Register::Literal(0)),
	};

	assert!(intro.iter().all(|reg| state.contains(reg.target)));

	for (line_idx, line) in block.iter().enumerate() {
		let pos = (block_idx, line_idx);
		match line {
			SimpleExpression::BinOp(SimpleBinOp {
				target,
				op,
				lhs,
				rhs,
			}) if !op.is_floating_point() => {
				let lhs_rhs = [lhs.clone(), rhs.clone()];
				let lhs_register = get_or_load_and_get_value(
					lhs.clone(),
					state,
					pos,
					&mut new_block,
					scope,
					&lhs_rhs,
				);
				let rhs_register = get_or_load_and_get_value(
					rhs.clone(),
					state,
					pos,
					&mut new_block,
					scope,
					&lhs_rhs,
				);

				let target_register =
					select_and_save_old(pos, state, scope, &mut new_block)?;

				let expr = Expression::BinOp(BinOp {
					target: target_register,
					op: op.into(),
					lhs: lhs_register,
					rhs: rhs_register,
				});

				state.registers[target_register.general_purpose().unwrap()] =
					Some(Source::Register(*target));

				new_block.block.push(expr);
			}
			SimpleExpression::UnOp(_) => todo!(),
			SimpleExpression::FunctionCall(SimpleFunctionCall {
				target,
				function,
				args,
			}) => {
				log::trace!("Function call: {target} ← {function}{args:?}");
				let (register_args, stack_args) = {
					// If the min is hit stack_args is guaranteed to be empty and this is required by indexing.
					// Honestly, it should just trim it down to max allowed size
					let split = (config.argument_registers as usize)
						.min(args.len());
					(&args[..split], &args[split..])
				};
				log::trace!(
					"stack: {stack_args:#?}\nregisters: {register_args:#?}"
				);
				// Do the arguments that end up on the stack first as to not interfere with whatever calculations
				// are required for the stack arguments
				for (idx, arg) in stack_args.iter().cloned().enumerate() {
					let arg_register = get_or_load_and_get_value(
						arg,
						state,
						pos,
						&mut new_block,
						scope,
						&[],
					);
					let expr = Expression::BinOp(BinOp {
						target: arg_register,
						op: Op::StoreMem,
						lhs: Register::StackPointer,
						// Negative offset so they end up in the called function's stack frame
						rhs: Register::Literal(
							-((idx + 1) as isize) as u64,
						),
					});
					new_block.block.push(expr);
				}

				// And then register arguments
				// Saving those we're about to
				let unprotected = config.unprotected_registers.iter().copied();
				let arguments = (0..config.argument_registers as usize)
					.map(Register::GeneralPurpose);
				for reg in unprotected.chain(arguments) {
					// This'd be so much nicer with if-let chains
					match reg {
						Register::GeneralPurpose(idx) => {
							// Save it if it's used AND will be used again
							if let Some(src) = &state.registers[idx] {
								if simplify::lines_till_last_use(src, scope, pos).is_some() {
									log::trace!("Saving {src:?}");
									save_on_stack(
										&mut state.registers,
										pos,
										idx,
										&mut new_block,
										scope,
										Register::GeneralPurpose(idx),
										&mut state.stack,
									);
								}
							}
						},
						_ => todo!("Handle saving of unprotected non-gp registers"),
					}
				}
				// And then loading in the new values
				for (idx, arg) in register_args.iter().cloned().enumerate() {
					match &state.registers[idx] {
						None => {
							log::trace!("Loading to None: {arg:?}");
							switch_or_load_value(
								idx,
								arg,
								&mut state.registers,
								&mut new_block.block,
								&mut state.stack,
							);
						}
						Some(src) => {
							// Load if not already there
							if src != &arg {
								log::trace!("Loading to {src:?}: {arg:?}");
								switch_or_load_value(
									idx,
									arg,
									&mut state.registers,
									&mut new_block.block,
									&mut state.stack,
								);
							}
						}
					}
				}
				// And clear because the ABI says we have no guarantees of what's happened to it.
				// Any restoration will be done when it's needed again
				// This is separate from the first loop to prevent reloading values already in the correct place
				for reg in config.unprotected_registers.iter() {
					match reg {
						&Register::GeneralPurpose(idx) => {
							state.registers[idx] = None;
						},
						_ => todo!("Handle saving of unprotected non-gp registers"),
					}
				}

				// And lastly check if the return register needs preserving
				let ret_reg = config.return_register as usize;
				let value_in_return_slot = &state.registers[ret_reg];
				if let Some(val) = value_in_return_slot {
					if simplify::lines_till_last_use(val, scope, pos).is_some()
					{
						log::trace!("Saving value that was in return slot: {val:?}");
						save_on_stack(
							&mut state.registers,
							pos,
							ret_reg,
							&mut new_block,
							scope,
							Register::GeneralPurpose(ret_reg),
							&mut state.stack,
						);
					}
				}

				let expr = Expression::FunctionCall(FunctionCall {
					function_name: function.clone(),
					args: stack_args.len(),
				});
				new_block.block.push(expr);
				state.registers[ret_reg] = Some(Source::Register(*target));
			}
			_ => todo!(),
		}
		log::trace!(
			"[{}]: {line:?}\nRegs: {:#?}\nStack: {:#?}\n",
			line!(),
			&state.registers,
			&state.stack
		);
	}

	let new_out = match out {
		SimpleBlockEnd::Return(target) => {
			let condition = get_or_load_condition(
				target.clone(),
				block_idx,
				state,
				scope,
				&mut new_block,
			)?;
			BlockEnd::Return(condition)
		}
		SimpleBlockEnd::One(o) => BlockEnd::One(o.clone()),
		SimpleBlockEnd::Two(target, l, r) => {
			let condition = get_or_load_condition(
				target.clone(),
				block_idx,
				state,
				scope,
				&mut new_block,
			)?;
			BlockEnd::Two(condition, *l, *r)
		}
	};
	new_block.out = new_out;

	Ok(new_block)
}

fn get_or_load_condition(
	target: Source,
	block_idx: usize,
	state: &mut State,
	scope: &[SimpleBlock],
	new_block: &mut Block,
) -> Result<Register, Error> {
	state.find(target.clone())
		.or_else(|| {
			let idx = state
				.registers
				.iter()
				.position(Option::is_none)
				.unwrap_or_else(|| {
					select_register((block_idx, usize::MAX), state, scope, &[])
						.0
						.general_purpose()
						.expect("Hardcoded false")
				});
			load_value_to_branch(&mut new_block.block, state, target, idx).ok()?;
			Some(Register::GeneralPurpose(idx))
		})
		.ok_or(Error::MissingRegister(line!()))
}
