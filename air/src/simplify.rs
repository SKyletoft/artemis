use std::{cmp::Ordering, collections::HashMap, fmt};

use anyhow::Result;
use derive_more::{Add, AddAssign, From, Into};
use smallvec::SmallVec;
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;

/// `usize`
#[derive(
	Debug,
	Clone,
	Copy,
	Add,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	AddAssign,
	Default,
	From,
	Into,
	Hash,
)]
#[repr(transparent)]
pub struct Register(usize);

impl fmt::Display for Register {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "${}", self.0)
	}
}

/// `usize`
#[derive(
	Debug,
	Clone,
	Copy,
	Add,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	AddAssign,
	Default,
	From,
	Into,
	Hash,
)]
#[repr(transparent)]
pub struct BlockId(usize);

impl fmt::Display for BlockId {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "#{}", self.0)
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
	pub variables: HashMap<SmallString, Source>,
	pub next_register: Register,
}

impl Context {
	pub fn next_register(&mut self) -> Register {
		let next = self.next_register;
		self.next_register += Register(1);
		next
	}
}

#[derive(Clone, Copy, PartialEq, Variantly)]
pub enum BlockEnd {
	#[variantly(rename = "ret")]
	Return(Source),
	One(BlockId),
	Two(Source, BlockId, BlockId),
}

impl fmt::Debug for BlockEnd {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Return(reg) => write!(f, "ret {reg}"),
			Self::One(target) => write!(f, "{target}"),
			Self::Two(reg, left, right) => write!(f, "{reg} ? {left} : {right}"),
		}
	}
}

impl Default for BlockEnd {
	fn default() -> Self {
		BlockEnd::Return(Source::Value(0))
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Block {
	pub intro: SmallVec<[PhiNode; 2]>, // Hardcoded 2 because I think this might be able to be an array or just 2 values, but I'm not 100% yet
	pub block: SmallVec<[SimpleExpression; 4]>,
	pub out: BlockEnd,
}

/// Registers | Value
#[derive(Debug, Copy, Clone, PartialEq, Eq, Variantly, Hash)]
pub enum Source {
	/// `Register`
	Register(Register),
	/// `u64`
	Value(u64),
}

impl Source {
	pub fn has_been_used_for_the_last_time(
		&self,
		(r_block, r_line): (usize, usize),
		last_use_of_register: &HashMap<Source, (usize, usize)>,
		debug: bool,
	) -> bool {
		if debug {
			dbg!(last_use_of_register, self, (r_block, r_line));
		}

		last_use_of_register
			.get(self)
			.map(|&(l_block, l_line)| match r_block.cmp(&l_block) {
				Ordering::Less => false,
				Ordering::Equal => l_line < r_line,
				Ordering::Greater => true,
			})
			.unwrap_or(true)
	}

	/// Counts the amount of lines until the last use of a Source within this function
	// TODO: Doesn't work at all for loops
	pub fn lines_till_last_use(
		&self,
		scope: &[Block],
		(block_idx, line_idx): (usize, usize),
	) -> Option<usize> {
		let block = &scope[block_idx];
		let in_this_block = block.intro.len() + block.block.len() - line_idx;

		// If it's the condition of this block we don't need to search through every line
		match &block.out {
			&BlockEnd::Two(condition, left, right) if &condition == self => {
				assert!(scope[usize::from(left)].intro.is_empty());
				assert!(scope[usize::from(right)].intro.is_empty());

				let steps_in_left_block =
					self.lines_till_last_use(scope, (left.into(), 0));
				let steps_in_right_block =
					self.lines_till_last_use(scope, (right.into(), 0));

				return match (steps_in_left_block, steps_in_right_block) {
					(Some(l), Some(r)) => Some(l.max(r) + in_this_block),
					(Some(n), None) | (None, Some(n)) => {
						Some(n + in_this_block)
					}
					(None, None) => Some(in_this_block),
				};
			}
			_ => (),
		}

		// Count steps until the next use
		for (steps, line) in block.block.iter().skip(line_idx + 1).enumerate() {
			let contains_self = match line {
				SimpleExpression::BinOp(SimpleBinOp { lhs, rhs, .. }) => {
					lhs == self || rhs == self
				}
				SimpleExpression::UnOp(SimpleUnOp { lhs, .. }) => lhs == self,
				SimpleExpression::FunctionCall(SimpleFunctionCall {
					args, ..
				}) => args.iter().any(|arg| arg == self),
			};
			if contains_self {
				// Then recurse so we don't have issues with several uses within a block
				let steps_so_far = steps + self
					.lines_till_last_use(
						scope,
						(block_idx, line_idx + steps + 1),
					)
					.unwrap_or(0);
				return Some(steps_so_far);
			}
		}

		let res = match block.out {
			BlockEnd::Return(_) => None,
			BlockEnd::One(next) => {
				let steps_in_intro =
					scope[usize::from(next)].intro.iter().rposition(
						|PhiNode {
						         value:
						                 [PhiEdge { value: l, .. }, PhiEdge { value: r, .. }],
						         ..
						 }| l == self || r == self,
					);

				let steps_in_next_block =
					self.lines_till_last_use(scope, (next.into(), 0));
				steps_in_next_block
					.or(steps_in_intro)
					.map(|n| n + in_this_block)
			}
			BlockEnd::Two(_, left, right) => {
				// Not actually an invariant, just a todo
				assert!(scope[usize::from(left)].intro.is_empty());
				assert!(scope[usize::from(right)].intro.is_empty());

				let steps_in_left_block =
					self.lines_till_last_use(scope, (left.into(), 0));
				let steps_in_right_block =
					self.lines_till_last_use(scope, (right.into(), 0));

				let max = match (steps_in_left_block, steps_in_right_block) {
					(Some(l), Some(r)) => Some(l.max(r)),
					(Some(n), None) | (None, Some(n)) => Some(n),
					(None, None) => None,
				};
				max.map(|v| v + in_this_block)
			}
		};

		res
	}
}

impl fmt::Display for Source {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Source::Register(Register(r)) => write!(f, "${r}"),
			Source::Value(v) => write!(f, "{v}"),
		}
	}
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SimpleOp {
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
}

impl fmt::Display for SimpleOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			SimpleOp::Add => write!(f, "+"),
			SimpleOp::Sub => write!(f, "-"),
			SimpleOp::Abs => write!(f, "±"),
			SimpleOp::Mul => write!(f, "×"),
			SimpleOp::Div => write!(f, "÷"),
			SimpleOp::FAdd => write!(f, "+."),
			SimpleOp::FSub => write!(f, "-."),
			SimpleOp::FAbs => write!(f, "±."),
			SimpleOp::FMul => write!(f, "×."),
			SimpleOp::FDiv => write!(f, "÷."),
			SimpleOp::And => write!(f, "Λ"),
			SimpleOp::Or => write!(f, "V"),
			SimpleOp::Xor => write!(f, "⊕"),
			SimpleOp::Not => write!(f, "¬"),
		}
	}
}

impl SimpleOp {
	pub fn is_floating_point(&self) -> bool {
		matches!(
			self,
			SimpleOp::FAdd
				| SimpleOp::FSub | SimpleOp::FAbs
				| SimpleOp::FMul | SimpleOp::FDiv
		)
	}
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SimpleBinOp {
	pub target: Register,
	pub op: SimpleOp,
	pub lhs: Source,
	pub rhs: Source,
}

impl SimpleBinOp {
	pub fn is_same(&self, other: &Self) -> bool {
		self.op == other.op
			&& ((self.lhs == other.lhs && self.rhs == other.rhs)
				|| (self.lhs == other.rhs && self.rhs == other.lhs))
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleUnOp {
	pub target: Register,
	pub op: SimpleOp,
	pub lhs: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleFunctionCall {
	pub target: Register,
	pub function: SmallString,
	pub args: SmallVec<[Source; 4]>,
}

#[derive(Clone, PartialEq)]
pub enum SimpleExpression {
	BinOp(SimpleBinOp),
	UnOp(SimpleUnOp),
	FunctionCall(SimpleFunctionCall),
}

impl fmt::Debug for SimpleExpression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			SimpleExpression::BinOp(SimpleBinOp {
				target,
				op,
				lhs,
				rhs,
			}) => write!(f, "{target} ← {lhs} {op} {rhs}"),
			SimpleExpression::UnOp(SimpleUnOp { target, op, lhs }) => {
				write!(f, "{target} ← {op} {lhs}")
			}
			SimpleExpression::FunctionCall(SimpleFunctionCall {
				target,
				function,
				args,
			}) => write!(f, "{target} ← {function}{args:?}"),
		}
	}
}

impl SimpleExpression {
	pub fn get_target(&self) -> Register {
		match self {
			SimpleExpression::BinOp(SimpleBinOp { target, .. })
			| SimpleExpression::UnOp(SimpleUnOp { target, .. })
			| SimpleExpression::FunctionCall(SimpleFunctionCall { target, .. }) => *target,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum SSAConstruct {
	Function {
		name: SmallString,
		args: usize,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PhiEdge {
	pub from: BlockId,
	pub value: Source,
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
	pub value: [PhiEdge; 2],
}

impl fmt::Debug for PhiNode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let PhiNode { target, value } = self;
		write!(f, "{target} ← φ{value:?}")
	}
}

pub fn validate_ir(construct: &SSAConstruct) -> Result<()> {
	todo!()
}
