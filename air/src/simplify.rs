use std::{collections::HashMap, fmt};

use anyhow::Result;
use derive_more::{Add, AddAssign, From, Into};
use smallvec::SmallVec;
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;

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
pub struct Register(pub usize);

impl fmt::Display for Register {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "${}", self.0)
	}
}

#[derive(
	Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From, Into,
)]
#[repr(transparent)]
pub struct BlockId(pub usize);

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
			Self::Two(reg, left, right) => write!(f, "{reg} ? {left}, {right}"),
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
	Register(Register),
	Value(u64),
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
	pub value: SmallVec<[PhiEdge; 2]>,
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
