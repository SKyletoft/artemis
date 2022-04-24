use std::collections::HashMap;

use anyhow::{bail, Result};
use derive_more::{Add, AddAssign, From};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{BinOp, FunctionCall, IfExpr, Literal, Op, Subexpr},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
pub struct Register(u32);

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
pub struct BlockId(u32);

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
	pub variables: HashMap<SmallString, Register>,
	pub next_register: Register,
}

impl Context {
	fn next(&mut self) -> Register {
		let next = self.next_register;
		self.next_register += Register(1);
		next
	}
}


#[derive(Debug, Copy, Clone, PartialEq, Variantly)]
pub enum Source {
	Register(Register),
	Integer(u64),
	Float(f64),
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SimpleBinOp {
	pub target: Register,
	pub op: SimpleOp,
	pub lhs: Source,
	pub rhs: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleUnOp {
	pub target: Register,
	pub lhs: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleFunctionCall {
	pub target: Register,
	pub function: SmallString,
	pub args: SmallVec<[Source; 4]>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleExpression {
	BinOp(SimpleBinOp),
	UnOp(SimpleUnOp),
	FunctionCall(SimpleFunctionCall),
}

