use anyhow::{bail, Result};
use once_cell::sync::Lazy;
use smallvec::{smallvec, SmallVec};
use variantly::Variantly;

use crate::{
	ast::{BinaryOperator, UnaryOperator},
	type_definition::{ActualType2, RawType2},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: SmallString,
	pub arguments: Vec<SmallString>,
	pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub name: SmallVec<[SmallString; 1]>,
	pub value: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub function_name: SmallString,
	pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Term(Term),
	Declaration(Declaration),
	Assignment(Declaration),
	Function(Box<Function>),
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Term {
	BinOp(BinOp),
	UnOp(UnOp),
	IfExpr(IfExpr),
	Block(Vec<Expr>),
	Literal(u64),
	Unit,
	Variable(SmallString),
	Tuple(Vec<Expr>),
	FunctionCall(FunctionCall),
	Expr(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
	pub lhs: Box<Expr>,
	pub op: Op,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnOp {
	pub op: Op,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	pub condition: Box<Expr>,
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
	Plus,
	Minus,
	Delta,
	Times,
	Div,
	UDiv,
	Exp,
	FPlus,
	FMinus,
	FDelta,
	FTimes,
	FDiv,
	FExp,
	Not,
	And,
	Or,
	Xor,
	Dot,
	GT,
	FGT,
	UGT,
	GTE,
	FGTE,
	UGTE,
	LT,
	FLT,
	ULT,
	LTE,
	FLTE,
	ULTE,
	Eq,
	FEq,
	Neq,
	FNeq,
}

impl From<(UnaryOperator, Type)> for Op {
	fn from(x: (UnaryOperator, Type)) -> Self {
		match x {
			(UnaryOperator::Not, Type::Floating) => Op::Not,
			(UnaryOperator::Not, _) => Op::Not,
			(UnaryOperator::Sub, Type::Floating) => Op::FMinus,
			(UnaryOperator::Sub, _) => Op::Minus,
		}
	}
}

impl From<(BinaryOperator, Type)> for Op {
	fn from(x: (BinaryOperator, Type)) -> Self {
		match x {
			(BinaryOperator::Exp, Type::Floating) => Op::FExp,
			(BinaryOperator::Exp, _) => Op::Exp,
			(BinaryOperator::Mul, Type::Floating) => Op::FTimes,
			(BinaryOperator::Mul, _) => Op::Times,
			(BinaryOperator::Div, Type::Signed) => Op::Div,
			(BinaryOperator::Div, Type::Unsigned) => Op::UDiv,
			(BinaryOperator::Div, Type::Floating) => Op::FDiv,
			(BinaryOperator::Rem, _) => todo!(),
			(BinaryOperator::Add, Type::Floating) => Op::FPlus,
			(BinaryOperator::Add, _) => Op::Plus,
			(BinaryOperator::Sub, Type::Floating) => Op::FMinus,
			(BinaryOperator::Sub, _) => Op::Minus,
			(BinaryOperator::Delta, Type::Floating) => Op::FDelta,
			(BinaryOperator::Delta, _) => Op::Delta,
			(BinaryOperator::And, _) => Op::And,
			(BinaryOperator::Or, _) => Op::Or,
			(BinaryOperator::Xor, _) => Op::Xor,
			(BinaryOperator::Dot, _) => Op::Dot,
			(BinaryOperator::Eq, _) => todo!(),
			(BinaryOperator::Neq, _) => todo!(),
			(BinaryOperator::Gt, _) => todo!(),
			(BinaryOperator::Lt, _) => todo!(),
			(BinaryOperator::Gte, _) => todo!(),
			(BinaryOperator::Lte, _) => todo!(),
			(BinaryOperator::LShift, _) => todo!(),
			(BinaryOperator::RShift, _) => todo!(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
	Signed,
	Unsigned,
	Floating,
}

impl Default for Type {
	fn default() -> Self {
		Type::Unsigned
	}
}

impl From<&ActualType2> for Type {
	fn from(t: &ActualType2) -> Self {
		let t = match t {
			ActualType2::Declared(t) => t,
			ActualType2::Inferred(t) => t.as_ref(),
		};
		match t.enum_type.0.as_slice() {
			[RawType2::Natural] => Type::Unsigned,
			[RawType2::Real] => Type::Floating,
			[RawType2::Integer] => Type::Signed,
			[RawType2::NumberLiteral] => Default::default(),
			_ => Default::default(),
		}
	}
}
