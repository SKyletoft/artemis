use anyhow::Result;
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	ast::{BinaryOperator, Pattern, UnaryOperator},
	type_definition::{EnumType2, Type2},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple(pub(crate) Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub(crate) pattern: Pattern,
	pub(crate) type_name: Type2,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub(crate) pattern: Pattern,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub(crate) func: Expr,
	pub(crate) args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PartialApplication {
	pub(crate) func: Expr,
	pub(crate) args: Vec<Option<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	pub(crate) condition: Expr,
	pub(crate) then_branch: Expr,
	pub(crate) else_branch: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
	pub(crate) expr: Expr,
	pub(crate) cases: Vec<Case>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
	pub(crate) pattern: Pattern,
	pub(crate) condition: Option<Expr>,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Expr {
	BinOp {
		left: Box<Expr>,
		right: Box<Expr>,
		op: BinaryOperator,
	},
	UnOp {
		op: UnaryOperator,
		right: Box<Expr>,
	},
	Leaf(Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Term {
	TypeValue(u64),
	Float(f64),
	Integer(i64),
	Boolean(bool),
	String(SmallString),
	Char(char),
	Unit,
	Tuple(Tuple),
	StructLiteral(StructLiteral),
	Block(Block),
	IfExpr(IfExpr),
	MatchExpr(MatchExpr),
	FunctionCall(FunctionCall),
	PartialApplication(PartialApplication),
	Declaration(Declaration),
	Assignment(Assignment),
	FunctionDefinition(FunctionDefinition),
	VarName(SmallString),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub(crate) name: SmallString,
	pub(crate) type_name: Type2,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
	pub(crate) name: SmallString,
	pub(crate) args: SmallVec<[Argument; 1]>,
	pub(crate) return_type: EnumType2,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub(crate) Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldLiteral {
	pub(crate) name: SmallString,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral(pub(crate) Vec<StructFieldLiteral>);
