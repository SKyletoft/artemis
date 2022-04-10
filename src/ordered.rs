use crate::{error::Error, Rule};
use anyhow::Result;
use pest::iterators::{Pair, Pairs};
use smallvec::SmallVec;
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	name: SmallString,
	arguments: SmallVec<[Argument; 4]>,
	return_type: Type,
	block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	name: SmallString,
	type_name: Type,
	value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	name: SmallString,
	value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	type_name: Type,
	name: SmallString,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Subexpr(Box<Subexpr>),
	Declaration,
	Assignment,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Subexpr {
	BinOp(BinOp),
	If(IfExpr),
	Block(Block),
	Literal(Literal),
	Variable(SmallString),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Integer(u64),
	Float(f64),
	Boolean(bool),
	Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
	lhs: Box<Subexpr>,
	op: Op,
	rhs: Box<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	condition: Box<Subexpr>,
	lhs: Block,
	rhs: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Plus,
	Minus,
	Delta,
	Times,
	Div,
	Exp,
	Not,
	And,
	Or,
	Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawType {
	Integer,
	Natural,
	Real,
	Boolean,
	Unit,
	Struct(SmallString),
	Tuple(Vec<Type>),
	Inferred,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	Const(RawType),
	Mutable(RawType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum AST {
	Declaration(Declaration),
	Function(Function),
	Argument(Argument),
	Expr(Expr),
	Subexpr(Subexpr),
	IfExpr(IfExpr),
	#[variantly(rename = "TypeLiteral")]
	Type(Type),
	TopLevelConstruct(TopLevelConstruct),
	Literal(Literal),
	Block(Block),
	RawToken(SmallString),
}

fn parse_raw_type(s: &str) -> RawType {
	match s {
		"ℕ" | "Nat" => RawType::Natural,
		"ℤ" | "Int" => RawType::Integer,
		"ℝ" | "Real" => RawType::Real,
		"𝔹" | "Bool" => RawType::Boolean,
		_ => RawType::Struct(s.into()),
	}
}

fn parse_type(s: &str) -> Type {
	if let Some(rest) = s.strip_prefix("mut") {
		let rest = rest.trim_start();
		let raw = parse_raw_type(rest);
		Type::Mutable(raw)
	} else {
		let raw = parse_raw_type(s);
		Type::Const(raw)
	}
}
