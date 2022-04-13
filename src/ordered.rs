use std::mem;

use crate::{error::Error, Rule};
use anyhow::{bail, Result};
use pest::iterators::Pair;
use smallvec::SmallVec;
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	name: SmallString,
	arguments: SmallVec<[Argument; 4]>,
	return_type: RawType,
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
pub struct FunctionCall {
	function_name: SmallString,
	arguments: Vec<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Subexpr(Subexpr),
	Declaration(Declaration),
	Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Subexpr {
	BinOp(BinOp),
	IfExpr(IfExpr),
	Block(Block),
	Literal(Literal),
	Variable(SmallString),
	Tuple(Vec<Subexpr>),
	FunctionCall(FunctionCall),
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
	Assignment(Assignment),
	Function(Function),
	Argument(Argument),
	Expr(Expr),
	Subexpr(Subexpr),
	IfExpr(IfExpr),
	#[variantly(rename = "TypeLiteral")]
	Type(Type),
	RawType(RawType),
	TopLevelConstruct(TopLevelConstruct),
	Literal(Literal),
	Block(Block),
	RawToken(SmallString),
}

macro_rules! remove_by_pattern {
	($e:expr, $p:pat, $a:expr) => {
		// Poor man's try block
		(|| {
			let e: &mut SmallVec<[AST; 8]> = $e;

			// Find the last one for minor performance improvements
			#[allow(unused_variables)] // Unused variable is named so the pattern can be used twice
			let idx = e.iter().rev().position(|n| matches!(n, $p))?;
			let wrapped = e.remove(e.len() - idx - 1);

			if let $p = wrapped {
				Some($a)
			} else {
				unreachable!()
			}
		})()
	};
}

