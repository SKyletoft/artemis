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

#[derive(Debug, Clone, PartialEq, Variantly)]
enum MaybeParsed<'a> {
	Parsed(AST),
	Unparsed(Pair<'a, Rule>),
	Operator(Op),
	Empty,
}

impl<'a> TryFrom<Pair<'a, Rule>> for MaybeParsed<'a> {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'a, Rule>) -> Result<MaybeParsed<'a>, Self::Error> {
		match pair.as_rule() {
			Rule::term => {
				let inner = pair
					.into_inner()
					.next()
					.expect("Rule should always contain exactly one subrule");
				let subexpr = match inner.as_rule() {
					Rule::float => Subexpr::Literal(Literal::Float(inner.as_str().parse()?)),
					Rule::integer => {
						// Try to parse as unsigned, on fail, retry as signed
						// instead and bitcast to unsigned because our IR
						// only supports unsigned
						let val = inner.as_str().parse::<u64>().or_else(|_| -> Result<u64> {
							Ok(inner.as_str().parse::<i64>()? as u64)
						})?;
						Subexpr::Literal(Literal::Integer(val))
					}
					Rule::boolean => Subexpr::Literal(Literal::Boolean(inner.as_str().parse()?)),
					Rule::unit => Subexpr::Literal(Literal::Unit),
					Rule::tuple => {
						let tuple = inner.into_inner()
							.into_iter()
							.map(|pair| {
								let expr = AST::try_from(pair)?
									.subexpr()
									.ok_or(Error::ParseError)?;
								Ok(expr)
							})
							.collect::<Result<Vec<_>>>()?;
						Subexpr::Tuple(tuple)
					},
					Rule::subexpr => {
						// Parentheses
						AST::try_from(inner)?.subexpr().ok_or(Error::ParseError)?
					}
					Rule::block => todo!(),
					Rule::if_expr => todo!(),
					Rule::function_call => {
						let mut inner_2 = inner
							.into_inner()
							.map(AST::try_from)
							.collect::<Result<SmallVec<[AST; 8]>>>()?;
						// There is always at least the name of the function being called
						assert!(!inner_2.is_empty());
						let function_name = inner_2
							.remove(0)
							.subexpr()
							.and_then(Subexpr::variable)
							.ok_or(Error::Internal)?;
						let arguments = inner_2
							.into_iter()
							.map(AST::subexpr)
							.collect::<Option<Vec<_>>>()
							.ok_or(Error::ParseError)?;
						Subexpr::FunctionCall(FunctionCall {
							function_name,
							arguments,
						})
					}
					Rule::var_name => Subexpr::Variable(inner.as_str().into()),
					_ => bail!(Error::Internal),
				};
				Ok(MaybeParsed::Parsed(AST::Subexpr(subexpr)))
			}
			Rule::operator => {
				let inner_op = pair
					.into_inner()
					.next()
					.expect("Rule should always contain exactly one subrule")
					.as_rule();
				let op = match inner_op {
					Rule::plus => Op::Plus,
					Rule::minus => Op::Minus,
					Rule::delta => Op::Delta,
					Rule::times => Op::Times,
					Rule::div => Op::Div,
					Rule::exp => Op::Exp,
					Rule::not => Op::Not,
					Rule::and => Op::And,
					Rule::or => Op::Or,
					Rule::xor => Op::Xor,
					_ => bail!(Error::Internal),
				};
				Ok(MaybeParsed::Operator(op))
			}
			_ => unreachable!(),
		}
	}
}

