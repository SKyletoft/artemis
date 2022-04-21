use std::mem;

use anyhow::{bail, Result};
use pest::{
	iterators::{Pair, Pairs},
	Parser,
};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{error::Error, GeneratedParser, Rule};

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: SmallString,
	pub arguments: SmallVec<[Argument; 4]>,
	pub return_type: RawType,
	pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub name: SmallString,
	pub type_name: Type,
	pub value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub name: SmallString,
	pub value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub type_name: Type,
	pub name: SmallString,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub function_name: SmallString,
	pub arguments: Vec<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Subexpr(Subexpr),
	Declaration(Declaration),
	Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq, Variantly)]
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
	pub lhs: Box<Subexpr>,
	pub op: Op,
	pub rhs: Box<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	pub condition: Box<Subexpr>,
	pub lhs: Block,
	pub rhs: Block,
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
	Dot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawType {
	Integer,
	Natural,
	Real,
	IntegerLiteral,
	Boolean,
	Unit,
	Struct(SmallString),
	Tuple(Vec<Type>),
	Inferred,
}

impl RawType {
	pub fn default_int(self) -> Self {
		if let RawType::IntegerLiteral = &self {
			RawType::Integer
		} else {
			self
		}
	}

	pub fn integer_equality(&self, rhs: &RawType) -> bool {
		if !(self == &RawType::IntegerLiteral || rhs == &RawType::IntegerLiteral) {
			return self == rhs;
		}
		matches!(self, RawType::Integer | RawType::Natural)
			|| matches!(rhs, RawType::Integer | RawType::Natural)
			|| matches!(
				(self, rhs),
				(RawType::IntegerLiteral, RawType::IntegerLiteral)
			)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub mutable: bool,
	pub raw: RawType,
}

impl Type {
	pub fn default_int(mut self) -> Self {
		self.raw = self.raw.default_int();
		self
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}

impl TryFrom<AST> for TopLevelConstruct {
	type Error = anyhow::Error;

	fn try_from(value: AST) -> Result<Self, Self::Error> {
		match value.clone() {
			AST::Declaration(d) => Ok(TopLevelConstruct::Declaration(d)),
			AST::Function(f) => Ok(TopLevelConstruct::Function(f)),
			_ => {
				log::error!("Illegal construct at top level: {value:?}");
				bail!(Error::ParseError)
			}
		}
	}
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
			#[allow(unused_variables)]
			// Unused variable is named so the pattern can be used twice
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
enum MaybeParsed {
	Parsed(AST),
	Operator(Op),
	Empty,
}

impl<'a> TryFrom<Pair<'a, Rule>> for MaybeParsed {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'a, Rule>) -> Result<MaybeParsed, Self::Error> {
		match pair.as_rule() {
			Rule::term => {
				let inner = pair
					.into_inner()
					.next()
					.expect("Rule should always contain exactly one subrule");
				let subexpr = match inner.as_rule() {
					Rule::float => Subexpr::Literal(Literal::Float(
						inner.as_str().parse()?,
					)),
					Rule::integer => {
						// Try to parse as unsigned, on fail, retry as signed
						// instead and bitcast to unsigned because our IR
						// only supports unsigned
						let val = inner.as_str().parse::<u64>().or_else(
							|_| -> Result<u64> {
								Ok(inner.as_str().parse::<i64>()?
									as u64)
							},
						)?;
						Subexpr::Literal(Literal::Integer(val))
					}
					Rule::boolean => Subexpr::Literal(Literal::Boolean(
						inner.as_str().parse()?,
					)),
					Rule::unit => Subexpr::Literal(Literal::Unit),
					Rule::tuple => {
						let tuple = inner
							.into_inner()
							.map(|pair| {
								let expr = AST::try_from(pair)?
									.subexpr()
									.ok_or(Error::ParseError)?;
								Ok(expr)
							})
							.collect::<Result<Vec<_>>>()?;
						Subexpr::Tuple(tuple)
					}
					// Parentheses
					Rule::subexpr => AST::try_from(inner)?
						.subexpr()
						.ok_or(Error::ParseError)?,
					Rule::block => {
						let vec = AST::try_from(inner)?
							.block()
							.ok_or(Error::ParseError)?;
						Subexpr::Block(vec)
					}
					Rule::if_expr => AST::try_from(inner)?
						.subexpr()
						.ok_or(Error::ParseError)?,
					Rule::function_call => {
						let mut inner_2 = inner
							.into_inner()
							.map(AST::try_from)
							.collect::<Result<SmallVec<[AST; 8]>>>()?;
						// There is always at least the name of the function being called
						if inner_2.is_empty() {
							bail!(Error::Internal);
						}
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

fn replace_bin_op(tokens: &mut SmallVec<[MaybeParsed; 4]>, idx: usize) -> Result<()> {
	if idx < 1 {
		bail!(Error::Internal);
	}
	let op = tokens.remove(idx).operator().ok_or(Error::Internal)?;
	let lhs = mem::replace(&mut tokens[idx - 1], MaybeParsed::Empty)
		.parsed()
		.and_then(AST::subexpr)
		.ok_or(Error::ParseError)?;
	let rhs = tokens
		.remove(idx)
		.parsed()
		.and_then(AST::subexpr)
		.ok_or(Error::ParseError)?;
	let res = Subexpr::BinOp(BinOp {
		lhs: Box::new(lhs),
		op,
		rhs: Box::new(rhs),
	});
	tokens[idx - 1] = MaybeParsed::Parsed(AST::Subexpr(res));
	Ok(())
}

impl<'a> TryFrom<Pair<'a, Rule>> for AST {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
		let res = match pair.as_rule() {
			Rule::var_name => AST::Subexpr(Subexpr::Variable(pair.as_str().into())),
			Rule::type_name => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				let mutable = remove_by_pattern!(&mut inner, AST::RawToken(a), a)
					.map(|s| s == "mut") // Because it could be `ref` too
					.unwrap_or(false);
				let raw = remove_by_pattern!(&mut inner, AST::RawType(a), a)
					.ok_or(Error::ParseError)?;
				AST::Type(Type { mutable, raw })
			}
			Rule::mutable => AST::RawToken("mut".into()),
			Rule::raw_type => {
				// `raw_type` rule always contains exactly one thing
				let inner = pair.into_inner().next().ok_or(Error::Internal)?;
				let raw = match inner.as_rule() {
					Rule::unit => RawType::Unit,
					Rule::native_types => match inner.as_str() {
						"ℕ" => RawType::Natural,
						"ℤ" => RawType::Integer,
						"ℝ" => RawType::Real,
						"𝔹" => RawType::Boolean,
						_ => unreachable!(),
					},
					Rule::tuple_type => {
						// Fix this when Type has been moved out into its own TryFrom impl
						let contained_types = inner
							.into_inner()
							.map(|pair| {
								AST::try_from(pair)
									.ok()
									.and_then(AST::type_literal)
							})
							.collect::<Option<Vec<Type>>>()
							.ok_or(Error::ParseError)?;
						RawType::Tuple(contained_types)
					}
					Rule::struct_name => match inner.as_str() {
						"Nat" => RawType::Natural,
						"Int" => RawType::Integer,
						"Real" => RawType::Real,
						"Bool" => RawType::Boolean,
						_ => RawType::Struct(inner.as_str().into()),
					},
					_ => unreachable!(),
				};
				AST::RawType(raw)
			}
			Rule::declaration => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				// Name, maybe type and value
				if !(inner.len() == 2 || inner.len() == 3) {
					bail!(Error::Internal);
				}

				let value = remove_by_pattern!(&mut inner, AST::Subexpr(a), a)
					.ok_or(Error::ParseError)?;
				let type_name = remove_by_pattern!(&mut inner, AST::Type(a), a)
					.unwrap_or(Type {
						raw: RawType::Inferred,
						mutable: false,
					});
				let name = remove_by_pattern!(
					&mut inner,
					AST::Subexpr(Subexpr::Variable(a)),
					a
				)
				.ok_or(Error::ParseError)?;
				AST::Declaration(Declaration {
					name,
					type_name,
					value,
				})
			}
			Rule::assignment => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				// Name, maybe operator and value
				if !(inner.len() == 2 || inner.len() == 3) {
					bail!(Error::Internal);
				}

				let rhs = remove_by_pattern!(&mut inner, AST::Subexpr(a), a)
					.ok_or(Error::ParseError)?;
				let op = remove_by_pattern!(&mut inner, AST::RawToken(a), a);
				let name = remove_by_pattern!(
					&mut inner,
					AST::Subexpr(Subexpr::Variable(a)),
					a
				)
				.ok_or(Error::ParseError)?;

				let value = if let Some(op) = op {
					let mut op_tokens = GeneratedParser::parse(
						Rule::operator,
						op.as_str(),
					)?
					.map(MaybeParsed::try_from)
					.collect::<Result<SmallVec<[MaybeParsed; 1]>>>()?;
					if op_tokens.len() != 1 {
						bail!(Error::Internal);
					}
					let op = op_tokens
						.pop()
						.map(MaybeParsed::try_from)
						.expect("Length checked above")?
						.operator()
						.ok_or(Error::ParseError)?;
					Subexpr::BinOp(BinOp {
						lhs: Box::new(Subexpr::Variable(name.clone())),
						op,
						rhs: Box::new(rhs),
					})
				} else {
					rhs
				};

				AST::Assignment(Assignment { name, value })
			}
			Rule::expr => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				if inner.len() != 1 {
					bail!(Error::Internal);
				}
				inner.pop().unwrap()
			}
			Rule::subexpr => {
				let mut tokens = pair
					.into_inner()
					.map(MaybeParsed::try_from)
					.collect::<Result<SmallVec<[MaybeParsed; 4]>>>()?;
				/*
				 *	Order of operations:
				 *	.
				 *	^
				 *	¬ -x
				 *	× /
				 *	+ - δ
				 *	Λ V ⊕
				 */
				while let Some(_idx) = tokens
					.iter()
					.position(|x| matches!(x, MaybeParsed::Operator(Op::Dot)))
				{
					todo!("Struct fields are not implemented yet");
				}
				while let Some(idx) = tokens
					.iter()
					// todo: Unary minus
					.position(|x| matches!(x, MaybeParsed::Operator(Op::Exp)))
				{
					replace_bin_op(&mut tokens, idx)?;
				}
				while let Some(idx) = tokens
					.iter()
					.position(|x| matches!(x, MaybeParsed::Operator(Op::Not)))
				{
					todo!()
				}
				while let Some(idx) = tokens.iter().position(|x| {
					matches!(x, MaybeParsed::Operator(Op::Times | Op::Div))
				}) {
					replace_bin_op(&mut tokens, idx)?;
				}
				while let Some(idx) = tokens.iter().position(|x| {
					matches!(
						x,
						MaybeParsed::Operator(
							Op::Plus | Op::Minus | Op::Delta
						)
					)
				}) {
					replace_bin_op(&mut tokens, idx)?;
				}
				while let Some(idx) = tokens.iter().position(|x| {
					matches!(
						x,
						MaybeParsed::Operator(Op::And | Op::Or | Op::Xor)
					)
				}) {
					replace_bin_op(&mut tokens, idx)?;
				}
				if !matches!(tokens.as_slice(), [MaybeParsed::Parsed(_)]) {
					bail!(Error::ParseError);
				}
				tokens.remove(0).parsed().ok_or(Error::Internal)?
			}
			Rule::fn_keyword => AST::RawToken(pair.as_str().into()),
			Rule::argument => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				let type_name = remove_by_pattern!(&mut inner, AST::Type(a), a)
					.ok_or(Error::ParseError)?;
				let name = remove_by_pattern!(
					&mut inner,
					AST::Subexpr(Subexpr::Variable(a)),
					a
				)
				.ok_or(Error::ParseError)?;
				AST::Argument(Argument { name, type_name })
			}
			Rule::function_definition => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				// Reverse order to reduce moves in the SmallVec
				let block = remove_by_pattern!(&mut inner, AST::Block(a), a)
					.ok_or(Error::ParseError)?;
				let return_type =
					remove_by_pattern!(&mut inner, AST::RawType(a), a)
						.unwrap_or(RawType::Unit);
				let arguments = {
					let mut args = SmallVec::new();
					while let Some(arg) =
						remove_by_pattern!(&mut inner, AST::Argument(a), a)
					{
						args.push(arg);
					}
					args
				};
				let name = remove_by_pattern!(
					&mut inner,
					AST::Subexpr(Subexpr::Variable(a)),
					a
				)
				.ok_or(Error::ParseError)?;
				let fn_keyword =
					remove_by_pattern!(&mut inner, AST::RawToken(a), a)
						.ok_or(Error::ParseError)?;
				if matches!(fn_keyword.as_str(), "\\") {
					log::warn!("\\ used over idiomatic λ");
				}
				AST::Function(Function {
					name,
					arguments,
					return_type,
					block,
				})
			}
			Rule::block => {
				let inner = pair
					.into_inner()
					.map(|pair| {
						AST::try_from(pair).and_then(|node| match node {
							AST::Expr(e) => Ok(e),
							AST::Subexpr(s) => Ok(Expr::Subexpr(s)),
							AST::Declaration(d) => {
								Ok(Expr::Declaration(d))
							}
							AST::Assignment(a) => {
								Ok(Expr::Assignment(a))
							}
							_ => bail!(Error::ParseError),
						})
					})
					.collect::<Result<Vec<Expr>>>()?;
				AST::Block(inner)
			}
			Rule::if_expr => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 3]>>>()?;
				if inner.len() != 3 {
					bail!(Error::ParseError);
				}
				let else_block = {
					let ast = inner.pop().expect("Length checked above");
					match ast {
						AST::Block(v) => Ok(v),
						AST::Subexpr(a @ Subexpr::IfExpr(_)) => {
							Ok(vec![Expr::Subexpr(a)])
						}
						AST::IfExpr(else_if) => Ok(vec![Expr::Subexpr(
							Subexpr::IfExpr(else_if),
						)]),
						_ => Err(Error::ParseError),
					}
				}?;
				let then_block = inner
					.pop()
					.expect("Length checked above")
					.block()
					.ok_or(Error::ParseError)?;
				let condition = inner
					.pop()
					.expect("Length checked above")
					.subexpr()
					.ok_or(Error::ParseError)?;
				AST::Subexpr(Subexpr::IfExpr(IfExpr {
					condition: Box::new(condition),
					lhs: then_block,
					rhs: else_block,
				}))
			}
			_ => AST::RawToken(pair.as_str().into()),
		};
		Ok(res)
	}
}

pub fn order(pairs: Pairs<Rule>) -> Result<Vec<TopLevelConstruct>> {
	pairs.map(|pair| AST::try_from(pair).and_then(TopLevelConstruct::try_from))
		.collect()
}
