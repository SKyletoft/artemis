use std::mem;

use anyhow::{bail, Result};
use derive_more::From;
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
	pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub name: SmallString,
	pub type_name: Type,
	pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub name: SmallString,
	pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub type_name: Type,
	pub name: SmallString,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub function_name: SmallString,
	pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Variantly, From)]
pub enum Expr {
	Term(Term),
	Declaration(Declaration),
	Assignment(Assignment),
}

impl TryFrom<AST> for Expr {
	type Error = Error;

	fn try_from(ast: AST) -> Result<Self, Error> {
		let res = match ast {
			AST::Term(s) => Expr::Term(s),
			AST::Declaration(d) => *d.value,
			AST::Assignment(a) => *a.value,
			_ => return Err(Error::ParseError(line!())),
		};

		Ok(res)
	}
}

impl TryFrom<&AST> for Expr {
	type Error = Error;

	fn try_from(ast: &AST) -> Result<Self, Error> {
		let res = match ast {
			AST::Term(s) => Expr::Term(s.clone()),
			AST::Declaration(d) => *d.value.clone(),
			AST::Assignment(a) => *a.value.clone(),
			_ => return Err(Error::ParseError(line!())),
		};

		Ok(res)
	}
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Term {
	BinOp(BinOp),
	UnOp(UnOp),
	IfExpr(IfExpr),
	Block(Block),
	Literal(Literal),
	Variable(SmallString),
	Tuple(Vec<Expr>),
	FunctionCall(FunctionCall),
	Expr(Box<Expr>),
}

impl TryFrom<AST> for Term {
	type Error = Error;

	fn try_from(value: AST) -> Result<Self, Self::Error> {
		let res = match value {
			AST::Declaration(d) => Term::Expr(Box::new(Expr::Declaration(d))),
			AST::Assignment(a) => Term::Expr(Box::new(Expr::Assignment(a))),
			AST::Expr(e) => Term::Expr(Box::new(e)),
			AST::Term(t) => t,
			AST::IfExpr(i) => Term::IfExpr(i),
			AST::Literal(l) => Term::Literal(l),
			AST::Block(b) => Term::Block(b),
			_ => return Err(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

#[derive(Debug, Clone, PartialEq, Variantly, From)]
pub enum Literal {
	Integer(u64),
	Float(f64),
	Boolean(bool),
	Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnOp {
	pub op: Op,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
	pub lhs: Box<Expr>,
	pub op: Op,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	pub condition: Box<Expr>,
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
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

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum RawType {
	Integer,
	Natural,
	Real,
	IntegerLiteral,
	Boolean,
	Unit,
	#[variantly(rename = "StructType")]
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

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Variantly, From)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}

impl TryFrom<AST> for TopLevelConstruct {
	type Error = anyhow::Error;

	fn try_from(value: AST) -> Result<Self, Self::Error> {
		match value.clone() {
			AST::Declaration(d) => Ok(d.into()),
			AST::Function(f) => Ok(f.into()),
			_ => {
				log::error!("Illegal construct at top level: {value:?}");
				bail!(Error::ParseError(line!()))
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Variantly, From)]
pub enum AST {
	Declaration(Declaration),
	Assignment(Assignment),
	Function(Function),
	Argument(Argument),
	Expr(Expr),
	Term(Term),
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

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Variantly)]
enum MaybeParsed {
	Parsed(AST),
	Operator(Op),
	Empty,
}

impl From<Term> for Result<MaybeParsed> {
	fn from(t: Term) -> Self {
		Ok(MaybeParsed::Parsed(AST::Term(t)))
	}
}

impl<'a> TryFrom<Pair<'a, Rule>> for MaybeParsed {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'a, Rule>) -> Result<MaybeParsed, Self::Error> {
		log::trace!(
			"[{}]: {:#?}\n{:#?}\n{:#?}",
			line!(),
			pair.as_rule(),
			pair.as_str(),
			&pair
		);
		let rule = pair.as_rule();
		let mut inner = pair.clone().into_inner();
		match rule {
			Rule::tuple => {
				let tuple = inner
					.map(|pair| {
						let expr = AST::try_from(pair)?
							.expr()
							.ok_or(Error::ParseError(line!()))?;
						Ok(expr)
					})
					.collect::<Result<Vec<_>>>()?;
				Ok(MaybeParsed::Parsed(AST::Term(Term::Tuple(tuple))))
			}
			Rule::block => {
				let block = inner
					.map(|pair| {
						let ast = AST::try_from(pair)?;
						let expr = ast
							.expr()
							.ok_or(Error::ParseError(line!()))?;
						Ok(expr)
					})
					.collect::<Result<Block>>()?;
				Ok(MaybeParsed::Parsed(AST::Block(block)))
			}
			// Parentheses and tuples
			Rule::term => {
				inner.next()
					.and_then(|pair| {
						let ast = AST::try_from(pair.clone());
						log::trace!(
							"[{}]: {:#?} → {:#?}",
							line!(),
							&pair.as_str(),
							&ast
						);
						let res: Term = ast.ok()?.try_into().ok()?;
						Some(res)
					})
					.ok_or(Error::ParseError(line!()))?
					.into()
			}
			Rule::if_expr => Term::IfExpr(
				inner.next()
					.and_then(|pair| AST::try_from(pair).ok()?.if_expr())
					.ok_or(Error::ParseError(line!()))?,
			)
			.into(),
			Rule::function_call => {
				let mut inner_2 = inner
					.next()
					.expect("There should be exactly one thing?")
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				// There is always at least the name of the function being called
				if inner_2.is_empty() {
					bail!(Error::Internal(line!()));
				}
				let function_name = inner_2
					.remove(0)
					.term()
					.and_then(Term::variable)
					.ok_or(Error::Internal(line!()))?;
				let arguments = inner_2
					.into_iter()
					.map(AST::expr)
					.collect::<Option<Vec<_>>>()
					.ok_or(Error::ParseError(line!()))?;
				Ok(MaybeParsed::Parsed(AST::Term(Term::FunctionCall(
					FunctionCall {
						function_name,
						arguments,
					},
				))))
			}
			Rule::var_name => Term::Variable(inner.as_str().into()).into(),
			Rule::unary_operator => {
				let inner_op = inner
					.next()
					.expect("Rule should always contain exactly one subrule")
					.as_rule();
				let op = match inner_op {
					Rule::minus => Op::Minus,
					Rule::not => Op::Not,
					_ => bail!(Error::Internal(line!())),
				};
				Ok(MaybeParsed::Operator(op))
			}
			Rule::binary_operator => {
				let inner_op = inner
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
					Rule::and => Op::And,
					Rule::or => Op::Or,
					Rule::xor => Op::Xor,
					_ => bail!(Error::Internal(line!())),
				};
				Ok(MaybeParsed::Operator(op))
			}

			_ => {
				let ast = AST::try_from(pair)?;
				Ok(MaybeParsed::Parsed(ast))
			}
		}
	}
}

fn replace_bin_op(tokens: &mut SmallVec<[MaybeParsed; 4]>, idx: usize) -> Result<()> {
	if idx < 1 {
		bail!(Error::Internal(line!()));
	}
	let op = tokens
		.remove(idx)
		.operator()
		.ok_or(Error::Internal(line!()))?;
	let lhs = mem::replace(&mut tokens[idx - 1], MaybeParsed::Empty)
		.parsed()
		.and_then(AST::term)
		.ok_or(Error::ParseError(line!()))?;
	let rhs = tokens
		.remove(idx)
		.parsed()
		.and_then(AST::term)
		.ok_or(Error::ParseError(line!()))?;
	let res = Term::BinOp(BinOp {
		lhs: Box::new(Expr::Term(lhs)),
		op,
		rhs: Box::new(Expr::Term(rhs)),
	});
	tokens[idx - 1] = MaybeParsed::Parsed(AST::Term(res));
	Ok(())
}

impl<'a> TryFrom<Pair<'a, Rule>> for AST {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
		let res = match pair.as_rule() {
			Rule::var_name => AST::Term(Term::Variable(pair.as_str().into())),
			Rule::type_name => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				let mutable = remove_by_pattern!(&mut inner, AST::RawToken(a), a)
					.map(|s| s == "mut") // Because it could be `ref` too
					.unwrap_or(false);
				let raw = remove_by_pattern!(&mut inner, AST::RawType(a), a)
					.ok_or(Error::ParseError(line!()))?;
				AST::Type(Type { mutable, raw })
			}
			Rule::mutable => AST::RawToken("mut".into()),
			Rule::raw_type => {
				// `raw_type` rule always contains exactly one thing
				let inner =
					pair.into_inner().next().ok_or(Error::Internal(line!()))?;
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
							.ok_or(Error::ParseError(line!()))?;
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
					bail!(Error::Internal(line!()));
				}

				let expr = inner.pop().expect("Length checked above").try_into()?;
				let type_name = remove_by_pattern!(&mut inner, AST::Type(a), a)
					.unwrap_or(Type {
						raw: RawType::Inferred,
						mutable: false,
					});
				let name = remove_by_pattern!(
					&mut inner,
					AST::Term(Term::Variable(a)),
					a
				)
				.ok_or(Error::ParseError(line!()))?;
				AST::Declaration(Declaration {
					name,
					type_name,
					value: Box::new(expr),
				})
			}
			Rule::assignment => {
				log::trace!("[{}]: {:#?}", line!(), &pair);
				let inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				// Name, maybe operator and value
				if !(inner.len() == 2 || inner.len() == 3) {
					bail!(Error::Internal(line!()));
				}

				let (name, value) = match inner.as_slice() {
					[AST::Term(Term::Variable(name)), val] => {
						(name.clone(), Box::new(val.try_into()?))
					}
					[AST::Term(Term::Variable(name)), AST::RawToken(t), val] => {
						let rhs = Box::new(val.try_into()?);
						let op = GeneratedParser::parse(
							Rule::binary_operator,
							t.as_str(),
						)?
						.map(MaybeParsed::try_from)
						.collect::<Result<SmallVec<[MaybeParsed; 1]>>>()?
						.pop()
						.map(MaybeParsed::try_from)
						.expect("Length checked above")?
						.operator()
						.ok_or(Error::ParseError(line!()))?;

						(
							name.clone(),
							Box::new(Expr::Term(Term::BinOp(BinOp {
								lhs: Box::new(Expr::Term(
									Term::Variable(
										name.clone(),
									),
								)),
								op,
								rhs,
							}))),
						)
					}
					_ => bail!(Error::ParseError(line!())),
				};
				AST::Assignment(Assignment { name, value })
			}
			// expr here is what used to be parentheses and can be treated as a pure expression
			Rule::expr | Rule::term => {
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
				while let Some(_idx) = tokens
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
					bail!(Error::ParseError(line!()));
				}
				tokens.pop()
					.expect("Checked in pattern above")
					.parsed()
					.expect("Checked in pattern above")
			}
			Rule::fn_keyword => AST::RawToken(pair.as_str().into()),
			Rule::argument => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				let type_name = remove_by_pattern!(&mut inner, AST::Type(a), a)
					.ok_or(Error::ParseError(line!()))?;
				let name = remove_by_pattern!(
					&mut inner,
					AST::Term(Term::Variable(a)),
					a
				)
				.ok_or(Error::ParseError(line!()))?;
				AST::Argument(Argument { name, type_name })
			}
			Rule::function_definition => {
				let mut inner = pair
					.into_inner()
					.map(AST::try_from)
					.collect::<Result<SmallVec<[AST; 8]>>>()?;
				// Reverse order to reduce moves in the SmallVec
				let last = inner.pop();
				let expr = match last {
					Some(AST::Term(s)) => Expr::Term(s),
					Some(AST::Declaration(d)) => *d.value,
					Some(AST::Assignment(a)) => *a.value,
					_ => bail!(Error::ParseError(line!())),
				};
				let return_type =
					remove_by_pattern!(&mut inner, AST::RawType(a), a)
						.unwrap_or(RawType::Unit);
				let arguments = {
					let mut args = SmallVec::new();
					while let Some(arg) =
						remove_by_pattern!(&mut inner, AST::Argument(a), a)
					{
						args.insert(0, arg);
					}
					args
				};
				let name = remove_by_pattern!(
					&mut inner,
					AST::Term(Term::Variable(a)),
					a
				)
				.ok_or(Error::ParseError(line!()))?;
				let fn_keyword =
					remove_by_pattern!(&mut inner, AST::RawToken(a), a)
						.ok_or(Error::ParseError(line!()))?;
				if matches!(fn_keyword.as_str(), "\\") {
					log::warn!("\\ used over idiomatic λ");
				}
				AST::Function(Function {
					name,
					arguments,
					return_type,
					expr,
				})
			}
			Rule::block => {
				let inner = pair
					.into_inner()
					.map(|pair| {
						AST::try_from(pair).and_then(|node| match node {
							AST::Expr(e) => Ok(e),
							AST::Term(s) => Ok(Expr::Term(s)),
							AST::Declaration(d) => {
								Ok(Expr::Declaration(d))
							}
							AST::Assignment(a) => {
								Ok(Expr::Assignment(a))
							}
							_ => bail!(Error::ParseError(line!())),
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
				log::trace!("[{}]: {:#?}", line!(), &inner);
				if inner.len() != 3 {
					bail!(Error::ParseError(line!()));
				}
				let else_block =
					inner.pop().expect("Length checked above").try_into()?;
				let then_block =
					inner.pop().expect("Length checked above").try_into()?;
				let condition =
					inner.pop().expect("Length checked above").try_into()?;
				AST::Term(Term::IfExpr(IfExpr {
					condition: Box::new(condition),
					lhs: Box::new(then_block),
					rhs: Box::new(else_block),
				}))
			}
			Rule::float => Term::Literal(Literal::Float(pair.as_str().parse()?)).into(),
			Rule::integer => {
				// Try to parse as unsigned, on fail, retry as signed
				// instead and bitcast to unsigned because our IR
				// only supports unsigned
				let val = pair.as_str().parse::<u64>().or_else(
					|_| -> Result<u64> {
						Ok(pair.as_str().parse::<i64>()? as u64)
					},
				)?;
				Term::Literal(Literal::Integer(val)).into()
			}
			Rule::boolean => {
				Term::Literal(Literal::Boolean(pair.as_str().parse()?)).into()
			}
			Rule::unit => Term::Literal(Literal::Unit).into(),
			Rule::function_call => {
				let mut inner = pair.into_inner().map(AST::try_from);
				let function_name = inner
					.next()
					.ok_or(Error::ParseError(line!()))??
					.term()
					.ok_or(Error::ParseError(line!()))?
					.variable()
					.ok_or(Error::ParseError(line!()))?;
				let arguments = inner
					.map(|r| r.map(Expr::try_from))
					.collect::<Result<Result<Vec<_>, Error>>>()??;
				Term::FunctionCall(FunctionCall {
					function_name,
					arguments,
				})
				.into()
			}
			Rule::tuple => {
				let items = pair.into_inner()
					.map(|pair| {
						let res = Expr::try_from(AST::try_from(pair)?)?;
						Ok(res)
					})
					.collect::<Result<Vec<_>>>()?;
				Term::Tuple(items).into()
			}
			_ => AST::RawToken(pair.as_str().into()),
		};
		Ok(res)
	}
}

pub fn order(mut pairs: Pairs<Rule>) -> Result<Vec<TopLevelConstruct>> {
	let top = pairs.next().expect("GeneratedParser works on Rule::top");
	if pairs.next().is_some() || top.as_rule() != Rule::top {
		panic!("GeneratedParser works on Rule::top?")
	}
	top.into_inner()
		.filter(|r| r.as_rule() != Rule::EOI)
		.map(|pair| AST::try_from(pair).and_then(TopLevelConstruct::try_from))
		.collect()
}
