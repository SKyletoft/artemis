use anyhow::{bail, Result};
use once_cell::sync::Lazy;
use smallvec::smallvec;
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{
		Argument, Assignment as OrderedAssignment, BinOp as OrderedBinOp,
		Declaration as OrderedDeclaration, Expr as OrderedExpr,
		Function as OrderedFunction, FunctionCall as OrderedFunctionCall,
		IfExpr as OrderedIfExpr, Literal as OrderedLiteral, Op as OrderedOp, RawType,
		Term as OrderedTerm, TopLevelConstruct as OrderedTopLevelConstruct,
	},
	type_check::{self, Context, FunctionType, TypeRecord},
};

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: SmallString,
	pub arguments: Vec<SmallString>,
	pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub name: SmallString,
	pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub name: SmallString,
	pub value: Box<Expr>,
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
	Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Term {
	BinOp(BinOp),
	UnOp(UnOp),
	IfExpr(IfExpr),
	Block(Block),
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

#[derive(Debug, Clone, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
	Signed,
	Unsigned,
	Floating,
}

pub fn detype_block(block: &[OrderedExpr], ctx: &mut Context) -> Result<(Vec<Expr>, Type)> {
	let mut inner_ctx = type_check::copy_for_inner_scope(ctx);
	let mut last = Type::Floating;
	let mut vec = Vec::with_capacity(block.len());
	for line in block.iter() {
		let (line, res) = detype_expr(line, &mut inner_ctx)?;
		vec.push(line);
		last = res;
	}
	Ok((vec, last))
}

pub fn detype_term(term: &OrderedTerm, ctx: &mut Context) -> Result<(Term, Type)> {
	let res = match term {
		OrderedTerm::UnOp(..) => todo!("unops"),
		OrderedTerm::Expr(expr) => match expr.as_ref() {
			OrderedExpr::Term(t) => {
				let (term, typ) = detype_term(t, ctx)?;
				(term, typ)
			}
			OrderedExpr::Declaration(d) => {
				let (decl, typ) = detype_declaration(d, ctx)?;
				(Term::Expr(Box::new(Expr::Declaration(decl))), typ)
			}
			OrderedExpr::Assignment(a) => {
				let (assign, typ) = detype_assignment(a, ctx)?;
				(Term::Expr(Box::new(Expr::Assignment(assign))), typ)
			}
			OrderedExpr::FunctionDeclaration(_) => todo!(),
			OrderedExpr::TypeAlias(_) => todo!(),
		},
		OrderedTerm::BinOp(OrderedBinOp { lhs, op, rhs }) => {
			let (lhs, left_float) = detype_expr(lhs.as_ref(), ctx)?;
			let (rhs, right_float) = detype_expr(rhs.as_ref(), ctx)?;
			if left_float != right_float {
				log::error!(
					"Internal [{}]: Left and right aren't of same floatiness\n\
					{lhs:?} {rhs:?}",
					line!()
				);
				bail!(Error::Internal(line!()));
			}
			let op = match (op, left_float) {
				(OrderedOp::Plus, Type::Floating) => Op::FPlus,
				(OrderedOp::Plus, _) => Op::Plus,
				(OrderedOp::Minus, Type::Floating) => Op::FMinus,
				(OrderedOp::Minus, _) => Op::Minus,
				(OrderedOp::Delta, Type::Floating) => Op::FDelta,
				(OrderedOp::Delta, _) => Op::Delta,
				(OrderedOp::Times, Type::Floating) => Op::FTimes,
				(OrderedOp::Times, _) => Op::Times,
				(OrderedOp::Div, Type::Floating) => Op::FDiv,
				(OrderedOp::Div, Type::Signed) => Op::Div,
				(OrderedOp::Div, Type::Unsigned) => Op::UDiv,
				(OrderedOp::Exp, Type::Floating) => Op::FExp,
				(OrderedOp::Exp, _) => Op::Exp,
				(OrderedOp::Not, _) => Op::Not,
				(OrderedOp::And, _) => Op::And,
				(OrderedOp::Or, _) => Op::Or,
				(OrderedOp::Xor, _) => Op::Xor,
				(OrderedOp::Dot, _) => Op::Dot,
			};
			let res = Term::BinOp(BinOp {
				lhs: Box::new(lhs),
				op,
				rhs: Box::new(rhs),
			});
			(res, left_float)
		}
		OrderedTerm::IfExpr(OrderedIfExpr {
			condition,
			lhs,
			rhs,
		}) => {
			let (condition, cond_float) = detype_expr(condition.as_ref(), ctx)?;
			let (lhs, left_float) = detype_expr(lhs.as_ref(), ctx)?;
			let (rhs, right_float) = detype_expr(rhs.as_ref(), ctx)?;
			if left_float != right_float || cond_float == Type::Floating {
				log::error!(
					"Internal [{}]: Incorrect floatiness in typechecked context\n\
					{cond_float:?} {left_float:?} {right_float:?}",
					line!()
				);
				bail!(Error::Internal(line!()));
			}
			let res = Term::IfExpr(IfExpr {
				condition: Box::new(condition),
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			});
			(res, left_float)
		}
		OrderedTerm::Block(block) => {
			let (res, is_float) = detype_block(block, ctx)?;
			(Term::Block(res), is_float)
		}
		OrderedTerm::Literal(l) => match l {
			OrderedLiteral::Float(f) => (Term::Literal(f.to_bits()), Type::Floating),
			OrderedLiteral::Boolean(b) => (Term::Literal(*b as u64), Type::Unsigned),
			OrderedLiteral::Unit => (Term::Unit, Type::Unsigned),
			OrderedLiteral::Integer(l) => (Term::Literal(*l), Type::Unsigned),
		},
		OrderedTerm::Variable(name) => {
			let res = Term::Variable(name.clone());
			let raw = ctx
				.get(name)
				.ok_or_else(|| {
					log::error!("Internal [{}]: Undefined variable in already checked context", line!());
					Error::Internal(line!())
				})?
				.0
				.clone()
				.variable()
				.ok_or_else(|| {
					log::error!("Internal [{}]: Variable was function in already checked context", line!());
					Error::Internal(line!())
				})?
				.raw;
			let is_float = if raw == RawType::Real {
				Type::Floating
			} else {
				Type::Unsigned
			};
			(res, is_float)
		}
		OrderedTerm::Tuple(_) => todo!("I should probably get rid of tuples here"),
		OrderedTerm::FunctionCall(OrderedFunctionCall {
			function_name,
			arguments,
		}) => {
			let arguments = arguments
				.iter()
				.map(|arg| detype_expr(arg, ctx).map(|(a, _)| a))
				.collect::<Result<Vec<_>>>()?;
			let res = Term::FunctionCall(FunctionCall {
				function_name: function_name.clone(),
				arguments,
			});
			let raw = ctx
				.get(function_name)
				.ok_or_else(|| {
					log::error!("Internal [{}]: Undefined function in already checked context", line!());
					Error::Internal(line!())
				})?
				.0
				.clone()
				.function()
				.ok_or_else(|| {
					log::error!("Internal [{}]: Function was variable in already checked context", line!());
					Error::Internal(line!())
				})?
				.return_type;
			let is_float = if raw == RawType::Real {
				Type::Floating
			} else {
				Type::Unsigned
			};
			(res, is_float)
		}
	};
	Ok(res)
}

pub fn detype_declaration(
	OrderedDeclaration {
		name,
		type_name,
		value,
	}: &OrderedDeclaration,
	ctx: &mut Context,
) -> Result<(Declaration, Type)> {
	let (value, typ) = detype_expr(value.as_ref(), ctx)?;
	let res = Declaration {
		name: name.clone(),
		value: Box::new(value),
	};
	ctx.insert(
		name.clone(),
		(TypeRecord::Variable(type_name.clone()), true),
	);
	Ok((res, typ))
}

pub fn detype_assignment(
	OrderedAssignment { name, value }: &OrderedAssignment,
	ctx: &mut Context,
) -> Result<(Assignment, Type)> {
	let (value, typ) = detype_expr(value.as_ref(), ctx)?;
	let res = Assignment {
		name: name.clone(),
		value: Box::new(value),
	};
	Ok((res, typ))
}

pub fn detype_expr(expr: &OrderedExpr, ctx: &mut Context) -> Result<(Expr, Type)> {
	let res = match expr {
		OrderedExpr::Term(s) => {
			let (term, is_float) = detype_term(s, ctx)?;
			(Expr::Term(term), is_float)
		}
		OrderedExpr::Declaration(d) => {
			let (decl, is_float) = detype_declaration(d, ctx)?;
			(Expr::Declaration(decl), is_float)
		}
		OrderedExpr::Assignment(a) => {
			let (assignment, is_float) = detype_assignment(a, ctx)?;
			(Expr::Assignment(assignment), is_float)
		}
		OrderedExpr::FunctionDeclaration(_) => todo!(),
		OrderedExpr::TypeAlias(_) => todo!(),
	};
	Ok(res)
}

// Implemented on reference so we don't have to clone thrown away fields
impl From<&OrderedTopLevelConstruct> for (SmallString, (TypeRecord, bool)) {
	fn from(tlc: &OrderedTopLevelConstruct) -> Self {
		match tlc {
			OrderedTopLevelConstruct::Declaration(OrderedDeclaration {
				name,
				type_name,
				..
			}) => (
				name.clone(),
				(TypeRecord::Variable(type_name.clone()), true),
			),
			OrderedTopLevelConstruct::Function(OrderedFunction {
				name,
				arguments,
				return_type,
				..
			}) => (
				name.clone(),
				(
					TypeRecord::Function(FunctionType {
						return_type: return_type.clone(),
						arguments: arguments
							.iter()
							.map(|Argument { type_name, .. }| {
								type_name.clone()
							})
							.collect(),
					}),
					true,
				),
			),
			OrderedTopLevelConstruct::TypeAlias(_) => todo!(),
		}
	}
}

pub(crate) static BUILTINS: Lazy<Vec<(SmallString, (TypeRecord, bool))>> = Lazy::new(|| {
	vec![
		(
			"print_n".into(),
			(
				TypeRecord::Function(FunctionType {
					return_type: RawType::Integer,
					arguments: smallvec![crate::ordered::Type {
						mutable: false,
						raw: RawType::Natural
					}],
				}),
				true,
			),
		),
		(
			"print_z".into(),
			(
				TypeRecord::Function(FunctionType {
					return_type: RawType::Integer,
					arguments: smallvec![crate::ordered::Type {
						mutable: false,
						raw: RawType::Integer
					}],
				}),
				true,
			),
		),
		(
			"print_b".into(),
			(
				TypeRecord::Function(FunctionType {
					return_type: RawType::Integer,
					arguments: smallvec![crate::ordered::Type {
						mutable: false,
						raw: RawType::Boolean
					}],
				}),
				true,
			),
		),
		(
			"print_r".into(),
			(
				TypeRecord::Function(FunctionType {
					return_type: RawType::Integer,
					arguments: smallvec![crate::ordered::Type {
						mutable: false,
						raw: RawType::Real
					}],
				}),
				true,
			),
		),
	]
});

pub fn detype(exprs: &[OrderedTopLevelConstruct]) -> Result<Vec<TopLevelConstruct>> {
	let mut ctx = exprs
		.iter()
		.map(<&OrderedTopLevelConstruct>::into)
		.chain(BUILTINS.iter().cloned())
		.collect::<Context>();

	exprs.iter()
		.map(|expr| {
			let res = match expr {
				OrderedTopLevelConstruct::Function(OrderedFunction {
					name,
					arguments,
					expr,
					..
				}) => {
					let mut inner_ctx = type_check::copy_for_inner_scope(&ctx);
					for Argument { type_name, name } in arguments.iter() {
						inner_ctx.insert(
							name.clone(),
							(
								TypeRecord::Variable(
									type_name.clone(),
								),
								true,
							),
						);
					}
					let (expr, _) = detype_expr(expr, &mut inner_ctx)?;
					TopLevelConstruct::Function(Function {
						name: name.clone(),
						arguments: arguments
							.iter()
							.map(|Argument { name, .. }| name.clone())
							.collect(),
						expr,
					})
				}
				OrderedTopLevelConstruct::Declaration(OrderedDeclaration {
					name,
					value,
					..
				}) => {
					let (value, _) = detype_expr(value.as_ref(), &mut ctx)?;
					TopLevelConstruct::Declaration(Declaration {
						name: name.clone(),
						value: Box::new(value),
					})
				}
				OrderedTopLevelConstruct::TypeAlias(_) => todo!(),
			};
			Ok(res)
		})
		.collect::<Result<Vec<_>>>()
}
