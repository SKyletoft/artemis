use anyhow::{bail, Result};
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{
		Assignment as OrderedAssignment, BinOp as OrderedBinOp,
		Declaration as OrderedDeclaration, Expr as OrderedExpr,
		Function as OrderedFunctions, Function as OrderedFunction,
		FunctionCall as OrderedFunctionCall, IfExpr as OrderedIfExpr,
		Literal as OrderedLiteral, Op as OrderedOp, RawType, Subexpr as OrderedSubexpr,
		TopLevelConstruct as OrderedTopLevelConstruct,
	},
	type_check::{self, Context, TypeRecord},
};

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: SmallString,
	pub arguments: usize,
	pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub name: SmallString,
	pub value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub name: SmallString,
	pub value: Subexpr,
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
	Literal(u64),
	Unit,
	Variable(SmallString),
	Tuple(Vec<Subexpr>),
	FunctionCall(FunctionCall),
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

fn is_floating(subexpr: &OrderedSubexpr, ctx: &mut Context) -> Result<bool> {
	match subexpr {
		OrderedSubexpr::BinOp(OrderedBinOp { lhs, .. }) => is_floating(lhs, ctx),
		OrderedSubexpr::IfExpr(OrderedIfExpr { lhs, .. }) | OrderedSubexpr::Block(lhs) => {
			todo!()
		}
		OrderedSubexpr::Literal(OrderedLiteral::Float(_)) => Ok(true),
		OrderedSubexpr::Literal(_) => Ok(false),
		OrderedSubexpr::Variable(name) => {
			let raw = ctx
				.get(name)
				.ok_or_else(|| {
					log::error!("Internal [{}]: Undefined variable in already checked context", line!());
					Error::Internal
				})?
				.0
				.clone()
				.variable()
				.ok_or_else(|| {
					log::error!("Internal [{}]: Variable was function in already checked context", line!());
					Error::Internal
				})?
				.raw;
			Ok(raw == RawType::Real)
		}
		OrderedSubexpr::FunctionCall(OrderedFunctionCall { function_name, .. }) => {
			let raw = ctx
				.get(function_name)
				.ok_or_else(|| {
					log::error!("Internal [{}]: Undefined function in already checked context", line!());
					Error::Internal
				})?
				.0
				.clone()
				.function()
				.ok_or_else(|| {
					log::error!("Internal [{}]: Function was variable in already checked context", line!());
					Error::Internal
				})?
				.return_type;
			Ok(raw == RawType::Real)
		}
		OrderedSubexpr::Tuple(_) => {
			log::error!("Type error [{}]: Operation on entire tuple", line!());
			bail!(Error::TypeError);
		}
	}
}

pub fn detype_subexpr(subexpr: &OrderedSubexpr, ctx: &mut Context) -> Result<Subexpr> {
	let res = match subexpr {
		OrderedSubexpr::BinOp(OrderedBinOp { lhs, op, rhs }) => {
			let is_float = is_floating(lhs, ctx)?;
			let lhs = detype_subexpr(lhs, ctx)?;
			let rhs = detype_subexpr(rhs, ctx)?;
			let op = match (op, is_float) {
				(OrderedOp::Plus, true) => Op::FPlus,
				(OrderedOp::Plus, false) => Op::Plus,
				(OrderedOp::Minus, true) => Op::FMinus,
				(OrderedOp::Minus, false) => Op::Minus,
				(OrderedOp::Delta, true) => Op::FDelta,
				(OrderedOp::Delta, false) => Op::Delta,
				(OrderedOp::Times, true) => Op::FTimes,
				(OrderedOp::Times, false) => Op::Times,
				(OrderedOp::Div, true) => Op::FDiv,
				(OrderedOp::Div, false) => Op::Div,
				(OrderedOp::Exp, true) => Op::FExp,
				(OrderedOp::Exp, false) => Op::Exp,
				(OrderedOp::Not, _) => Op::Not,
				(OrderedOp::And, _) => Op::And,
				(OrderedOp::Or, _) => Op::Or,
				(OrderedOp::Xor, _) => Op::Xor,
				(OrderedOp::Dot, _) => Op::Dot,
			};
			Subexpr::BinOp(BinOp {
				lhs: Box::new(lhs),
				op,
				rhs: Box::new(rhs),
			})
		}
		OrderedSubexpr::IfExpr(_) => todo!(),
		OrderedSubexpr::Block(_) => todo!(),
		OrderedSubexpr::Literal(l) => match l {
			OrderedLiteral::Integer(l) => Subexpr::Literal(*l),
			OrderedLiteral::Float(f) => Subexpr::Literal(f.to_bits()),
			OrderedLiteral::Boolean(b) => Subexpr::Literal(*b as u64),
			OrderedLiteral::Unit => Subexpr::Unit,
		},
		OrderedSubexpr::Variable(v) => Subexpr::Variable(v.clone()),
		OrderedSubexpr::Tuple(t) => todo!("I should probably get rid of tuples here"),
		OrderedSubexpr::FunctionCall(_) => todo!(),
	};
	Ok(res)
}

pub fn detype_declaration(OrderedDeclaration { name, type_name, value }: &OrderedDeclaration, ctx: &mut Context) -> Result<Declaration> {
	let mut inner_ctx = type_check::copy_for_inner_scope(ctx);
	let value = detype_subexpr(value, &mut inner_ctx)?;
	ctx.insert(name.clone(), (TypeRecord::Variable(type_name.clone()), true));
	Ok(Declaration {name: name.clone(), value})
}

pub fn detype_expr(expr: &OrderedExpr, ctx: &mut Context) -> Result<Expr> {
	let res = match expr {
		OrderedExpr::Subexpr(s) => Expr::Subexpr(detype_subexpr(s, ctx)?),
		OrderedExpr::Declaration(d) => Expr::Declaration(detype_declaration(d, ctx)?),
		OrderedExpr::Assignment(a) => todo!(),
	};
	Ok(res)
}
