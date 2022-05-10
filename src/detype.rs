use std::collections::HashMap;

use anyhow::{bail, Result};
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{
		Argument, Assignment as OrderedAssignment, BinOp as OrderedBinOp,
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
	pub arguments: Vec<SmallString>,
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

pub fn detype_block(block: &[OrderedExpr], ctx: &mut Context) -> Result<(Vec<Expr>, bool)> {
	let mut inner_ctx = type_check::copy_for_inner_scope(ctx);
	let mut last = true;
	let mut vec = Vec::with_capacity(block.len());
	for line in block.iter() {
		let (line, res) = detype_expr(line, &mut inner_ctx)?;
		vec.push(line);
		last = res;
	}
	Ok((vec, last))
}

pub fn detype_subexpr(subexpr: &OrderedSubexpr, ctx: &mut Context) -> Result<(Subexpr, bool)> {
	let res = match subexpr {
		OrderedSubexpr::BinOp(OrderedBinOp { lhs, op, rhs }) => {
			let (lhs, left_float) = detype_subexpr(lhs, ctx)?;
			let (rhs, right_float) = detype_subexpr(rhs, ctx)?;
			if left_float != right_float {
				log::error!(
					"Internal [{}]: Left and right aren't of same floatiness\n\
					{lhs:?} {rhs:?}",
					line!()
				);
				bail!(Error::Internal);
			}
			let op = match (op, left_float) {
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
			let res = Subexpr::BinOp(BinOp {
				lhs: Box::new(lhs),
				op,
				rhs: Box::new(rhs),
			});
			(res, left_float)
		}
		OrderedSubexpr::IfExpr(OrderedIfExpr {
			condition,
			lhs,
			rhs,
		}) => {
			let (condition, cond_float) = detype_subexpr(condition, ctx)?;
			let (lhs, left_float) = detype_block(lhs, ctx)?;
			let (rhs, right_float) = detype_block(rhs, ctx)?;
			if left_float != right_float || cond_float {
				log::error!(
					"Internal [{}]: Incorrect floatiness in typechecked context\n\
					{cond_float} {left_float} {right_float}",
					line!()
				);
				bail!(Error::Internal);
			}
			let res = Subexpr::IfExpr(IfExpr {
				condition: Box::new(condition),
				lhs,
				rhs,
			});
			(res, left_float)
		}
		OrderedSubexpr::Block(block) => {
			let (res, is_float) = detype_block(block, ctx)?;
			(Subexpr::Block(res), is_float)
		}
		OrderedSubexpr::Literal(l) => match l {
			OrderedLiteral::Integer(l) => (Subexpr::Literal(*l), false),
			OrderedLiteral::Float(f) => (Subexpr::Literal(f.to_bits()), true),
			OrderedLiteral::Boolean(b) => (Subexpr::Literal(*b as u64), false),
			OrderedLiteral::Unit => (Subexpr::Unit, false),
		},
		OrderedSubexpr::Variable(name) => {
			let res = Subexpr::Variable(name.clone());
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
			let is_float = raw == RawType::Real;
			(res, is_float)
		}
		OrderedSubexpr::Tuple(_) => todo!("I should probably get rid of tuples here"),
		OrderedSubexpr::FunctionCall(OrderedFunctionCall {
			function_name,
			arguments,
		}) => {
			let arguments = arguments
				.iter()
				.map(|arg| detype_subexpr(arg, ctx).map(|(a, _)| a))
				.collect::<Result<Vec<_>>>()?;
			let res = Subexpr::FunctionCall(FunctionCall {
				function_name: function_name.clone(),
				arguments,
			});
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
			let is_float = raw == RawType::Real;
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
) -> Result<(Declaration, bool)> {
	let (value, is_float) = detype_subexpr(value, ctx)?;
	let res = Declaration {
		name: name.clone(),
		value,
	};
	ctx.insert(
		name.clone(),
		(TypeRecord::Variable(type_name.clone()), true),
	);
	Ok((res, is_float))
}

pub fn detype_assignment(
	OrderedAssignment { name, value }: &OrderedAssignment,
	ctx: &mut Context,
) -> Result<(Assignment, bool)> {
	let (value, is_float) = detype_subexpr(value, ctx)?;
	let res = Assignment {
		name: name.clone(),
		value,
	};
	Ok((res, is_float))
}

pub fn detype_expr(expr: &OrderedExpr, ctx: &mut Context) -> Result<(Expr, bool)> {
	let res = match expr {
		OrderedExpr::Subexpr(s) => {
			let (subexpr, is_float) = detype_subexpr(s, ctx)?;
			(Expr::Subexpr(subexpr), is_float)
		}
		OrderedExpr::Declaration(d) => {
			let (decl, is_float) = detype_declaration(d, ctx)?;
			(Expr::Declaration(decl), is_float)
		}
		OrderedExpr::Assignment(a) => {
			let (assignment, is_float) = detype_assignment(a, ctx)?;
			(Expr::Assignment(assignment), is_float)
		}
	};
	Ok(res)
}

pub fn detype(exprs: &[OrderedTopLevelConstruct]) -> Result<Vec<TopLevelConstruct>> {
	let mut ctx = exprs
		.iter()
		.filter_map(|tlc| match tlc {
			OrderedTopLevelConstruct::Declaration(OrderedDeclaration {
				name,
				type_name,
				..
			}) => Some((
				name.clone(),
				(TypeRecord::Variable(type_name.clone()), true),
			)),
			_ => None,
		})
		.collect::<HashMap<_, _>>();

	exprs.iter()
		.map(|expr| {
			let res = match expr {
				OrderedTopLevelConstruct::Function(OrderedFunction {
					name,
					arguments,
					block,
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
					let (block, _) = detype_block(block, &mut inner_ctx)?;
					TopLevelConstruct::Function(Function {
						name: name.clone(),
						arguments: arguments
							.iter()
							.map(|Argument { name, .. }| name.clone())
							.collect(),
						block,
					})
				}
				OrderedTopLevelConstruct::Declaration(OrderedDeclaration {
					name,
					value,
					..
				}) => {
					let (value, _) = detype_subexpr(value, &mut ctx)?;
					TopLevelConstruct::Declaration(Declaration {
						name: name.clone(),
						value,
					})
				}
			};
			Ok(res)
		})
		.collect::<Result<Vec<_>>>()
}
