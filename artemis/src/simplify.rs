use std::{mem, slice};

use air::ir::{
	Block, BlockEnd, BlockId, Context, PhiEdge, PhiNode, SSAConstruct, SimpleBinOp,
	SimpleExpression, SimpleFunctionCall, SimpleOp, Source, SimpleUnOp,
};
use anyhow::{bail, Result};
use rayon::prelude::*;
use smallvec::SmallVec;

use crate::{
	detype2_types::{
		BinOp, Declaration, Expr, Function, FunctionCall, IfExpr, Op, Term,
		TopLevelConstruct, UnOp,
	},
	error::Error,
};

pub fn simplify_term(
	term: &Term,
	current: &mut Block,
	blocks: &mut Vec<Block>,
	ctx: &mut Context,
) -> Result<Source> {
	let res = match term {
		Term::Expr(expr) => simplify_expr(expr, current, blocks, ctx)?,
		Term::UnOp(UnOp { op, rhs }) => {
			let right = simplify_expr(rhs.as_ref(), current, blocks, ctx)?;
			let target = ctx.next_register();
			let simple_operator = match op {
				Op::Plus => SimpleOp::Add,
				Op::FPlus => SimpleOp::FAdd,
				Op::Minus => SimpleOp::Sub,
				Op::FMinus => SimpleOp::FSub,
				Op::Times => SimpleOp::Mul,
				Op::FTimes => SimpleOp::FMul,
				Op::Div => SimpleOp::Div,
				Op::UDiv => SimpleOp::UDiv,
				Op::FDiv => SimpleOp::FDiv,
				Op::And => SimpleOp::And,
				Op::Or => SimpleOp::Or,
				Op::Xor => SimpleOp::Xor,
				// Op::Dot => todo!("Structs aren't implemented yet"),
				Op::Delta | Op::FDelta | Op::Exp | Op::FExp | Op::Not => {
					unreachable!("Separate cases above")
				}

				Op::GT => todo!(),
				Op::FGT => todo!(),
				Op::UGT => todo!(),
				Op::GTE => todo!(),
				Op::FGTE => todo!(),
				Op::UGTE => todo!(),
				Op::LT => todo!(),
				Op::FLT => todo!(),
				Op::ULT => todo!(),
				Op::LTE => todo!(),
				Op::FLTE => todo!(),
				Op::ULTE => todo!(),
				Op::Eq => todo!(),
				Op::FEq => todo!(),
				Op::Neq => todo!(),
				Op::FNeq => todo!(),

				Op::LoadMut => SimpleOp::LoadMut,
				Op::LoadConst => SimpleOp::LoadConst,
				Op::StoreExclusive => SimpleOp::StoreExclusive,
				Op::StoreVolatile => SimpleOp::StoreVolatile,
			};
			let this = SimpleUnOp {
				target,
				op: simple_operator,
				rhs: right,
			};
			current.block.push(SimpleExpression::UnOp(this));
			Source::Register(target)
		},
		Term::BinOp(BinOp { op: Op::Not, .. }) => {
			log::error!("Internal [{}]: Not as binop", line!());
			bail!(Error::Internal(line!()));
		}
		Term::BinOp(BinOp {
			lhs: _,
			op: Op::Delta,
			rhs: _,
		}) => todo!("transform into sub + abs"),
		Term::BinOp(BinOp {
			lhs: _,
			op: Op::FDelta,
			rhs: _,
		}) => todo!("transform into sub + abs"),
		Term::BinOp(BinOp {
			lhs: _,
			op: Op::Exp,
			rhs: _,
		}) => todo!("function call?"),
		Term::BinOp(BinOp { lhs, op, rhs }) => {
			let left = simplify_expr(lhs.as_ref(), current, blocks, ctx)?;
			let right = simplify_expr(rhs.as_ref(), current, blocks, ctx)?;
			let target = ctx.next_register();
			let simple_operator = match op {
				Op::Plus => SimpleOp::Add,
				Op::FPlus => SimpleOp::FAdd,
				Op::Minus => SimpleOp::Sub,
				Op::FMinus => SimpleOp::FSub,
				Op::Times => SimpleOp::Mul,
				Op::FTimes => SimpleOp::FMul,
				Op::Div => SimpleOp::Div,
				Op::UDiv => SimpleOp::UDiv,
				Op::FDiv => SimpleOp::FDiv,
				Op::And => SimpleOp::And,
				Op::Or => SimpleOp::Or,
				Op::Xor => SimpleOp::Xor,
				// Op::Dot => todo!("Structs aren't implemented yet"),
				Op::Delta | Op::FDelta | Op::Exp | Op::FExp | Op::Not => {
					unreachable!("Separate cases above")
				}

				Op::GT => todo!(),
				Op::FGT => todo!(),
				Op::UGT => todo!(),
				Op::GTE => todo!(),
				Op::FGTE => todo!(),
				Op::UGTE => todo!(),
				Op::LT => todo!(),
				Op::FLT => todo!(),
				Op::ULT => todo!(),
				Op::LTE => todo!(),
				Op::FLTE => todo!(),
				Op::ULTE => todo!(),
				Op::Eq => todo!(),
				Op::FEq => todo!(),
				Op::Neq => todo!(),
				Op::FNeq => todo!(),

				Op::LoadMut => SimpleOp::LoadMut,
				Op::LoadConst => SimpleOp::LoadConst,
				Op::StoreExclusive => SimpleOp::StoreExclusive,
				Op::StoreVolatile => SimpleOp::StoreVolatile,
			};
			let this = SimpleBinOp {
				target,
				op: simple_operator,
				lhs: left,
				rhs: right,
			};
			current.block.push(SimpleExpression::BinOp(this));
			Source::Register(target)
		}
		Term::IfExpr(IfExpr {
			condition,
			lhs,
			rhs,
		}) => {
			let cond_reg = simplify_expr(condition.as_ref(), current, blocks, ctx)?;
			let last_block = mem::take(current);
			let curr_idx = blocks.len();
			blocks.push(last_block);
			let old_variables = ctx.variables.clone();

			let then_start_id = blocks.len();
			let (then_end_id, then_source) = simplify_exprs(
				slice::from_ref(lhs.as_ref()),
				current,
				blocks,
				ctx,
			)?;
			let then_variables =
				mem::replace(&mut ctx.variables, old_variables.clone());

			let else_start_id = blocks.len();
			let (else_end_id, else_source) = simplify_exprs(
				slice::from_ref(rhs.as_ref()),
				current,
				blocks,
				ctx,
			)?;
			let else_variables =
				mem::replace(&mut ctx.variables, old_variables.clone());

			// Only merge new context when needed
			if then_variables != else_variables {
				old_variables
					.keys()
					.map(|key| {
						(key, &then_variables[key], &else_variables[key])
					})
					.filter(|(_, l, r)| l != r)
					.for_each(|(key, then_var_src, else_var_src)| {
						let target = ctx.next_register();
						ctx.variables.insert(
							key.clone(),
							Source::Register(target),
						);
						current.intro.push(PhiNode {
							target,
							value: [
								PhiEdge {
									from: then_end_id,
									value: then_var_src.clone(),
								},
								PhiEdge {
									from: else_end_id,
									value: else_var_src.clone(),
								},
							],
						});
					});
			}

			let phi_target = ctx.next_register();
			current.intro.push(PhiNode {
				target: phi_target,
				value: [
					PhiEdge {
						from: then_end_id,
						value: then_source,
					},
					PhiEdge {
						from: else_end_id,
						value: else_source,
					},
				],
			});

			blocks[curr_idx].out =
				BlockEnd::Two(cond_reg, then_start_id.into(), else_start_id.into());
			// Invariant: All branches must make sure the source comes first in `blocks`
			let next = BlockId::from(blocks.len());
			let then_id: usize = then_end_id.into();
			let else_id: usize = else_end_id.into();
			blocks[then_id].out = BlockEnd::One(next);
			blocks[else_id].out = BlockEnd::One(next);

			Source::Register(phi_target)
		}
		Term::Block(exprs) => {
			let mut last = Source::Value(0);
			for expr in exprs.iter() {
				last = simplify_expr(expr, current, blocks, ctx)?;
			}
			last
		}
		Term::Literal(v) => Source::Value(*v),
		Term::Variable(v) => match ctx.variables.get(v) {
			Some(r) => anyhow::Ok(r.clone()),
			None => todo!("Handle loading of globals"),
		}?,
		Term::Tuple(_) => todo!(
			"Should tuples even exist at this stage? Should they be a stack thing?\n\
			These are design questions, not implementation"
		),
		Term::FunctionCall(FunctionCall {
			function_name,
			arguments,
		}) => {
			let args = arguments
				.iter()
				.map(|expr| simplify_expr(expr, current, blocks, ctx))
				.collect::<Result<SmallVec<_>>>()?;
			let target = ctx.next_register();
			let call = SimpleFunctionCall {
				target,
				function: function_name.clone(),
				args,
			};
			current.block.push(SimpleExpression::FunctionCall(call));
			Source::Register(target)
		}
		Term::FunctionObjectCall(_) => todo!(),
		Term::Unit => todo!(),
	};
	Ok(res)
}

pub fn simplify_expr(
	expr: &Expr,
	current: &mut Block,
	blocks: &mut Vec<Block>,
	ctx: &mut Context,
) -> Result<Source> {
	match expr {
		Expr::Term(s) => simplify_term(s, current, blocks, ctx),
		Expr::Declaration(Declaration { name, value })
		| Expr::Assignment(Declaration { name, value }) => {
			let mut last = Err(Error::EmptyAssignment(line!()));
			for (n, v) in name.iter().cloned().zip(value.iter()) {
				let source = simplify_expr(v, current, blocks, ctx)?;
				ctx.variables.insert(n, source.clone());
				last = Ok(source);
			}
			Ok(last?) // anyhow type hack
		}
		Expr::Function(f) => Ok(Source::LinkerValue(f.name.clone())),
	}
}

/// Returns the **ending** block and source
pub fn simplify_exprs(
	exprs: &[Expr],
	current: &mut Block,
	blocks: &mut Vec<Block>,
	ctx: &mut Context,
) -> Result<(BlockId, Source)> {
	let mut last_source = Source::Value(0);
	for expr in exprs.iter() {
		last_source = simplify_expr(expr, current, blocks, ctx)?;
	}
	current.out = BlockEnd::Return(last_source.clone());
	let final_block = mem::take(current);
	blocks.push(final_block);
	let final_id = blocks.len() - 1;
	Ok((final_id.into(), last_source))
}

pub fn simplify(tlcs: &[TopLevelConstruct]) -> Result<Vec<SSAConstruct>> {
	tlcs.par_iter()
		.map(|tlc| match tlc {
			TopLevelConstruct::Function(Function {
				name,
				arguments,
				expr,
			}) => {
				let mut current = Block::default();
				let mut blocks = Vec::default();
				let mut ctx = Context {
					variables: arguments
						.iter()
						.enumerate()
						.map(|(idx, name)| {
							(name.clone(), Source::Register(idx.into()))
						})
						.collect(),
					next_register: arguments.len().into(),
				};
				let _ = simplify_exprs(
					slice::from_ref(expr),
					&mut current,
					&mut blocks,
					&mut ctx,
				)?;
				let ret = SSAConstruct::Function {
					name: name.clone(),
					blocks,
					args: arguments.len() as u64,
				};
				Ok(ret)
			}
			TopLevelConstruct::Declaration(_) => todo!(),
		})
		.collect()
}
