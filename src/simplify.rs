use std::{collections::HashMap, fmt, mem};

use anyhow::{bail, Result};
use derive_more::{Add, AddAssign, From, Into};
use smallvec::{smallvec, SmallVec};
use variantly::Variantly;

use crate::{
	detype::{
		Assignment, BinOp, Declaration, Expr, Function, FunctionCall, IfExpr, Op, Subexpr,
		TopLevelConstruct,
	},
	error::Error,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(
	Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From, Into,
)]
#[repr(transparent)]
pub struct Register(usize);

impl fmt::Display for Register {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "${}", self.0)
	}
}

#[derive(
	Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From, Into,
)]
#[repr(transparent)]
pub struct BlockId(usize);

impl fmt::Display for BlockId {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "#{}", self.0)
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
	pub variables: HashMap<SmallString, Source>,
	pub next_register: Register,
}

impl Context {
	fn next(&mut self) -> Register {
		let next = self.next_register;
		self.next_register += Register(1);
		next
	}
}

#[derive(Clone, Copy, PartialEq)]
pub enum BlockEnd {
	Return,
	One(BlockId),
	Two(Source, BlockId, BlockId),
}

impl fmt::Debug for BlockEnd {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Return => write!(f, "ret"),
			Self::One(target) => write!(f, "{target}"),
			Self::Two(reg, left, right) => write!(f, "{reg} ? {left}, {right}"),
		}
	}
}

impl Default for BlockEnd {
	fn default() -> Self {
		BlockEnd::Return
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Block {
	pub intro: SmallVec<[PhiNode; 2]>, // Hardcoded 2 because I think this might be able to be an array or just 2 values, but I'm not 100% yet
	pub block: SmallVec<[SimpleExpression; 4]>,
	pub out: BlockEnd,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Variantly)]
pub enum Source {
	Register(Register),
	Value(u64),
}

impl fmt::Display for Source {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Source::Register(Register(r)) => write!(f, "${r}"),
			Source::Value(v) => write!(f, "{v}"),
		}
	}
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SimpleOp {
	Add,
	Sub,
	Abs,
	Mul,
	Div,
	FAdd,
	FSub,
	FAbs,
	FMul,
	FDiv,
	And,
	Or,
	Xor,
	Not,
}

impl fmt::Display for SimpleOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			SimpleOp::Add => write!(f, "+"),
			SimpleOp::Sub => write!(f, "-"),
			SimpleOp::Abs => write!(f, "±"),
			SimpleOp::Mul => write!(f, "×"),
			SimpleOp::Div => write!(f, "÷"),
			SimpleOp::FAdd => write!(f, "+."),
			SimpleOp::FSub => write!(f, "-."),
			SimpleOp::FAbs => write!(f, "±."),
			SimpleOp::FMul => write!(f, "×."),
			SimpleOp::FDiv => write!(f, "÷."),
			SimpleOp::And => write!(f, "Λ"),
			SimpleOp::Or => write!(f, "V"),
			SimpleOp::Xor => write!(f, "⊕"),
			SimpleOp::Not => write!(f, "¬"),
		}
	}
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SimpleBinOp {
	pub target: Register,
	pub op: SimpleOp,
	pub lhs: Source,
	pub rhs: Source,
}

impl SimpleBinOp {
	fn is_same(&self, other: &Self) -> bool {
		self.op == other.op
			&& ((self.lhs == other.lhs && self.rhs == other.rhs)
				|| (self.lhs == other.rhs && self.rhs == other.lhs))
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleUnOp {
	pub target: Register,
	pub lhs: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleFunctionCall {
	pub target: Register,
	pub function: SmallString,
	pub args: SmallVec<[Source; 4]>,
}

#[derive(Clone, PartialEq)]
pub enum SimpleExpression {
	BinOp(SimpleBinOp),
	UnOp(SimpleUnOp),
	FunctionCall(SimpleFunctionCall),
}

impl fmt::Debug for SimpleExpression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			SimpleExpression::BinOp(SimpleBinOp {
				target,
				op,
				lhs,
				rhs,
			}) => write!(f, "{target} ← {lhs} {op} {rhs}"),
			SimpleExpression::UnOp(SimpleUnOp { target, lhs }) => {
				write!(f, "{target} ← {lhs}")
			}
			SimpleExpression::FunctionCall(SimpleFunctionCall {
				target,
				function,
				args,
			}) => write!(f, "{target} ← {function}{args:?}"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum SSAConstruct {
	Function {
		name: SmallString,
		blocks: Vec<Block>,
	},
	Variable {
		name: SmallString,
		value: u64,
	},
	ImmediateExpression {
		name: SmallString,
		value: Vec<Block>,
	},
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PhiEdge {
	from: BlockId,
	value: Source,
}

impl fmt::Debug for PhiEdge {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let PhiEdge { from, value } = self;
		write!(f, "{from}:{value}")
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct PhiNode {
	target: Register,
	value: SmallVec<[PhiEdge; 2]>,
}

impl fmt::Debug for PhiNode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let PhiNode { target, value } = self;
		write!(f, "{target} ← φ{value:?}")
	}
}

pub fn simplify_subexpr(
	subexpr: &Subexpr,
	current: &mut Block,
	blocks: &mut Vec<Block>,
	ctx: &mut Context,
) -> Result<Source> {
	let res = match subexpr {
		Subexpr::BinOp(BinOp { op: Op::Not, .. }) => {
			log::error!("Internal [{}]: Not as binop", line!());
			bail!(Error::Internal);
		}
		Subexpr::BinOp(BinOp {
			lhs,
			op: Op::Delta,
			rhs,
		}) => todo!("transform into sub + abs"),
		Subexpr::BinOp(BinOp {
			lhs,
			op: Op::FDelta,
			rhs,
		}) => todo!("transform into sub + abs"),
		Subexpr::BinOp(BinOp {
			lhs,
			op: Op::Exp,
			rhs,
		}) => todo!("function call?"),
		Subexpr::BinOp(BinOp { lhs, op, rhs }) => {
			let left = simplify_subexpr(lhs, current, blocks, ctx)?;
			let right = simplify_subexpr(rhs, current, blocks, ctx)?;
			let target = ctx.next();
			let simple_operator = match op {
				Op::Plus => SimpleOp::Add,
				Op::FPlus => SimpleOp::FAdd,
				Op::Minus => SimpleOp::Sub,
				Op::FMinus => SimpleOp::FSub,
				Op::Times => SimpleOp::Mul,
				Op::FTimes => SimpleOp::FMul,
				Op::Div => SimpleOp::Div,
				Op::FDiv => SimpleOp::FDiv,
				Op::And => SimpleOp::And,
				Op::Or => SimpleOp::Or,
				Op::Xor => SimpleOp::Xor,
				Op::Dot => todo!("Structs aren't implemented yet"),
				Op::Delta | Op::FDelta | Op::Exp | Op::FExp | Op::Not => {
					unreachable!("Separate cases above")
				}
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
		Subexpr::IfExpr(IfExpr {
			condition,
			lhs,
			rhs,
		}) => {
			let cond_reg = simplify_subexpr(condition, current, blocks, ctx)?;
			let last_block = mem::take(current);
			let curr_idx = blocks.len();
			blocks.push(last_block);
			let old_variables = ctx.variables.clone();

			let then_start_id = blocks.len();
			let (then_end_id, then_source) = simplify_exprs(lhs, current, blocks, ctx)?;
			let then_variables =
				mem::replace(&mut ctx.variables, old_variables.clone());

			let else_start_id = blocks.len();
			let (else_end_id, else_source) = simplify_exprs(rhs, current, blocks, ctx)?;
			let else_variables =
				mem::replace(&mut ctx.variables, old_variables.clone());

			// Only merge new context when needed
			if then_variables != else_variables {
				old_variables
					.keys()
					.map(|key| (then_variables[key], else_variables[key]))
					.filter(|(l, r)| l != r)
					.for_each(|(then_var_src, else_var_src)| {
						current.intro.push(PhiNode {
							target: ctx.next(),
							value: smallvec![
								PhiEdge {
									from: then_end_id,
									value: then_var_src
								},
								PhiEdge {
									from: else_end_id,
									value: else_var_src
								}
							],
						});
					});
			}

			let phi_target = ctx.next();
			current.intro.push(PhiNode {
				target: phi_target,
				value: smallvec![
					PhiEdge {
						from: then_end_id,
						value: then_source
					},
					PhiEdge {
						from: else_end_id,
						value: else_source
					}
				],
			});

			blocks[curr_idx].out =
				BlockEnd::Two(cond_reg, then_start_id.into(), else_start_id.into());
			// Invariant: All branches must make sure the source comes first in `blocks`
			let next = BlockId(blocks.len());
			let then_id: usize = then_end_id.into();
			let else_id: usize = else_end_id.into();
			blocks[then_id].out = BlockEnd::One(next);
			blocks[else_id].out = BlockEnd::One(next);

			Source::Register(phi_target)
		}
		Subexpr::Block(_) => todo!(),
		Subexpr::Literal(v) => Source::Value(*v),
		Subexpr::Variable(v) => match ctx.variables.get(v) {
			Some(&r) => anyhow::Ok(r),
			None => todo!("Handle loading of globals"),
		}?,
		Subexpr::Tuple(_) => {
			todo!("Should tuples even exist at this stage? Should they be a stack thing? These are design questions, not implementation")
		}
		Subexpr::FunctionCall(FunctionCall {
			function_name,
			arguments,
		}) => {
			let args = arguments
				.iter()
				.map(|s| simplify_subexpr(s, current, blocks, ctx))
				.collect::<Result<SmallVec<_>>>()?;
			let target = ctx.next();
			let call = SimpleFunctionCall {
				target,
				function: function_name.clone(),
				args,
			};
			current.block.push(SimpleExpression::FunctionCall(call));
			Source::Register(target)
		}
		Subexpr::Unit => todo!(),
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
		Expr::Subexpr(s) => simplify_subexpr(s, current, blocks, ctx),
		Expr::Declaration(Declaration { name, value })
		| Expr::Assignment(Assignment { name, value }) => {
			let source = simplify_subexpr(value, current, blocks, ctx)?;
			ctx.variables.insert(name.clone(), source);
			Ok(source)
		}
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
	let final_block = mem::take(current);
	blocks.push(final_block);
	let final_id = blocks.len() - 1;
	Ok((final_id.into(), last_source))
}

pub fn simplify(tlcs: &[TopLevelConstruct]) -> Result<Vec<SSAConstruct>> {
	tlcs.iter()
		.map(|tlc| match tlc {
			TopLevelConstruct::Function(Function {
				name,
				arguments,
				block,
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
				let (..) =
					simplify_exprs(block, &mut current, &mut blocks, &mut ctx)?;
				let ret = SSAConstruct::Function {
					name: name.clone(),
					blocks,
				};
				Ok(ret)
			}
			TopLevelConstruct::Declaration(_) => todo!(),
		})
		.collect()
}
