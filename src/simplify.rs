use std::{collections::HashMap, mem};

use anyhow::{bail, Result};
use derive_more::{Add, AddAssign, From};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	detype::{Assignment, BinOp, Declaration, Expr, FunctionCall, IfExpr, Op, Subexpr},
	error::Error,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
#[repr(transparent)]
pub struct Register(usize);

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
#[repr(transparent)]
pub struct BlockId(usize);

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockEnd {
	Return,
	One(BlockId),
	Two(Source, BlockId, BlockId),
}

impl Default for BlockEnd {
	fn default() -> Self {
		BlockEnd::Return
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Block {
	pub id: BlockId,
	pub intro: (), // Phi nodes
	pub block: SmallVec<[SimpleExpression; 4]>,
	pub out: BlockEnd,
}

#[derive(Debug, Copy, Clone, PartialEq, Variantly)]
pub enum Source {
	Register(Register),
	Value(u64),
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

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleExpression {
	BinOp(SimpleBinOp),
	UnOp(SimpleUnOp),
	FunctionCall(SimpleFunctionCall),
}

pub fn simplify_subexpr(
	subexpr: &Subexpr,
	current: &mut Block,
	blocks: &mut Vec<Option<Block>>,
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
			let cond = simplify_subexpr(condition, current, blocks, ctx)?;
			let cond_idx = blocks.len();
			blocks.push(None); // Reserve a space for the block we came from
			let completed_block = mem::replace(current, Block::default());
			let existing_context = mem::replace(&mut ctx.variables, HashMap::new());

			let (then_idx, then_source) = simplify_exprs(lhs, current, blocks, ctx)?;
			let then_code = mem::replace(current, Block::default());
			blocks.push(Some(then_code));

			let else_idx = blocks.len();
			blocks.push(None);
			let else_source = simplify_exprs(rhs, current, blocks, ctx)?;
			let else_code = mem::replace(current, Block::default());

			let block = Block {
				id: cond_idx.into(),
				intro: todo!(),
				block: completed_block,
				out: BlockEnd::Two(todo!().into(), then_source, else_source),
			};
			todo!()
		}
		Subexpr::Block(_) => todo!(),
		Subexpr::Literal(v) => Source::Value(*v),
		Subexpr::Variable(v) => *ctx.variables.get(v).ok_or_else(|| {
			log::error!("Internal [{}]: Use of undeclared variable", line!());
			Error::Internal
		})?,
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
	blocks: &mut Vec<Option<Block>>,
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

pub fn simplify_exprs(
	exprs: &[Expr],
	current: &mut Block,
	blocks: &mut Vec<Option<Block>>,
	ctx: &mut Context,
) -> Result<(BlockId, Source)> {
	todo!()
}
