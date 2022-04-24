use std::collections::HashMap;

use anyhow::{bail, Result};
use derive_more::{Add, AddAssign, From};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{BinOp, FunctionCall, IfExpr, Literal, Op, Subexpr},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
pub struct Register(u32);

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
pub struct BlockId(u32);

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
	pub variables: HashMap<SmallString, Register>,
	pub next_register: Register,
}

impl Context {
	fn next(&mut self) -> Register {
		let next = self.next_register;
		self.next_register += Register(1);
		next
	}
}


#[derive(Debug, Copy, Clone, PartialEq, Variantly)]
pub enum Source {
	Register(Register),
	Integer(u64),
	Float(f64),
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
	out: &mut Vec<SimpleExpression>,
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
			op: Op::Exp,
			rhs,
		}) => todo!("function call?"),
		Subexpr::BinOp(BinOp { lhs, op, rhs }) => {
			let left = simplify_subexpr(lhs, out, ctx)?;
			let right = simplify_subexpr(rhs, out, ctx)?;
			let target = ctx.next();
			let simple_operator = match op {
				Op::Plus => SimpleOp::Add,
				Op::Minus => SimpleOp::Sub,
				Op::Times => SimpleOp::Mul,
				Op::Div => SimpleOp::Div,
				Op::And => SimpleOp::And,
				Op::Or => SimpleOp::Or,
				Op::Xor => SimpleOp::Xor,
				Op::Dot => todo!("Structs aren't implemented yet"),
				Op::Delta | Op::Exp | Op::Not => unreachable!(),
			};
			let this = SimpleBinOp {
				target,
				op: simple_operator,
				lhs: left,
				rhs: right,
			};
			out.push(SimpleExpression::BinOp(this));
			Source::Register(target)
		}
		Subexpr::IfExpr(IfExpr {
			condition,
			lhs,
			rhs,
		}) => todo!(),
		Subexpr::Block(_) => todo!(),
		Subexpr::Literal(v) => match v {
			Literal::Integer(i) => Source::Integer(*i),
			Literal::Float(f) => Source::Float(*f),
			Literal::Boolean(b) => Source::Integer(*b as u64),
			Literal::Unit => {
				log::error!("Internal [{}]: Use of unit value", line!());
				bail!(Error::Internal);
			}
		},
		Subexpr::Variable(v) => match ctx.variables.get(v) {
			Some(v) => Source::Register(*v),
			None => {
				log::error!("Internal [{}]: Use of undeclared variable", line!());
				bail!(Error::Internal);
			}
		},
		Subexpr::Tuple(_) => {
			todo!("Should tuples even exist at this stage? Should they be a stack thing? These are design questions, not implementation")
		}
		Subexpr::FunctionCall(FunctionCall {
			function_name,
			arguments,
		}) => {
			let args = arguments
				.iter()
				.map(|s| simplify_subexpr(s, out, ctx))
				.collect::<Result<SmallVec<_>>>()?;
			let target = ctx.next();
			let call = SimpleFunctionCall {
				target,
				function: function_name.clone(),
				args,
			};
			out.push(SimpleExpression::FunctionCall(call));
			Source::Register(target)
		}
	};
	Ok(res)
}
