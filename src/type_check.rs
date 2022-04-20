use std::collections::HashMap;

use anyhow::{bail, Result};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{
		Argument, Assignment, BinOp, Declaration, Expr, Function, FunctionCall, IfExpr, Literal,
		Op, RawType, Subexpr, TopLevelConstruct, Type,
	},
};

type SmallString = smallstr::SmallString<[u8; 16]>;
type Context = HashMap<SmallString, TypeRecord>;

#[derive(Debug, Clone, PartialEq)]
struct FunctionType {
	return_type: RawType,
	arguments: SmallVec<[Type; 4]>,
}

#[derive(Debug, Clone, PartialEq, Variantly)]
enum TypeRecord {
	Variable(Type),
	Function(FunctionType),
}

fn type_of_expr(expr: &Expr, ctx: &Context) -> Result<Type> {
	match expr {
		Expr::Subexpr(s) => type_of_subexpr(s, ctx),
		Expr::Declaration(Declaration { type_name, .. }) => Ok(type_name.clone()),
		Expr::Assignment(Assignment { name, .. }) => {
			let res = ctx
				.get(name)
				.ok_or_else(|| {
					log::error!("Internal: Supposedly checked variable is undefined");
					Error::Internal
				})?
				.clone()
				.variable()
				.ok_or_else(|| {
					log::error!("Internal: Supposedly checked variable was a function");
					Error::Internal
				})?;
			Ok(res)
		}
	}
}

fn type_of_subexpr(subexpr: &Subexpr, ctx: &Context) -> Result<Type> {
	// Trusts the check functions and only checks the left hand side in expressions that require lhs and rhs to be of the same type
	match subexpr {
		Subexpr::BinOp(BinOp { lhs, .. }) => type_of_subexpr(lhs.as_ref(), ctx),
		Subexpr::IfExpr(IfExpr { lhs: slice, .. }) | Subexpr::Block(slice) => {
			type_of_expr(&slice[slice.len() - 1], ctx)
		}
		Subexpr::Literal(l) => match l {
			Literal::Integer(_) => Ok(Type::Const(RawType::Integer)),
			Literal::Float(_) => Ok(Type::Const(RawType::Real)),
			Literal::Boolean(_) => Ok(Type::Const(RawType::Boolean)),
			Literal::Unit => Ok(Type::Const(RawType::Unit)),
		},
		Subexpr::Tuple(v) => Ok(Type::Const(RawType::Tuple(
			v.iter()
				.map(|s| type_of_subexpr(s, ctx))
				.collect::<Result<_>>()?,
		))),
		Subexpr::Variable(name) => {
			let res = ctx
				.get(name)
				.ok_or_else(|| {
					log::error!(
						"Internal: Supposedly checked variable is undefined\n\
						{name:?} {ctx:?}"
					);
					Error::Internal
				})?
				.clone()
				.variable()
				.ok_or_else(|| {
					log::error!(
						"Internal: Supposedly checked variable was a function\n\
						{name:?} {ctx:?}"
					);
					Error::Internal
				})?;
			Ok(res)
		}
		Subexpr::FunctionCall(FunctionCall { function_name, .. }) => {
			let res = ctx
				.get(function_name)
				.ok_or_else(|| {
					log::error!(
						"Internal: Supposedly checked function is undefined\n\
						{function_name:?} {ctx:?}"
					);
					Error::Internal
				})?
				.clone()
				.function()
				.ok_or_else(|| {
					log::error!(
						"Internal: Supposedly checked function was a variable\n\
						{function_name:?} {ctx:?}"
					);
					Error::Internal
				})?
				.return_type;
			Ok(Type::Const(res))
		}
	}
}

pub fn check_program(top_level: &[TopLevelConstruct]) -> Result<()> {
	let mut ctx = HashMap::new();
	for branch in top_level.iter() {
		match branch {
			TopLevelConstruct::Function(Function {
				name,
				arguments,
				return_type,
				block,
			}) => {
				let mut inner_ctx = ctx.clone();
				for Argument { type_name, name } in arguments.into_iter() {
					inner_ctx.insert(name.clone(), TypeRecord::Variable(type_name.clone()));
				}
				for line in block.iter() {
					check_expr(line, &mut inner_ctx)?;
				}
				let actual_type = type_of_expr(&block[block.len() - 1], &inner_ctx)?;
				if actual_type.raw() != return_type {
					log::error!(
						"Type mismatch in function return type: {return_type:?} \
						 {actual_type:?}\n{branch:?}\nλ{name} {arguments:?} → {return_type:?}"
					);
					bail!(Error::TypeError);
				}
				ctx.insert(
					name.clone(),
					TypeRecord::Function(FunctionType {
						return_type: return_type.clone(),
						arguments: arguments.iter().map(|arg| arg.type_name.clone()).collect(),
					}),
				);
			}
			TopLevelConstruct::Declaration(decl) => check_declaration(decl, &mut ctx)?,
		}
	}
	Ok(())
}

fn check_block(block: &[Expr], ctx: &Context) -> Result<()> {
	let mut inner_ctx = ctx.clone();
	for line in block.iter() {
		check_expr(line, &mut inner_ctx)?;
	}
	Ok(())
}

fn check_declaration(
	Declaration {
		name,
		type_name,
		value,
	}: &Declaration,
	ctx: &mut Context,
) -> Result<()> {
	check_subexpr(value, ctx)?;
	let actual_type = type_of_subexpr(value, &ctx)?;
	let correct_type = match type_name {
		Type::Const(RawType::Inferred) => actual_type.clone(),
		_ => type_name.clone(),
	};
	if actual_type.raw() != correct_type.raw() {
		log::error!("Type mismatch: {type_name:?} {actual_type:?}");
		bail!(Error::TypeError);
	}
	ctx.insert(name.clone(), TypeRecord::Variable(correct_type));
	Ok(())
}

fn check_assignment(Assignment { name, value }: &Assignment, ctx: &mut Context) -> Result<()> {
	check_subexpr(value, ctx)?;
	let actual_type = type_of_subexpr(value, ctx)?;
	let recorded_type = ctx.get(name).ok_or_else(|| {
		log::error!("Use of undeclared variable: {name}");
		Error::TypeError
	})?;
	if &TypeRecord::Variable(actual_type.clone()) != recorded_type {
		log::error!("Type mismatch:  {recorded_type:?} {actual_type:?}");
		bail!(Error::TypeError);
	}
	Ok(())
}

fn check_expr(expr: &Expr, ctx: &mut Context) -> Result<()> {
	match expr {
		Expr::Subexpr(s) => check_subexpr(s, ctx),
		Expr::Declaration(d) => check_declaration(d, ctx),
		Expr::Assignment(a) => check_assignment(a, ctx),
	}
}

fn check_subexpr(expr: &Subexpr, ctx: &mut Context) -> Result<()> {
	match expr {
		Subexpr::BinOp(BinOp { lhs, op, rhs }) => {
			check_subexpr(lhs, ctx)?;
			check_subexpr(rhs, ctx)?;
			let lhs_type = type_of_subexpr(lhs, ctx)?;
			let rhs_type = type_of_subexpr(rhs, ctx)?;
			let eq = match op {
				Op::Plus | Op::Minus | Op::Times | Op::Div | Op::Exp => {
					lhs_type.raw() == rhs_type.raw()
						&& matches!(
							lhs_type.raw(),
							RawType::Integer | RawType::Natural | RawType::Real
						)
				}
				Op::Delta => {
					lhs_type.raw() == rhs_type.raw() && lhs_type.raw() == &RawType::Natural
				}
				Op::And | Op::Or | Op::Xor => {
					lhs_type.raw() == rhs_type.raw() && lhs_type.raw() == &RawType::Boolean
				}
				_ => {
					log::error!("Internal: Unary operator in binop?\n{expr:?}");
					bail!(Error::Internal);
				}
			};
			if !eq {
				log::error!(
					"Type error: Mismatch in Binary Operator\n\
					{lhs:?}: {lhs_type:?} {op:?} {rhs:?}: {rhs_type:?}"
				);
				bail!(Error::TypeError);
			}
			Ok(())
		}
		Subexpr::IfExpr(IfExpr {
			condition,
			lhs,
			rhs,
		}) => {
			let cond_type = type_of_subexpr(condition, ctx)?;
			if cond_type.raw() != &RawType::Boolean {
				log::error!("Type error: Non boolean condition in if statement\n{condition:?}");
				bail!(Error::TypeError);
			}
			check_block(lhs, ctx)?;
			check_block(rhs, ctx)?;
			Ok(())
		}
		Subexpr::Block(block) => check_block(block, ctx),
		Subexpr::Tuple(tuple) => tuple
			.iter()
			.map(|s| check_subexpr(s, ctx))
			.collect::<Result<Vec<()>>>()
			.map(|_| ()),
		Subexpr::FunctionCall(FunctionCall {
			function_name,
			arguments,
		}) => {
			let expected_args = ctx
				.get(function_name)
				.ok_or_else(|| {
					log::error!(
						"Internal: Supposedly checked function is undefined\n\
						{function_name:?} {ctx:?}"
					);
					Error::Internal
				})?
				.clone()
				.function()
				.ok_or_else(|| {
					log::error!(
						"Internal: Supposedly checked function was a variable\n\
						{function_name:?} {ctx:?}"
					);
					Error::Internal
				})?
				.arguments;
			for (expected_arg, actual_arg) in expected_args.iter().zip(arguments.iter()) {
				let actual_type = type_of_subexpr(actual_arg, ctx)?;
				if expected_arg != &actual_type {
					log::error!(
						"Type mismatch in function arguments: {expected_arg:?} {actual_type:?}"
					);
					bail!(Error::TypeError);
				}
			}
			Ok(())
		}
		_ => Ok(()),
	}
}
