use std::collections::HashMap;

use anyhow::{bail, Result};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{
		Argument, Assignment, BinOp, Declaration, Expr, Function, FunctionCall, IfExpr,
		Literal, Op, RawType, Subexpr, TopLevelConstruct, Type,
	},
};

type SmallString = smallstr::SmallString<[u8; 16]>;
type Context = HashMap<SmallString, (TypeRecord, bool)>;

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

fn copy_for_inner_scope(ctx: &Context) -> Context {
	ctx.iter()
		.map(|(key, (typ, _))| (key.clone(), (typ.clone(), false)))
		.collect()
}

pub fn check_program(top_level: &mut [TopLevelConstruct]) -> Result<()> {
	let mut ctx = HashMap::new();
	for branch in top_level.iter_mut() {
		match branch {
			TopLevelConstruct::Function(fun) => {
				check_function(fun, &mut ctx)?;
			}
			TopLevelConstruct::Declaration(decl) => {
				check_declaration(decl, &mut ctx)?;
			}
		}
	}
	Ok(())
}

fn check_function(
	Function {
		name,
		arguments,
		return_type,
		block,
	}: &mut Function,
	ctx: &mut Context,
) -> Result<()> {
	let mut inner_ctx = copy_for_inner_scope(&ctx);
	for Argument { type_name, name } in arguments.into_iter() {
		inner_ctx.insert(
			name.clone(),
			(TypeRecord::Variable(type_name.clone()), true),
		);
	}
	for line in block.iter_mut() {
		check_expr(line, &mut inner_ctx)?;
	}
	let actual_type = {
		let last_statement = block.last_mut().ok_or_else(|| {
			log::error!("Internal: Block was empty? This should be a parse error");
			Error::Internal
		})?;
		let actual = check_expr(last_statement, &mut inner_ctx)?.raw;
		if actual != RawType::Inferred {
			actual
		} else if let Expr::Declaration(Declaration { name, .. }) = last_statement {
			if let Some((TypeRecord::Variable(Type { raw, .. }), _)) =
				inner_ctx.get(name)
			{
				raw.clone()
			} else {
				log::error!("Internal: Last statement in block wasn't recorded properly?");
				bail!(Error::Internal);
			}
		} else {
			log::error!("Internal: Inferred implies declaration at the end of a block");
			bail!(Error::Internal);
		}
	};
	if !actual_type.integer_equality(return_type) {
		log::error!(
			"Type mismatch in function return type [{}]: {return_type:?} \
						 {actual_type:?}\nλ{name} {arguments:?} → {return_type:?}",
			line!()
		);
		bail!(Error::TypeError);
	}
	ctx.insert(
		name.clone(),
		(
			TypeRecord::Function(FunctionType {
				return_type: return_type.clone(),
				arguments: arguments
					.iter()
					.map(|arg| arg.type_name.clone())
					.collect(),
			}),
			true,
		),
	);
	Ok(())
}

fn check_block(block: &mut [Expr], ctx: &Context) -> Result<Type> {
	let mut inner_ctx = copy_for_inner_scope(ctx);
	let mut last = Type {
		mutable: false,
		raw: RawType::Unit,
	};
	for line in block.iter_mut() {
		last = check_expr(line, &mut inner_ctx)?;
	}
	Ok(last)
}

fn check_declaration(
	Declaration {
		name,
		type_name,
		value,
	}: &mut Declaration,
	ctx: &mut Context,
) -> Result<Type> {
	if matches!(ctx.get(name), Some((_, true))) {
		log::error!(
			"Same scope shadowing [{}]: {name} already exists in this scope",
			line!()
		);
		bail!(Error::TypeError)
	}
	check_subexpr(value, ctx)?;
	let actual_type = check_subexpr(value, ctx)?;
	let correct_type = match type_name.raw {
		RawType::Inferred => actual_type.clone(),
		_ => type_name.clone(),
	};
	if !actual_type.raw.integer_equality(&correct_type.raw) {
		log::error!(
			"Type mismatch [{}]:\n{type_name:?}\n{actual_type:?}\n{correct_type:?}",
			line!()
		);
		bail!(Error::TypeError);
	}
	let maybe_defaulted = correct_type.default_int();
	ctx.insert(
		name.clone(),
		(TypeRecord::Variable(maybe_defaulted.clone()), true),
	);
	Ok(maybe_defaulted)
}

fn check_assignment(
	Assignment { name, value }: &mut Assignment,
	ctx: &mut Context,
) -> Result<Type> {
	check_subexpr(value, ctx)?;
	let actual_type = check_subexpr(value, ctx)?.raw;
	let recorded_type = ctx.get(name).ok_or_else(|| {
		log::error!("Use of undeclared variable [{}]: {name}", line!());
		Error::TypeError
	})?;
	if let (TypeRecord::Variable(Type { raw, mutable }), _) = recorded_type {
		if !raw.integer_equality(&actual_type) {
			log::error!(
				"Type mismatch [{}]: {recorded_type:?} {actual_type:?}",
				line!()
			);
			bail!(Error::TypeError);
		}
		if !*mutable {
			log::error!("Write to const [{}]: {name}: {recorded_type:?}", line!());
			bail!(Error::TypeError);
		}
	}
	let res = ctx
		.get(name)
		.ok_or_else(|| {
			log::error!(
				"Internal [{}]: Supposedly checked variable is undefined",
				line!()
			);
			Error::Internal
		})?
		.clone()
		.0
		.variable()
		.ok_or_else(|| {
			log::error!(
				"Internal [{}]: Supposedly checked variable was a function",
				line!()
			);
			Error::Internal
		})?;
	Ok(res)
}

fn check_expr(expr: &mut Expr, ctx: &mut Context) -> Result<Type> {
	match expr {
		Expr::Subexpr(s) => check_subexpr(s, ctx),
		Expr::Declaration(d) => check_declaration(d, ctx),
		Expr::Assignment(a) => check_assignment(a, ctx),
	}
}

fn check_subexpr(expr: &mut Subexpr, ctx: &mut Context) -> Result<Type> {
	let res = match expr {
		Subexpr::BinOp(BinOp { lhs, op, rhs }) => {
			let lhs_type = check_subexpr(lhs, ctx)?;
			let rhs_type = check_subexpr(rhs, ctx)?;
			let eq =
				match op {
					Op::Plus | Op::Minus | Op::Times | Op::Div | Op::Exp => {
						lhs_type.raw.integer_equality(&rhs_type.raw)
							&& matches!(
								lhs_type.raw,
								RawType::Integer
									| RawType::Natural | RawType::Real
									| RawType::IntegerLiteral
							)
					}
					Op::Delta => {
						lhs_type.raw.integer_equality(&rhs_type.raw)
							&& (lhs_type.raw == RawType::Natural
								|| rhs_type.raw == RawType::Natural)
					}
					Op::And | Op::Or | Op::Xor => {
						lhs_type.raw == rhs_type.raw
							&& lhs_type.raw == RawType::Boolean
					}
					_ => {
						log::error!("Internal [{}]: Unary operator in binop?\n{expr:?}", line!());
						bail!(Error::Internal);
					}
				};
			if !eq {
				log::error!(
					"Type error [{}]: Mismatch in Binary Operator\n\
					{lhs:?}: {lhs_type:?} {op:?} {rhs:?}: {rhs_type:?}",
					line!()
				);
				bail!(Error::TypeError);
			}
			lhs_type
		}
		Subexpr::IfExpr(IfExpr {
			condition,
			lhs,
			rhs,
		}) => {
			let cond_type = check_subexpr(condition, ctx)?;
			if cond_type.raw != RawType::Boolean {
				log::error!(
					"Type error [{}]: Non boolean condition in if statement\n{condition:?}",
					line!()
				);
				bail!(Error::TypeError);
			}
			let lhs_type = check_block(lhs, ctx)?;
			let rhs_type = check_block(rhs, ctx)?;

			if !lhs_type.raw.integer_equality(&rhs_type.raw) {
				log::error!(
					"Type error [{}]: Different types in then and else part of if statement\n\
					{lhs_type:?} {rhs_type:?}",
					line!()
				);
				bail!(Error::TypeError);
			}

			lhs_type
		}
		Subexpr::Block(block) => check_block(block, ctx)?,
		Subexpr::Tuple(tuple) => {
			let types = tuple
				.iter_mut()
				.map(|s| check_subexpr(s, ctx))
				.collect::<Result<Vec<Type>>>()?;
			Type {
				raw: RawType::Tuple(types),
				mutable: false,
			}
		}
		Subexpr::FunctionCall(FunctionCall {
			function_name,
			arguments,
		}) => {
			let FunctionType {
				return_type,
				arguments: expected_args,
			} = ctx.get(function_name)
				.ok_or_else(|| {
					log::error!(
						"Internal [{}]: Supposedly checked function is undefined\n\
						{function_name:?} {ctx:?}",
						line!()
					);
					Error::Internal
				})?
				.clone()
				.0
				.function()
				.ok_or_else(|| {
					log::error!(
						"Internal [{}]: Supposedly checked function was a variable\n\
						{function_name:?} {ctx:?}",
						line!()
					);
					Error::Internal
				})?;
			for (expected_arg, actual_arg) in
				expected_args.iter().zip(arguments.iter_mut())
			{
				let actual_type = check_subexpr(actual_arg, ctx)?;
				if expected_arg != &actual_type {
					log::error!(
						"Type mismatch in function arguments [{}]: {expected_arg:?} {actual_type:?}", line!()
					);
					bail!(Error::TypeError);
				}
			}
			Type {
				raw: return_type,
				mutable: false,
			}
		}
		Subexpr::Literal(Literal::Integer(_)) => Type {
			raw: RawType::IntegerLiteral,
			mutable: false,
		},
		Subexpr::Literal(Literal::Float(_)) => Type {
			raw: RawType::Real,
			mutable: false,
		},
		Subexpr::Literal(Literal::Boolean(_)) => Type {
			raw: RawType::Boolean,
			mutable: false,
		},
		Subexpr::Literal(Literal::Unit) => Type {
			raw: RawType::Unit,
			mutable: false,
		},
		Subexpr::Variable(name) => ctx
			.get(name)
			.ok_or_else(|| {
				let bt = backtrace::Backtrace::new();
				eprintln!("{bt:?}");
				log::error!(
					"Internal [{}]: Supposedly checked variable is undefined\n\
					{name:?} {ctx:?}",
					line!()
				);
				Error::Internal
			})?
			.clone()
			.0
			.variable()
			.ok_or_else(|| {
				log::error!(
					"Internal [{}]: Supposedly checked variable was a function\n\
					{name:?} {ctx:?}",
					line!()
				);
				Error::Internal
			})?,
	};
	Ok(res)
}
