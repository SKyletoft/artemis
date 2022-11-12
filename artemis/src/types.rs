use std::{collections::HashMap, f64::EPSILON, rc::Rc};

use anyhow::{bail, Result};
use smallvec::{smallvec, SmallVec};

use crate::{
	ast::{
		self, ActualType, Argument, ArgumentList, Assignment, Declaration, Expr,
		FunctionCall, FunctionDefinition, IfExpr, InnerPattern, PartialApplication,
		Pattern, RawTerm, RawType, StructPattern, Term, TypeAlias,
	},
	ast2::{
		Argument as Argument2, Assignment as Assignment2, Block,
		Declaration as Declaration2, Expr as Expr2, FunctionCall as FunctionCall2,
		FunctionDefinition as FunctionDefinition2, IfExpr as IfExpr2,
		PartialApplication as PartialApplication2, Term as Term2, Tuple,
	},
	error::Error,
	type_definition::{ActualType2, Context, EnumType2, RawType2, Type2},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

trait Check
where
	Self: Sized,
{
	type Output;
	fn check(self, ctx: &mut Context) -> Result<(Self::Output, EnumType2)>;
}

pub fn check_and_infer(program: Vec<Expr>) -> Result<Vec<Expr2>> {
	let mut ctx = Context::default();
	program.into_iter()
		.map(|e| check_top_level(e, &mut ctx))
		.collect()
}

fn check_top_level(e: Expr, ctx: &mut Context) -> Result<Expr2> {
	let term = *e.leaf().ok_or(Error::ForbiddenExprAtTopLevel(line!()))?;
	if !matches!(
		&term.raw_term,
		RawTerm::FunctionDefinition(_) | RawTerm::TypeAlias(_) | RawTerm::Declaration(_)
	) {
		bail!(Error::ForbiddenExprAtTopLevel(line!()));
	}

	let (res, _) = term.check(ctx)?;

	Ok(Expr2::Leaf(Box::new(res)))
}

impl Check for FunctionDefinition {
	type Output = FunctionDefinition2;

	fn check(self, ctx: &mut Context) -> Result<(FunctionDefinition2, EnumType2)> {
		let FunctionDefinition {
			name,
			args,
			return_type,
			expr,
		} = self;
		// Type conversions  to get rid of aliases
		let ret_type = EnumType2::try_from_ast(&return_type, ctx)?;
		let new_args = argument2_try_from_ast(&args, ctx)?;

		// Setup context for the function body
		for Argument { name, type_name } in args.0.iter().cloned() {
			let desugared_type = ActualType2::try_from_ast_type(&type_name, ctx)?;
			ctx.variables.insert(name, desugared_type);
		}

		// Check the function body
		let (new_expr, actual_ret_type) = expr.check(ctx)?;

		// Check that the return type is correct
		if !ret_type.contains(&actual_ret_type) {
			log::error!("{ret_type} ≠ {actual_ret_type}");
			bail!(Error::MismatchedTypes(line!()));
		}

		let func_type = {
			let args =
				args.0.iter()
					.map(|arg| {
						Type2::try_from_ast(&arg.type_name, ctx)
							.map(|t| t.enum_type)
					})
					.collect::<Result<_>>()?;
			let ret = EnumType2::try_from_ast(&return_type, ctx)?.into();
			RawType2::FunctionType { args, ret }
		};

		// And build the result
		let res = FunctionDefinition2 {
			name,
			args: new_args,
			return_type: ret_type,
			expr: new_expr,
		};
		Ok((res, func_type.try_into()?))
	}
}

impl Check for Expr {
	type Output = Expr2;

	fn check(self, ctx: &mut Context) -> Result<(Expr2, EnumType2)> {
		match self {
			Expr::BinOp { left, right, op } => {
				let (l, t1) = left.check(ctx)?;
				let (r, t2) = right.check(ctx)?;
				Ok((
					Expr2::BinOp {
						left: Box::new(l),
						right: Box::new(r),
						op,
					},
					t1.join(t2),
				))
			}
			Expr::UnOp { op, right } => {
				let (r, typ) = right.check(ctx)?;
				Ok((
					Expr2::UnOp {
						op,
						right: Box::new(r),
					},
					typ,
				))
			}
			Expr::Leaf(t) => {
				let (t2, typ) = t.check(ctx)?;
				Ok((Expr2::Leaf(Box::new(t2)), typ))
			}
		}
	}
}

impl Check for Term {
	type Output = Term2;

	fn check(self, ctx: &mut Context) -> Result<(Term2, EnumType2)> {
		let Term {
			raw_term,
			type_ascription,
		} = self;

		let (term, typ): (Term2, EnumType2) = match raw_term {
			RawTerm::Float(f) => (Term2::Float(f), RawType2::Real.into()),
			RawTerm::Integer(i) => (Term2::Integer(i), RawType2::NumberLiteral.into()),
			RawTerm::Boolean(b) => (Term2::Boolean(b), RawType2::Bool.into()),
			RawTerm::String(_) => todo!(),
			RawTerm::Char(_) => todo!(),
			RawTerm::Unit => (Term2::Unit, RawType2::Unit.into()),
			RawTerm::Tuple(t) => {
				let (tuple, types) = check_expr_array(t.0, &mut ctx.clone())?;
				(Term2::Tuple(Tuple(tuple)), RawType2::Tuple(types).into())
			}
			RawTerm::StructLiteral(_) => todo!(),
			RawTerm::Block(b) => {
				let (block, mut types) = check_expr_array(b.0, &mut ctx.clone())?;
				(
					Term2::Block(Block(block)),
					types.pop().expect("A block cannot be empty or else it would parse as unit"),
				)
			}
			RawTerm::IfExpr(IfExpr {
				condition,
				then_branch,
				else_branch,
			}) => {
				let (condition, cond_type) = condition.check(ctx)?;

				if cond_type != RawType2::Bool.into() {
					bail!(Error::ConditionIsntBoolean(line!()));
				}

				let (then_branch, then_type) =
					then_branch.check(&mut ctx.clone())?;
				let (else_branch, else_type) =
					else_branch.check(&mut ctx.clone())?;

				let if_type = then_type.join(else_type);
				let res = IfExpr2 {
					condition,
					then_branch,
					else_branch,
				};

				(Term2::IfExpr(res), if_type)
			}
			RawTerm::MatchExpr(_) => todo!(),
			RawTerm::FunctionCall(fun) => {
				let (fun, typ) = fun.check(ctx)?;
				(Term2::FunctionCall(fun), typ)
			}
			RawTerm::PartialApplication(fun) => {
				let (fun, typ) = fun.check(ctx)?;
				(Term2::PartialApplication(fun), typ)
			}
			RawTerm::Declaration(def) => {
				let (decl, typ) = def.check(ctx)?;
				(Term2::Declaration(decl), typ)
			}
			RawTerm::Assignment(assignment) => {
				let (assign, typ) = assignment.check(ctx)?;
				(Term2::Assignment(assign), typ)
			}
			RawTerm::FunctionDefinition(fn_def) => {
				if ctx.variables.contains_key(fn_def.name.as_str()) {
					bail!(Error::DuplicateFunctionDefinition(line!()))
				}
				let (new_def, signature) = fn_def.check(&mut ctx.clone())?;
				ctx.variables.insert(
					new_def.name.clone(),
					ActualType2::Inferred(Rc::new(Type2 {
						mutable: false,
						enum_type: signature.clone(),
					})),
				);
				(Term2::FunctionDefinition(new_def), signature)
			}
			RawTerm::TypeAlias(ta) => ta.check(ctx)?,
			RawTerm::VarName(n) => {
				let typ = ctx
					.variables
					.get(&n)
					.ok_or(Error::UndefinedVariable(line!()))?;
				let typ = match typ {
					ActualType2::Declared(t) => t.enum_type.clone(),
					ActualType2::Inferred(t) => t.enum_type.clone(),
				};
				let res = Term2::VarName(n);

				(res, typ)
			}
		};

		if let Some(t) = type_ascription {
			let flattened_type = EnumType2::try_from_ast(&t, ctx)?;
			if !flattened_type.contains(&typ) {
				bail!(Error::IncorrectTypeAscription(line!()));
			}
		}

		Ok((term, typ))
	}
}

impl Check for Declaration {
	type Output = Declaration2;

	fn check(self, ctx: &mut Context) -> Result<(Declaration2, EnumType2)> {
		let Declaration {
			pattern,
			type_name,
			expr,
		} = self;

		let actual_type = ActualType2::try_from_ast(&type_name, ctx)?;
		let (expr, typ) = expr.check(ctx)?;

		let bindings = enum_type_matches_pattern(&typ, &pattern)?;
		ctx.join(bindings);

		if !actual_type.contains(&typ) {
			bail!(Error::MismatchedTypes(line!()));
		}

		Ok((
			Declaration2 {
				pattern,
				type_name: actual_type.or(typ.clone()),
				expr,
			},
			typ,
		))
	}
}

impl Check for FunctionCall {
	type Output = FunctionCall2;

	fn check(self, ctx: &mut Context) -> Result<(FunctionCall2, EnumType2)> {
		let FunctionCall { func, args } = self;
		let (func, typ) = func.check(ctx)?;
		let (args, types) = split_vec(
			args.into_iter()
				.map(|e| e.check(ctx))
				.collect::<Result<Vec<_>>>()?,
		);

		let ret = if let [RawType2::FunctionType { args, ret }] = typ.0.as_slice() {
			if let Some((expected, actual)) =
				args.iter().zip(types.iter()).find(|(a, b)| a != b)
			{
				log::error!("{expected} ≠ {actual}");
				bail!(Error::MismatchedTypes(line!()));
			}
			ret.as_ref().clone()
		} else {
			bail!(Error::TypeNonFunctionAsFunction(line!()));
		};

		let res = FunctionCall2 { func, args };
		Ok((res, ret))
	}
}

impl Check for PartialApplication {
	type Output = PartialApplication2;

	fn check(self, ctx: &mut Context) -> Result<(PartialApplication2, EnumType2)> {
		let PartialApplication { func, args } = self;
		let (func, typ) = func.check(ctx)?;
		let args = args
			.into_iter()
			.map(|o| o.map(|e| e.check(ctx).map(|(a, _)| a)).transpose())
			.collect::<Result<Vec<_>>>()?;
		let res = PartialApplication2 { func, args };
		Ok((res, typ))
	}
}

impl Check for Assignment {
	type Output = Assignment2;

	fn check(self, ctx: &mut Context) -> Result<(Assignment2, EnumType2)> {
		// TODO: Handle proper patterns in assignment
		let Assignment { pattern, expr } = self;

		let v = match pattern {
			Pattern {
				inner: InnerPattern::Var(ref v),
				label: None,
				irrefutable: false,
			} => v,
			_ => bail!(Error::TODOUnsupportedAssignmentPattern(line!())),
		};

		let (new_expr, typ) = expr.check(ctx)?;
		let Type2 {
			mutable,
			enum_type: correct_type,
		} = ctx.variables
			.get(v)
			.ok_or(Error::AssignmentToUndeclaredVariable(line!()))?
			.clone()
			.inner();

		if !mutable {
			bail!(Error::AssignmentToConst(line!()));
		}

		if !correct_type.contains(&typ) {
			bail!(Error::MismatchedTypes(line!()));
		}

		let ret = Assignment2 {
			pattern,
			expr: new_expr,
		};

		Ok((ret, correct_type))
	}
}

fn enum_type_matches_pattern(typ: &EnumType2, pat: &Pattern) -> Result<Context> {
	let mut new_ctx = Context::new();
	let Pattern {
		label,
		inner,
		irrefutable,
	} = pat;

	match inner {
		InnerPattern::StructPattern(StructPattern { fields, more }) => {
			todo!()
		}
		InnerPattern::TuplePattern(_) => todo!(),
		InnerPattern::Float(_) => todo!(),
		InnerPattern::Integer(_) => {
			new_ctx.try_insert(label, RawType2::Integer.try_into().unwrap());
			let matches = typ.0.iter().any(|x| {
				matches!(
					x,
					RawType2::Natural
						| RawType2::Integer | RawType2::NumberLiteral
				)
			});
			match (matches, *irrefutable) {
				(true, true) => bail!(Error::UnprovedIrrefutablePattern(line!())),
				(false, _) => bail!(Error::PatternDoesntMatch(line!())),
				_ => {}
			}
		}
		InnerPattern::Boolean(_) => todo!(),
		InnerPattern::String(_) => todo!(),
		InnerPattern::Char(_) => todo!(),
		InnerPattern::Var(v) => {
			let name = if let Some(lbl) = label {
				lbl.clone()
			} else {
				v.clone()
			};
			new_ctx.variables.insert(name, typ.clone().into());
		}
		InnerPattern::Any => {}
	};

	Ok(new_ctx)
}

impl Check for TypeAlias {
	type Output = Term2;

	fn check(self, ctx: &mut Context) -> Result<(Term2, EnumType2)> {
		let TypeAlias {
			name,
			mutable,
			type_name,
		} = self;
		let converted_type = EnumType2::try_from_ast(&type_name, ctx)?;
		if mutable {
			bail!(Error::MutableTypeAlias(line!()));
		}
		ctx.types.insert(name, converted_type.clone());
		Ok((Term2::TypeValue(converted_type.id()), converted_type))
	}
}

fn check_expr_array(exprs: Vec<Expr>, ctx: &mut Context) -> Result<(Vec<Expr2>, Vec<EnumType2>)> {
	let results = exprs
		.into_iter()
		.map(|e| e.check(ctx))
		.collect::<Result<Vec<_>>>()?;
	Ok(split_vec(results))
}

fn split_vec<T, U>(v: Vec<(T, U)>) -> (Vec<T>, Vec<U>) {
	let mut l_vec = Vec::with_capacity(v.len());
	let mut r_vec = Vec::with_capacity(v.len());
	for (l, r) in v.into_iter() {
		l_vec.push(l);
		r_vec.push(r);
	}
	(l_vec, r_vec)
}

fn argument2_try_from_ast(args: &ArgumentList, ctx: &Context) -> Result<SmallVec<[Argument2; 1]>> {
	args.0.iter()
		.map(|Argument { name, type_name }| {
			let name = name.clone();
			let type_name = Type2::try_from_ast(type_name, ctx)?;

			Ok(Argument2 { name, type_name })
		})
		.collect::<Result<_>>()
}

fn float_eq(a: f64, b: f64) -> bool {
	(a - b).abs() < EPSILON || (b - a).abs() < EPSILON
}
