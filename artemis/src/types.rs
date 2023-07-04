use std::rc::Rc;

use anyhow::{bail, Result};
use smallvec::{smallvec, SmallVec};

use crate::{
	ast::{
		ActualType, Argument, ArgumentList, Assignment, BinaryOperator, Case, Declaration,
		EnumType, Expr, FunctionCall, FunctionDefinition, IfExpr, InnerPattern, MatchExpr,
		PartialApplication, Pattern, RawTerm, StructFieldLiteral, StructFieldPattern,
		StructLiteral, StructPattern, Term, TypeAlias,
	},
	ast2::{
		Argument as Argument2, Assignment as Assignment2, Block, Case as Case2,
		Declaration as Declaration2, Expr as Expr2, FunctionCall as FunctionCall2,
		FunctionDefinition as FunctionDefinition2, IfExpr as IfExpr2,
		MatchExpr as MatchExpr2, PartialApplication as PartialApplication2,
		StructFieldLiteral as StructFieldLiteral2, StructLiteral as StructLiteral2,
		Term as Term2, Tuple,
	},
	error::Error,
	split_vec,
	type_definition::{
		ActualType2, Context, EnumType2, RawType2, StructField2, StructType2, Type2,
	},
};

trait Check
where
	Self: Sized,
{
	type Output;
	fn check(self, ctx: &mut Context) -> Result<(Self::Output, EnumType2)>;
}

pub fn check_and_infer(program: Vec<Expr>) -> Result<Vec<Expr2>> {
	let mut ctx = Context::with_builtins();
	for e in program.iter() {
		get_from_top_level(e, &mut ctx)?;
	}
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

fn get_from_top_level(e: &Expr, ctx: &mut Context) -> Result<()> {
	let term = e
		.leaf_ref()
		.ok_or(Error::ForbiddenExprAtTopLevel(line!()))?;
	match &term.raw_term {
		RawTerm::FunctionDefinition(FunctionDefinition {
			name, args, return_type, ..
		}) => {
			let func_type = func_type(args, return_type, ctx)?;
			ctx.variables.insert(name.clone(), func_type.into());
		}
		RawTerm::Declaration(Declaration {
			pattern:
				Pattern {
					label: None,
					inner: InnerPattern::Var(n),
					..
				},
			type_name,
			..
		}) => {
			let actual_type = match type_name {
				ActualType::Declared(d) => ActualType2::try_from_ast_type(d, ctx)?,
				ActualType::Inferred => {
					log::debug!("{:#?}", term.raw_term);
					bail!(Error::RequireTypeAtTopLevel(line!()))
				}
			};
			ctx.variables.insert(n.clone(), actual_type);
		}
		RawTerm::Declaration(_) => todo!("Top level pattern matching is todo for now"),
		RawTerm::TypeAlias(TypeAlias {
			name,
			mutable: false,
			type_name,
		}) => {
			ctx.types.insert(
				name.clone(),
				EnumType2::try_from_ast(type_name, ctx)?,
			);
		}
		_ => bail!(Error::ForbiddenExprAtTopLevel(line!())),
	}
	Ok(())
}

fn func_type(args: &ArgumentList, return_type: &EnumType, ctx: &Context) -> Result<RawType2> {
	let args =
		args.0.iter()
			.map(|arg| Type2::try_from_ast(&arg.type_name, ctx).map(|t| t.enum_type))
			.collect::<Result<_>>()?;
	let ret = EnumType2::try_from_ast(return_type, ctx)?.into();

	Ok(RawType2::FunctionType { args, ret })
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
		let expected_ret_type = EnumType2::try_from_ast(&return_type, ctx)?;
		let new_args = argument2_try_from_ast(&args, ctx)?;

		// Setup context for the function body
		for Argument { name, type_name } in args.0.iter().cloned() {
			let desugared_type = ActualType2::try_from_ast_type(&type_name, ctx)?;
			ctx.variables.insert(name, desugared_type);
		}

		// Check the function body
		log::trace!("{expr:#?}");
		let (new_expr, actual_ret_type) = expr.check(ctx)?;

		// Check that the return type is correct
		if !expected_ret_type.contains(&actual_ret_type) {
			log::debug!("{ctx}");
			log::error!("{name}: {expected_ret_type} ≠ {actual_ret_type}");
			bail!(Error::MismatchedTypes(line!()));
		}

		let func_type = func_type(&args, &return_type, ctx)?;

		// And build the result
		let res = FunctionDefinition2 {
			name,
			args: new_args,
			return_type: expected_ret_type,
			expr: new_expr,
		};
		Ok((res, func_type.try_into()?))
	}
}

impl Check for Expr {
	type Output = Expr2;

	fn check(self, ctx: &mut Context) -> Result<(Expr2, EnumType2)> {
		match self {
			Expr::BinOp {
				left,
				right,
				op: BinaryOperator::Dot,
			} => {
				let (l, t1) = left.check(ctx)?;
				let Term { raw_term: RawTerm::VarName(n), ..} = *right.leaf().ok_or(Error::UnknownStructField(line!()))? else {
					bail!(Error::UnknownStructField(line!()));
				};

				let typ = t1.get_field(&n)?;
				Ok((
					Expr2::BinOp {
						left: Box::new(l),
						right: Box::new(Expr2::Leaf(Box::new(
							Term2::VarName(n),
						))),
						op: BinaryOperator::Dot,
					},
					typ,
				))
			}
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
			RawTerm::Natural(n) => (Term2::Natural(n), RawType2::Natural.into()),
			RawTerm::Integer(i) => (Term2::Integer(i), RawType2::Integer.into()),
			RawTerm::Boolean(b) => (Term2::Boolean(b), RawType2::Bool.into()),
			RawTerm::String(_) => todo!(),
			RawTerm::Char(_) => todo!(),
			RawTerm::Unit => (Term2::Unit, RawType2::Unit.into()),
			RawTerm::Tuple(t) => {
				let (tuple, types) = check_expr_array(t.0, &mut ctx.clone())?;
				(
					Term2::Tuple(Tuple(tuple)),
					RawType2::Tuple(types).into(),
				)
			}
			RawTerm::StructLiteral(l) => {
				let (lit, typ) = l.check(ctx)?;
				(Term2::StructLiteral(lit), typ)
			}
			RawTerm::Block(b) => {
				let (block, mut types) = check_expr_array(b.0, &mut ctx.clone())?;
				(
					Term2::Block(Block(block)),
					types
						.pop()
						.expect("A block cannot be empty or else it would parse as unit"),
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
			RawTerm::MatchExpr(mat) => {
				let (mat, typ) = mat.check(ctx)?;
				(Term2::MatchExpr(mat), typ)
			}
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
				let typ = ctx.variables.get(&n).ok_or_else(|| {
					log::error!("Undefined variable: {n}");
					Error::UndefinedVariable(line!())
				})?;
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

impl Check for MatchExpr {
	type Output = MatchExpr2;

	fn check(self, ctx: &mut Context) -> Result<(MatchExpr2, EnumType2)> {
		let MatchExpr { expr, cases } = self;
		let (expr, _) = expr.check(ctx)?;

		let case_typ_pairs = cases
			.into_iter()
			.map(|c| c.check(ctx))
			.collect::<Result<_>>()?;
		let (cases, types) = split_vec(case_typ_pairs);
		let typ = types
			.into_iter()
			.reduce(EnumType2::join)
			.ok_or(Error::NoCasesInMatch(line!()))?;

		let res = MatchExpr2 { expr, cases };
		Ok((res, typ))
	}
}

impl Check for Case {
	type Output = Case2;

	fn check(self, _ctx: &mut Context) -> Result<(Case2, EnumType2)> {
		todo!()
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

		let (expr, typ) = expr.check(ctx)?;

		let actual_type = match type_name {
			ActualType::Declared(d) => ActualType2::try_from_ast_type(&d, ctx)?,
			ActualType::Inferred => ActualType2::Inferred(Rc::new(Type2 {
				mutable: false,
				enum_type: typ.clone(),
			})),
		};

		let bindings = enum_type_matches_pattern(
			actual_type.enum_type(),
			&pattern,
			actual_type.mutable(),
		)?;
		ctx.join(bindings);

		if !actual_type.contains(&typ) {
			log::error!("{typ} cannot be assigned to {actual_type}");
			bail!(Error::MismatchedTypes(line!()));
		}

		let type_name = actual_type.or(typ.clone());

		Ok((
			Declaration2 {
				pattern,
				type_name,
				expr,
			},
			typ,
		))
	}
}

impl Check for StructLiteral {
	type Output = StructLiteral2;

	fn check(self, ctx: &mut Context) -> Result<(StructLiteral2, EnumType2)> {
		let StructLiteral(fields) = self;
		let split_struct_field_literal = |(StructFieldLiteral2 { name, expr }, typ)| {
			let field_lit = StructFieldLiteral2 {
				name: name.clone(),
				expr,
			};
			let struct_field = StructField2 {
				name,
				type_name: typ,
			};
			(field_lit, struct_field)
		};

		let (checked_fields, types) = split_vec(
			fields.into_iter()
				.map(|f| f.check(ctx).map(split_struct_field_literal))
				.collect::<Result<_>>()?,
		);

		let typ = EnumType2(smallvec![RawType2::StructType(StructType2(
			types
		))]);
		let lit = StructLiteral2(checked_fields);

		Ok((lit, typ))
	}
}

impl Check for StructFieldLiteral {
	type Output = StructFieldLiteral2;

	fn check(self, ctx: &mut Context) -> Result<(StructFieldLiteral2, EnumType2)> {
		let StructFieldLiteral { name, expr } = self;
		let (expr, typ) = expr.check(ctx)?;
		Ok((StructFieldLiteral2 { name, expr }, typ))
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

		let [RawType2::FunctionType { args: typ_args, ret }] = typ.0.as_slice() else {
			bail!(Error::TypeNonFunctionAsFunction(line!()));
		};

		if let Some((expected, actual)) =
			typ_args.iter().zip(types.iter()).find(|(a, b)| a != b)
		{
			log::error!("{expected} ≠ {actual}");
			bail!(Error::MismatchedTypes(line!()));
		}

		let res = FunctionCall2 { func, args };
		Ok((res, ret.as_ref().clone()))
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

fn enum_type_matches_pattern(typ: &EnumType2, pat: &Pattern, mutable: bool) -> Result<Context> {
	let mut new_ctx = Context::new();
	let Pattern {
		label,
		inner,
		irrefutable,
	} = pat;

	new_ctx.try_insert(label, typ.clone().into());
	match inner {
		InnerPattern::StructPattern(StructPattern { fields, more }) => {
			assert!(!*more, "todo");
			let mut matches_all_options = true;

			'outer: for StructFieldPattern {
				label,
				name,
				pattern,
			} in fields.iter()
			{
				let mut field_type = EnumType2(SmallVec::new());
				for variant in typ.0.iter() {
					let RawType2::StructType(StructType2(ts)) = variant else {
						bail!(Error::PatternDoesntMatch(line!()))
					};
					let field = ts.iter().find(|field| &field.name == name);
					let Some(field) = field else {
						matches_all_options = false;
						continue 'outer;
					};
					field_type = field_type.join(field.type_name.clone());
				}

				match (label, pattern) {
					(None, None) => {
						new_ctx.variables.insert(
							name.clone(),
							Type2 {
								mutable,
								enum_type: field_type,
							}
							.into(),
						);
					}
					(None, Some(p)) => {
						let bindings = enum_type_matches_pattern(
							&field_type,
							p,
							mutable,
						)?;
						new_ctx.join(bindings);
					}
					(Some(l), None) => {
						new_ctx.variables.insert(
							l.clone(),
							Type2 {
								mutable,
								enum_type: field_type,
							}
							.into(),
						);
					}
					(Some(l), Some(p)) => {
						let bindings = enum_type_matches_pattern(
							&field_type,
							p,
							mutable,
						)?;
						new_ctx.join(bindings);
						new_ctx.variables.insert(
							l.clone(),
							Type2 {
								mutable,
								enum_type: field_type,
							}
							.into(),
						);
					}
				}
			}

			if !(matches_all_options || *irrefutable) {
				bail!(Error::UnprovedIrrefutablePattern(line!()));
			}
		}
		InnerPattern::TuplePattern(_) => todo!(),
		InnerPattern::Float(_) => todo!(),
		InnerPattern::Natural(_) => todo!(),
		InnerPattern::Integer(_) => todo!(),
		InnerPattern::Boolean(_) => todo!(),
		InnerPattern::String(_) => todo!(),
		InnerPattern::Char(_) => todo!(),
		InnerPattern::Var(v) => {
			if label.is_none() {
				new_ctx.variables.insert(
					v.clone(),
					ActualType2::Declared(Type2 {
						mutable,
						enum_type: typ.clone(),
					}),
				);
			}
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
		Ok((
			Term2::TypeValue(converted_type.id()),
			converted_type,
		))
	}
}

fn check_expr_array(exprs: Vec<Expr>, ctx: &mut Context) -> Result<(Vec<Expr2>, Vec<EnumType2>)> {
	let results = exprs
		.into_iter()
		.map(|e| e.check(ctx))
		.collect::<Result<Vec<_>>>()?;
	Ok(split_vec(results))
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
