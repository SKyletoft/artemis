use anyhow::{bail, Result};
use smallvec::{smallvec, SmallVec};

type SmallString = smallstr::SmallString<[u8; 16]>;

use crate::{
	ast::{
		BinaryOperator, InnerPattern, Pattern, StructFieldPattern, StructPattern,
		TuplePattern,
	},
	ast2::{
		self, Argument, Assignment as Ast2Assignment, Declaration as Ast2Declaration,
		Expr as Ast2Expr, FunctionCall as Ast2FunctionCall,
		FunctionDefinition as Ast2FunctionDefinition,
		StructFieldLiteral as StructFieldLiteral2, StructLiteral as StructLiteral2,
		Term as Ast2Term,
	},
	detype2_types::{
		BinOp, Declaration, Expr, Function, FunctionCall, IfExpr, Op, Term,
		TopLevelConstruct, UnOp,
	},
	error::Error,
	type_definition::{Context, EnumType2, RawType2, StructField2, StructType2, Type2},
};

pub fn detype_program(block: Vec<Ast2Expr>) -> Result<Vec<TopLevelConstruct>> {
	let mut ctx = Context::with_builtins();
	block.into_iter()
		.map(|x| x.detype(&mut ctx))
		.collect::<Result<Vec<_>>>()?
		.into_iter()
		.filter(|(x, _)| !matches!(x, Expr::Term(Term::Literal(_)))) // Top level type alias
		.map(|(x, _)| x.try_into())
		.collect()
}

trait Detype {
	type Output;

	fn detype(self, ctx: &mut Context) -> Result<(Self::Output, EnumType2)>;
}

impl TryFrom<Expr> for TopLevelConstruct {
	type Error = anyhow::Error;

	fn try_from(e: Expr) -> Result<Self> {
		match e {
			Expr::Term(Term::Expr(inner)) => TopLevelConstruct::try_from(*inner),
			Expr::Declaration(d) => Ok(TopLevelConstruct::Declaration(d)),
			Expr::Function(f) => Ok(TopLevelConstruct::Function(*f)),
			e => {
				log::trace!("{e:#?}");
				bail!(Error::InternalIllegalConstructAtTopLevel(line!()))
			}
		}
	}
}

impl Detype for Ast2Expr {
	type Output = Expr;

	fn detype(self, ctx: &mut Context) -> Result<(Expr, EnumType2)> {
		let res = match self {
			Ast2Expr::BinOp {
				left,
				right,
				op: BinaryOperator::Dot,
			} => {
				let (lhs, l_type) = left.detype(ctx)?;

				let Ast2Expr::Leaf(r_box) = right.as_ref() else {
					bail!(Error::InternalNonNameDot(line!()));
				};
				let Ast2Term::VarName(name) = r_box.as_ref() else {
					bail!(Error::InternalNonNameDot(line!()))
				};

				let left_type = match l_type.0.as_slice() {
					[RawType2::StructType(StructType2(fields))] => fields,
					_ => bail!(Error::TODOAccessFieldOnEnum(line!())),
				};

				let field_idx = left_type
					.iter()
					.position(|f| &f.name == name)
					.ok_or(Error::Internal(line!()))?;
				let typ = left_type[field_idx].type_name.clone();

				let res = Expr::Term(Term::UnOp(UnOp {
					op: Op::LoadConst,
					rhs: Box::new(Expr::Term(Term::BinOp(BinOp {
						lhs: Box::new(lhs),
						op: Op::Plus,
						rhs: Box::new(Expr::Term(Term::Literal(
							field_idx as u64,
						))),
					}))),
				}));

				(res, typ)
			}
			Ast2Expr::BinOp { left, right, op } => {
				let (lhs, l_type) = left.detype(ctx)?;
				let (rhs, r_type) = right.detype(ctx)?;
				if l_type != r_type {
					bail!(Error::InternalMismatchedTypes(line!()));
				}
				let typ =
					l_type.get_only().ok_or(Error::MismatchedTypes(line!()))?;

				let op = Op::from((op, typ));
				let res = Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(lhs),
					op,
					rhs: Box::new(rhs),
				}));

				(res, typ.into())
			}
			Ast2Expr::UnOp { op, right } => {
				let (rhs, typ) = right.detype(ctx)?;
				let typ = typ.get_only().ok_or(Error::MismatchedTypes(line!()))?;
				let op = Op::from((op, typ));
				let res = Expr::Term(Term::UnOp(UnOp {
					op,
					rhs: Box::new(rhs),
				}));
				(res, typ.into())
			}
			Ast2Expr::Leaf(leaf) => {
				let (res, typ) = leaf.detype(ctx)?;
				(Expr::Term(res), typ)
			}
		};
		Ok(res)
	}
}

impl Detype for Ast2Term {
	type Output = Term;

	fn detype(self, ctx: &mut Context) -> Result<(Term, EnumType2)> {
		let res: (Term, EnumType2) = match self {
			Ast2Term::TypeValue(t) => (Term::Literal(t), RawType2::Natural.into()),
			Ast2Term::Float(f) => (Term::Literal(f.to_bits()), RawType2::Real.into()),
			Ast2Term::Natural(n) => (Term::Literal(n), RawType2::Natural.into()),
			Ast2Term::Integer(i) => (Term::Literal(i as u64), RawType2::Integer.into()),
			Ast2Term::Boolean(b) => (Term::Literal(b as u64), RawType2::Natural.into()),
			Ast2Term::String(_) => todo!(),
			Ast2Term::Char(_) => todo!(),
			Ast2Term::Unit => (Term::Unit, RawType2::Unit.into()),
			Ast2Term::Tuple(_) => todo!(),
			Ast2Term::StructLiteral(s) => s.detype(ctx)?,
			Ast2Term::Block(ast2::Block(b)) => {
				let len = b.len();
				let (res, typ) = b
					.into_iter()
					.map(|e| e.detype(ctx))
					.collect::<Result<Vec<_>>>()?
					.into_iter()
					.fold(
						(Vec::with_capacity(len), RawType2::Unit.into()),
						|(mut acc, _), (curr, typ)| {
							acc.push(curr);
							(acc, typ)
						},
					);
				(Term::Block(res), typ)
			}
			Ast2Term::IfExpr(ast2::IfExpr {
				condition,
				then_branch,
				else_branch,
			}) => {
				let (condition, _) = condition.detype(ctx)?;
				let (lhs, lhs_typ) = then_branch.detype(ctx)?;
				let (rhs, rhs_typ) = else_branch.detype(ctx)?;

				if lhs_typ != rhs_typ {
					bail!(Error::InternalMismatchedTypes(line!()));
				}

				let if_expr = IfExpr {
					condition: Box::new(condition),
					lhs: Box::new(lhs),
					rhs: Box::new(rhs),
				};
				(Term::IfExpr(if_expr), lhs_typ)
			}
			Ast2Term::MatchExpr(_) => todo!(),
			Ast2Term::FunctionCall(Ast2FunctionCall { func, args }) => {
				let arguments = args
					.into_iter()
					.map(|e| e.detype(ctx).map(|(e, _)| e))
					.collect::<Result<_>>()?;
				let (term, typ) = match func.leaf().and_then(|t| t.var_name()) {
					Some(function_name) => {
						let function_ret_type =
							ctx.variables
								.get(&function_name)
								.and_then(|t| {
									let raw = t
										.inner_ref()
										.enum_type
										.get_only()?;
									match raw {
									RawType2::FunctionType { ret, .. } => Some(ret.as_ref()),
									_ => None,
								}
								})
								.ok_or_else(|| {
									dbg!(&function_name);
									dbg!(&ctx.variables
										[&function_name]);
									Error::InternalMismatchedTypes(line!())
								})?;
						(
							Term::FunctionCall(FunctionCall {
								function_name,
								arguments,
							}),
							function_ret_type.clone(),
						)
					}
					None => todo!(),
				};

				(term, typ)
			}
			Ast2Term::Declaration(Ast2Declaration {
				pattern,
				expr,
				type_name,
			}) => {
				let (expr, typ) = expr.detype(ctx)?;

				let (name, types, value) =
					flatten_pattern(&pattern, &type_name, &expr)?;
				for (var_name, typ) in name.iter().cloned().zip(types.into_iter()) {
					ctx.variables.insert(var_name, typ.into());
				}
				let d = Declaration { name, value };

				(Term::Expr(Box::new(Expr::Declaration(d))), typ)
			}
			Ast2Term::Assignment(Ast2Assignment { pattern, expr }) => {
				let (expr, typ) = expr.detype(ctx)?;
				match pattern.inner {
					InnerPattern::Var(name) => (
						Term::Expr(Box::new(Expr::Assignment(
							Declaration {
								name: smallvec![name],
								value: vec![expr],
							},
						))),
						typ,
					),
					InnerPattern::Any => (Term::Expr(Box::new(expr)), typ),
					_ => todo!("TODO: Proper patterns in assignments"),
				}
			}
			Ast2Term::FunctionDefinition(Ast2FunctionDefinition {
				name,
				args,
				expr,
				..
			}) => {
				let mut inner_ctx = ctx.clone();
				for Argument { name, type_name } in args.iter().cloned() {
					inner_ctx.variables.insert(name, type_name.into());
				}

				let arguments = args.into_iter().map(|arg| arg.name).collect();
				let (expr, typ) = expr.detype(&mut inner_ctx)?;
				let f = Function {
					name,
					arguments,
					expr,
				};
				(
					Term::Expr(Box::new(Expr::Function(Box::new(f)))),
					typ,
				)
			}
			Ast2Term::VarName(n) => {
				let typ = ctx
					.variables
					.get(&n)
					.ok_or_else(|| {
						log::trace!("{n:#?}\n{ctx:#?}");
						Error::InternalCheckedUndefinedVariable(line!())
					})?
					.inner_ref()
					.enum_type
					.clone();
				(Term::Variable(n), typ)
			}
		};
		Ok(res)
	}
}

impl Detype for StructLiteral2 {
	type Output = Term;

	fn detype(self, ctx: &mut Context) -> Result<(Self::Output, EnumType2)> {
		let StructLiteral2(mut fields) = self;
		fields.sort_by(|a, b| a.name.cmp(&b.name)); // Sort by key has a lifetime issue?

		let mut ops = vec![Expr::Declaration(Declaration {
			name: smallvec!["_tmp".into()],
			value: vec![Expr::Term(Term::FunctionCall(FunctionCall {
				function_name: "malloc".into(), // TODO: Replace with GC-alloc later (when I have a GC)
				arguments: vec![Expr::Term(Term::Literal(fields.len() as u64 * 8))],
			}))],
		})];
		let mut typ = Vec::new();

		for (idx, StructFieldLiteral2 { expr, name }) in fields.into_iter().enumerate() {
			let (e, type_name) = expr.detype(ctx)?;
			ops.push(Expr::Term(Term::BinOp(BinOp {
				lhs: Box::new(Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(Expr::Term(Term::Variable("_tmp".into()))),
					op: Op::Plus,
					rhs: Box::new(Expr::Term(Term::Literal(idx as u64))),
				}))),
				op: Op::StoreExclusive,
				rhs: Box::new(e),
			})));
			typ.push(StructField2 { name, type_name });
		}

		ops.push(Expr::Term(Term::Variable("_tmp".into())));

		Ok((
			Term::Block(ops),
			RawType2::StructType(StructType2(typ)).into(),
		))
	}
}

#[allow(clippy::type_complexity)]
fn flatten_pattern(
	pat: &Pattern,
	typ: &Type2,
	expr: &Expr,
) -> Result<(SmallVec<[SmallString; 1]>, Vec<Type2>, Vec<Expr>)> {
	let Pattern {
		label,
		inner,
		irrefutable: _,
	} = pat;
	let mut names = SmallVec::new();
	let mut types = Vec::new();
	let mut exprs = Vec::new();

	if let Some(label) = label {
		names.push(label.clone());
		types.push(typ.clone());
		exprs.push(expr.clone());
	}

	match inner {
		InnerPattern::StructPattern(StructPattern {
			fields: existing_fields,
			..
		}) => {
			let mut matchable_types = typ
				.enum_type
				.0
				.iter()
				.filter_map(|t| match t {
					RawType2::StructType(StructType2(fields)) => {
						Some((fields, t.id()))
					}
					_ => None,
				})
				.collect::<SmallVec<[_; 1]>>();

			if matchable_types.len() != 1 {
				bail!(Error::TODONotYetSupportedPatternMatch(line!()));
			}

			let (all_fields, _id) = matchable_types.remove(0);

			for (
				idx,
				StructFieldPattern {
					label: field_label,
					name: _,
					pattern,
				},
				field_type,
			) in all_fields.iter().enumerate().filter_map(
				|(idx, StructField2 { name, type_name })| {
					existing_fields
						.iter()
						.find(|field| &field.name == name)
						.map(|x| (idx, x, type_name))
				},
			) {
				let e = Expr::Term(Term::UnOp(UnOp {
					op: Op::LoadConst,
					rhs: Box::new(Expr::Term(Term::BinOp(BinOp {
						lhs: Box::new(expr.clone()),
						op: Op::Plus,
						rhs: Box::new(Expr::Term(Term::Literal(
							idx as u64,
						))),
					}))),
				}));
				match (field_label, pattern) {
					(None, None) => todo!(),
					(None, Some(_)) => todo!(),
					(Some(l), None) => {
						names.push(l.clone());
						types.push(Type2::from(field_type.clone()));
						exprs.push(e);
					}
					(Some(_), Some(_)) => todo!(),
				}
			}
		}
		InnerPattern::TuplePattern(TuplePattern(fields)) => {
			// TODO: Am I even handling non-labelled fields properly?
			for (idx, pattern) in fields.iter().enumerate() {
				let e = Expr::Term(Term::UnOp(UnOp {
					op: Op::LoadConst,
					rhs: Box::new(Expr::Term(Term::BinOp(BinOp {
						lhs: Box::new(expr.clone()),
						op: Op::Plus,
						rhs: Box::new(Expr::Term(Term::Literal(
							idx as u64,
						))),
					}))),
				}));

				let (mut inner_names, mut inner_types, mut inner_exprs) =
					flatten_pattern(pattern, todo!(), &e)?;
				names.append(&mut inner_names);
				types.append(&mut inner_types);
				exprs.append(&mut inner_exprs);
			}
		}
		InnerPattern::Var(n) => {
			names.push(n.clone());
			types.push(typ.clone());
			exprs.push(expr.clone());
		}

		// Value patterns are only there for matching, not binding
		InnerPattern::Float(_)
		| InnerPattern::Natural(_)
		| InnerPattern::Integer(_)
		| InnerPattern::Boolean(_)
		| InnerPattern::String(_)
		| InnerPattern::Char(_)
		| InnerPattern::Any => {}
	}

	Ok((names, types, exprs))
}
