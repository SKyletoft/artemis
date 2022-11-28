use anyhow::{bail, Result};
use once_cell::sync::Lazy;
use smallvec::{smallvec, SmallVec};
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;

use crate::{
	ast::{
		BinaryOperator, InnerPattern, Pattern, StructFieldLiteral, StructFieldPattern,
		StructPattern, TuplePattern, UnaryOperator,
	},
	ast2::{
		self, Argument, Assignment as Ast2Assignment, Declaration as Ast2Declaration,
		Expr as Ast2Expr, FunctionDefinition as Ast2FunctionDefinition,
		StructFieldLiteral as StructFieldLiteral2, StructLiteral as StructLiteral2,
		Term as Ast2Term,
	},
	detype2_types::{
		BinOp, Declaration, Expr, Function, FunctionCall, IfExpr, Op, Term,
		TopLevelConstruct, Type, UnOp,
	},
	error::Error,
	split_vec,
	type_definition::{ActualType2, Context, RawType2, StructField2, StructType2, Type2},
};

pub fn detype_program(block: Vec<Ast2Expr>) -> Result<Vec<TopLevelConstruct>> {
	let mut ctx = Context::new();
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

	fn detype(self, ctx: &mut Context) -> Result<(Self::Output, Type)>;
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

	fn detype(self, ctx: &mut Context) -> Result<(Expr, Type)> {
		let res = match self {
			Ast2Expr::BinOp { left, right, op } => {
				let (lhs, l_type) = left.detype(ctx)?;
				let (rhs, r_type) = right.detype(ctx)?;
				if l_type != r_type {
					if (l_type == Type::Floating) ^ (r_type == Type::Floating) {
						bail!(Error::InternalMismatchedTypes(line!()));
					} else {
						log::trace!("Type issue with mixed signedness");
					}
				}
				let op = Op::from((op, l_type));
				let res = Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(lhs),
					op,
					rhs: Box::new(rhs),
				}));

				(res, l_type)
			}
			Ast2Expr::UnOp { op, right } => {
				let (rhs, typ) = right.detype(ctx)?;
				let op = Op::from((op, typ));
				let res = Expr::Term(Term::UnOp(UnOp {
					op,
					rhs: Box::new(rhs),
				}));
				(res, typ)
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

	fn detype(self, ctx: &mut Context) -> Result<(Term, Type)> {
		let res = match self {
			Ast2Term::TypeValue(t) => (Term::Literal(t), Type::Unsigned),
			Ast2Term::Float(f) => (Term::Literal(f.to_bits()), Type::Floating),
			Ast2Term::Integer(i) => (Term::Literal(i as u64), Default::default()),
			Ast2Term::Boolean(b) => (Term::Literal(b as u64), Type::Unsigned),
			Ast2Term::String(_) => todo!(),
			Ast2Term::Char(_) => todo!(),
			Ast2Term::Unit => (Term::Unit, Type::Unsigned),
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
						(Vec::with_capacity(len), Type::Unsigned),
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
			Ast2Term::FunctionCall(_) => todo!(),
			Ast2Term::PartialApplication(_) => todo!(),
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
				let (expr, _) = expr.detype(&mut inner_ctx)?;
				let f = Function {
					name,
					arguments,
					expr,
				};
				let typ = Type::Unsigned;
				(Term::Expr(Box::new(Expr::Function(Box::new(f)))), typ)
			}
			Ast2Term::VarName(n) => {
				let typ = ctx
					.variables
					.get(&n)
					.ok_or(Error::InternalCheckedUndefinedVariable(line!()))?
					.into();
				(Term::Variable(n), typ)
			}
		};
		Ok(res)
	}
}

impl Detype for StructLiteral2 {
	type Output = Term;

	fn detype(self, ctx: &mut Context) -> Result<(Self::Output, Type)> {
		let StructLiteral2(mut fields) = self;
		fields.sort_by(|a, b| a.name.cmp(&b.name)); // Sort by key has a lifetime issue?

		let mut ops = vec![Expr::Declaration(Declaration {
			name: smallvec!["_tmp".into()],
			value: vec![Expr::Term(Term::FunctionCall(FunctionCall {
				function_name: "malloc".into(), // TODO: Replace with GC-alloc later (when I have a GC)
				arguments: vec![Expr::Term(Term::Literal(fields.len() as u64 * 8))],
			}))],
		})];

		for (idx, StructFieldLiteral2 { expr, .. }) in fields.into_iter().enumerate() {
			let (e, _) = expr.detype(ctx)?;
			ops.push(Expr::Term(Term::BinOp(BinOp {
				lhs: Box::new(Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(Expr::Term(Term::Variable("_tmp".into()))),
					op: Op::Plus,
					rhs: Box::new(Expr::Term(Term::Literal(idx as u64))),
				}))),
				op: Op::StoreExclusive,
				rhs: Box::new(e),
			})));
		}

		ops.push(Expr::Term(Term::Variable("_tmp".into())));

		Ok((Term::Block(ops), Type::default()))
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
		irrefutable,
	} = pat;
	let mut names = SmallVec::new();
	let mut types = Vec::new();
	let mut exprs = Vec::new();

	if let Some(label) = label {
		names.push(label.clone());
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

			let (all_fields, id) = matchable_types.remove(0);

			for (
				idx,
				StructFieldPattern {
					label: field_label,
					name,
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
				let e = Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(expr.clone()),
					op: Op::Dot,
					rhs: Box::new(Expr::Term(Term::Literal(idx as u64))),
				}));
				match (field_label, pattern) {
					(None, None) => todo!(),
					(None, Some(_)) => todo!(),
					(Some(_), None) => todo!(),
					(Some(_), Some(_)) => todo!(),
				}
			}

			todo!()
		}
		InnerPattern::TuplePattern(TuplePattern(fields)) => {
			// TODO: Am I even handling non-labelled fields properly?
			for (idx, pattern) in fields.iter().enumerate() {
				let e = Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(expr.clone()),
					op: Op::Dot,
					rhs: Box::new(Expr::Term(Term::Literal(idx as u64))),
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
		| InnerPattern::Integer(_)
		| InnerPattern::Boolean(_)
		| InnerPattern::String(_)
		| InnerPattern::Char(_)
		| InnerPattern::Any => {}
	}

	Ok((names, types, exprs))
}
