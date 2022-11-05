use anyhow::{bail, Result};
use once_cell::sync::Lazy;
use smallvec::smallvec;
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;

use crate::{
	ast::{
		BinaryOperator, InnerPattern, Pattern, StructFieldPattern, StructPattern,
		TuplePattern, UnaryOperator,
	},
	ast2::{
		self, Argument, Declaration as Ast2Declaration, Expr as Ast2Expr,
		FunctionDefinition as Ast2FunctionDefinition, Term as Ast2Term,
	},
	detype2_types::{
		BinOp, Declaration, Expr, Function, FunctionCall, IfExpr, Op, Term,
		TopLevelConstruct, Type, UnOp,
	},
	error::Error,
	type_definition::Context,
};

pub fn detype_program(block: Vec<Ast2Expr>) -> Result<Vec<TopLevelConstruct>> {
	let mut ctx = Context::new();
	block.into_iter()
		.map(|x| x.detype(&mut ctx))
		.collect::<Result<Vec<_>>>()?
		.into_iter()
		.map(|(x, _)| x.try_into())
		.collect()
}

impl TryFrom<Expr> for TopLevelConstruct {
	type Error = anyhow::Error;

	fn try_from(e: Expr) -> Result<Self> {
		match e {
			Expr::Declaration(d) => Ok(TopLevelConstruct::Declaration(d)),
			Expr::Function(f) => Ok(TopLevelConstruct::Function(*f)),
			_ => bail!(Error::InternalIllegalConstructAtTopLevel(line!())),
		}
	}
}

impl Ast2Expr {
	fn detype(self, ctx: &mut Context) -> Result<(Expr, Type)> {
		let res = match self {
			Ast2Expr::BinOp { left, right, op } => {
				let (lhs, l_type) = left.detype(ctx)?;
				let (rhs, r_type) = right.detype(ctx)?;
				if l_type != r_type {
					bail!(Error::InternalMismatchedTypes(line!()));
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

impl Ast2Term {
	fn detype(self, ctx: &mut Context) -> Result<(Term, Type)> {
		let res = match self {
			Ast2Term::TypeValue(t) => (Term::Literal(t), Type::Unsigned),
			Ast2Term::Float(f) => (Term::Literal(f.to_bits()), Type::Floating),
			Ast2Term::Integer(i) => (Term::Literal(i as u64), Type::Unsigned),
			Ast2Term::Boolean(b) => (Term::Literal(b as u64), Type::Unsigned),
			Ast2Term::String(_) => todo!(),
			Ast2Term::Char(_) => todo!(),
			Ast2Term::Unit => (Term::Unit, Type::Unsigned),
			Ast2Term::Tuple(_) => todo!(),
			Ast2Term::StructLiteral(_) => todo!(),
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
			Ast2Term::IfExpr(_) => todo!(),
			Ast2Term::MatchExpr(_) => todo!(),
			Ast2Term::FunctionCall(_) => todo!(),
			Ast2Term::PartialApplication(_) => todo!(),
			Ast2Term::Declaration(Ast2Declaration {
				pattern,
				type_name,
				expr,
			}) => {
				let (expr, typ) = expr.detype(ctx)?;

				let d = Declaration {
					name: todo!(),
					value: todo!(),
				};
				(Term::Expr(Box::new(Expr::Declaration(d))), typ)
			}
			Ast2Term::Assignment(_) => todo!(),
			Ast2Term::FunctionDefinition(Ast2FunctionDefinition {
				name,
				args,
				expr,
				..
			}) => {
				let arguments = args.into_iter().map(|arg| arg.name).collect();
				let (expr, _) = expr.detype(ctx)?;
				let f = Function {
					name,
					arguments,
					expr,
				};
				let typ = Type::Unsigned;
				(Term::Expr(Box::new(Expr::Function(Box::new(f)))), typ)
			}
			Ast2Term::VarName(_) => todo!(),
		};
		Ok(res)
	}
}

fn flatten_pattern(pat: &Pattern, expr: &Expr) -> Result<Vec<(SmallString, Expr)>> {
	let Pattern {
		label,
		inner,
		irrefutable,
	} = pat;
	let mut res = Vec::new();

	if let Some(label) = label {
		res.push((label.clone(), expr.clone()));
	}
	match inner {
		InnerPattern::StructPattern(StructPattern { fields, .. }) => {
			// TODO: Skipped fields?
			// TODO: Am I even handling non-labelled fields properly?
			for (idx, StructFieldPattern { name, pattern }) in fields.iter().enumerate()
			{
				let e = Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(expr.clone()),
					op: Op::Dot,
					rhs: Box::new(Expr::Term(Term::Literal(idx as u64))),
				}));
				if let Some(pat) = pattern {
					res.append(&mut flatten_pattern(pat, &e)?);
				} else {
					res.push((name.clone(), e));
				}
			}
		}
		InnerPattern::TuplePattern(TuplePattern(fields)) => {
			// TODO: Am I even handling non-labelled fields properly?
			for (idx, pattern) in fields.iter().enumerate() {
				let e = Expr::Term(Term::BinOp(BinOp {
					lhs: Box::new(expr.clone()),
					op: Op::Dot,
					rhs: Box::new(Expr::Term(Term::Literal(idx as u64))),
				}));
				res.append(&mut flatten_pattern(pat, &e)?);
			}
		}
		InnerPattern::Var(n) => {
			res.push((n.clone(), expr.clone()));
		}

		// Value patterns are only there for matching, not binding
		InnerPattern::Float(_)
		| InnerPattern::Integer(_)
		| InnerPattern::Boolean(_)
		| InnerPattern::String(_)
		| InnerPattern::Char(_)
		| InnerPattern::Any => {}
	}

	Ok(res)
}
