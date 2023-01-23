use std::fmt::Write;

use anyhow::{bail, Result};
use once_cell::sync::Lazy;
use pest::{
	iterators::{Pair, Pairs},
	pratt_parser::{Assoc, Op, PrattParser},
};
use smallvec::{smallvec, SmallVec};

use crate::{
	ast::{
		ActualType, Argument, ArgumentList, Assignment, BinaryOperator, Block, Case,
		Declaration, EnumType, Expr, FunctionCall, FunctionDefinition, IfExpr,
		InnerPattern, MatchExpr, PartialApplication, Pattern, RawTerm, RawType, ReturnType,
		StructField, StructFieldLiteral, StructFieldPattern, StructLiteral, StructPattern,
		StructType, Term, Tuple, TuplePattern, Type, TypeAlias, UnaryOperator,
	},
	error::Error,
	Rule,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

static PRATT: Lazy<PrattParser<Rule>> = Lazy::new(|| {
	PrattParser::new()
		.op(Op::infix(Rule::lpipe, Assoc::Left) | Op::infix(Rule::rpipe, Assoc::Right))
		.op(Op::infix(Rule::or, Assoc::Left))
		.op(Op::infix(Rule::and, Assoc::Left))
		.op(Op::infix(Rule::xor, Assoc::Left))
		.op(Op::infix(Rule::eq, Assoc::Left) | Op::infix(Rule::neq, Assoc::Left))
		.op(Op::infix(Rule::greater, Assoc::Left)
			| Op::infix(Rule::greater_eq, Assoc::Left)
			| Op::infix(Rule::less, Assoc::Left)
			| Op::infix(Rule::less_eq, Assoc::Left))
		.op(Op::infix(Rule::rshift, Assoc::Left) | Op::infix(Rule::lshift, Assoc::Left))
		.op(Op::infix(Rule::plus, Assoc::Left)
			| Op::infix(Rule::minus, Assoc::Left)
			| Op::infix(Rule::delta, Assoc::Left))
		.op(Op::infix(Rule::times, Assoc::Left)
			| Op::infix(Rule::div, Assoc::Left)
			| Op::infix(Rule::rem, Assoc::Left))
		.op(Op::prefix(Rule::negate) | Op::prefix(Rule::not))
		.op(Op::infix(Rule::exp, Assoc::Left))
		.op(Op::infix(Rule::dot, Assoc::Left)
			| Op::postfix(Rule::call)
			| Op::postfix(Rule::application))
});

impl TryFrom<Pair<'_, Rule>> for Type {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::type_name);
		let inner = pair.into_inner().collect::<SmallVec<[_; 1]>>();
		let res = match inner.as_slice() {
			[type_name] => Type {
				mutable: false,
				enum_type: EnumType::try_from(type_name.clone())?,
			},
			[_, type_name] => Type {
				mutable: true,
				enum_type: EnumType::try_from(type_name.clone())?,
			},
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for RawType {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::raw_type);
		let inner = inner(pair)?;
		let res = match inner.as_rule() {
			Rule::native_types => match inner.as_str() {
				"ℕ" => RawType::Natural,
				"ℝ" => RawType::Real,
				"ℤ" => RawType::Integer,
				"𝔹" => RawType::Bool,
				"∀" => RawType::Any,
				"∃" => bail!(Error::ParseError(line!())),
				"𝕋" => RawType::Type,
				_ => bail!(Error::ParseError(line!())),
			},
			Rule::struct_name => RawType::StructNameOrAlias(inner.as_str().into()),
			Rule::unit => RawType::Unit,
			Rule::tuple_type => RawType::Tuple(
				inner.into_inner()
					.map(EnumType::try_from)
					.collect::<Result<_>>()?,
			),
			Rule::struct_type => RawType::StructType(StructType::try_from(inner)?),
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for Tuple {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::tuple);
		let inner = inner(pair)?;
		let res = Tuple(inner
			.into_inner()
			.map(Expr::try_from)
			.collect::<Result<_, _>>()?);
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for Declaration {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::declaration);
		let inner = pair.into_inner().collect::<SmallVec<[_; 3]>>();
		let res = match inner.as_slice() {
			[pattern, type_name, expr] => {
				let pattern = Pattern::try_from(pattern.clone())?;
				let type_name = ActualType::Declared(type_name.clone().try_into()?);
				let expr = Expr::try_from(expr.clone())?;
				Declaration {
					pattern,
					type_name,
					expr,
				}
			}
			[pattern, expr] => {
				let pattern = Pattern::try_from(pattern.clone())?;
				let expr = Expr::try_from(expr.clone())?;
				let type_name = ActualType::Inferred;
				Declaration {
					pattern,
					type_name,
					expr,
				}
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for Assignment {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::assignment);
		let inner = pair.into_inner().collect::<SmallVec<[_; 3]>>();
		let res = match inner.as_slice() {
			[pattern, op, expr] => {
				let pattern = Pattern::try_from(pattern.clone())?;
				let lhs = pattern
					.inner
					.clone()
					.var()
					.ok_or(Error::OpAssignOnPattern(line!()))?;
				let op = BinaryOperator::try_from(op.clone())?;
				let rhs = Expr::try_from(expr.clone())?;
				let expr = Expr::BinOp {
					left: Box::new(Expr::Leaf(Box::new(Term {
						raw_term: RawTerm::VarName(lhs),
						type_ascription: None,
					}))),
					right: Box::new(rhs),
					op,
				};

				Assignment { pattern, expr }
			}
			[pattern, expr] => {
				let pattern = Pattern::try_from(pattern.clone())?;
				let expr = Expr::try_from(expr.clone())?;
				Assignment { pattern, expr }
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for IfExpr {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::if_expr);
		let inner = pair.into_inner().collect::<SmallVec<[_; 3]>>();
		let res = match inner.as_slice() {
			[condition, then_branch, else_branch] => {
				let condition = Expr::try_from(condition.clone())?;
				let then_branch = Expr::try_from(then_branch.clone())?;
				let else_branch = Expr::try_from(else_branch.clone())?;
				IfExpr {
					condition,
					then_branch,
					else_branch,
				}
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for MatchExpr {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::match_expr);
		let mut inner = pair.into_inner();
		let expr = Expr::try_from(inner.next().ok_or(Error::ParseError(line!()))?)?;
		let cases = inner.map(Case::try_from).collect::<Result<_>>()?;
		Ok(MatchExpr { expr, cases })
	}
}

impl TryFrom<Pair<'_, Rule>> for Case {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::case);
		let inner = pair.into_inner().collect::<SmallVec<[_; 3]>>();
		let res = match inner.as_slice() {
			[pattern, _, expr] => {
				let pattern = Pattern::try_from(pattern.clone())?;
				let expr = Expr::try_from(expr.clone())?;
				Case {
					pattern,
					condition: None,
					expr,
				}
			}
			[pattern, condition, _, expr] => {
				let pattern = Pattern::try_from(pattern.clone())?;
				let condition = Some(Expr::try_from(condition.clone())?);
				let expr = Expr::try_from(expr.clone())?;
				Case {
					pattern,
					condition,
					expr,
				}
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for Pattern {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::pattern);
		let inner = pair.into_inner().collect::<SmallVec<[_; 3]>>();

		let res = match inner.as_slice() {
			[label, pattern, _] => {
				let label = Some(label.as_str().into());
				let inner = InnerPattern::try_from(pattern.clone())?;
				let irrefutable = true;

				Pattern {
					label,
					inner,
					irrefutable,
				}
			}
			[label, pattern] if pattern.as_rule() == Rule::inner_pattern => {
				let label = Some(label.as_str().into());
				let inner = InnerPattern::try_from(pattern.clone())?;
				let irrefutable = false;

				Pattern {
					label,
					inner,
					irrefutable,
				}
			}
			[pattern, _] if pattern.as_rule() == Rule::inner_pattern => {
				let label = None;
				let inner = InnerPattern::try_from(pattern.clone())?;
				let irrefutable = true;

				Pattern {
					label,
					inner,
					irrefutable,
				}
			}
			[pattern] if pattern.as_rule() == Rule::inner_pattern => {
				let label = None;
				let inner = InnerPattern::try_from(pattern.clone())?;
				let irrefutable = false;

				Pattern {
					label,
					inner,
					irrefutable,
				}
			}
			_ => {
				log::trace!("Failing parse: {inner:#?}");
				bail!(Error::ParseError(line!()))
			}
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for InnerPattern {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::inner_pattern);
		let inner = inner(pair)?;
		let res = match inner.as_rule() {
			Rule::struct_pattern => {
				InnerPattern::StructPattern(StructPattern::try_from(inner)?)
			}
			Rule::tuple_pattern => todo!(),
			Rule::float => InnerPattern::Float(inner.as_str().parse()?),
			Rule::integer => InnerPattern::Integer(inner.as_str().parse()?),
			Rule::boolean => todo!(),
			Rule::string => todo!(),
			Rule::char => todo!(),
			Rule::var_name => InnerPattern::Var(inner.as_str().into()),
			_ => {
				if inner.as_str() == "_" {
					InnerPattern::Any
				} else {
					bail!(Error::ParseError(line!()))
				}
			}
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for StructFieldPattern {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::struct_field_pattern);
		let inner = pair.into_inner().collect::<SmallVec<[_; 2]>>();
		let res = match inner.as_slice() {
			[_label, _name, _pattern] => todo!(),
			[label, name] if name.as_rule() == Rule::var_name => {
				let label = Some(label.as_str().into());
				let name = name.as_str().into();
				let pattern = None;
				StructFieldPattern {
					label,
					name,
					pattern,
				}
			}
			[name, pattern] if pattern.as_rule() == Rule::pattern => {
				let label = None;
				let name = name.as_str().into();
				let pattern = Some(Pattern::try_from(pattern.clone())?);

				StructFieldPattern {
					label,
					name,
					pattern,
				}
			}
			[name] => {
				let label = None;
				let name = name.as_str().into();
				let pattern = None;

				StructFieldPattern {
					label,
					name,
					pattern,
				}
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for StructPattern {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::struct_pattern);

		let mut inner = pair.into_inner().collect::<SmallVec<[_; 2]>>();
		let more = if inner.last().map(Pair::as_rule) == Some(Rule::more) {
			inner.pop();
			true
		} else {
			false
		};

		let fields = inner
			.into_iter()
			.map(StructFieldPattern::try_from)
			.collect::<Result<_>>()?;

		Ok(StructPattern { fields, more })
	}
}

impl TryFrom<Pair<'_, Rule>> for TuplePattern {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::tuple_pattern);
		todo!()
	}
}

impl TryFrom<Pair<'_, Rule>> for Expr {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::expr);
		PRATT.map_primary(|pair| RawTerm::try_from(pair).map(|t| -> Expr { t.into() }))
			.map_prefix(|pair, subexpr| {
				let op = UnaryOperator::try_from(pair)?;
				let right = subexpr?.into();
				Ok(Expr::UnOp { op, right })
			})
			.map_postfix(|f, args| {
				let func = f?;
				let res =
					match args.as_rule() {
						Rule::application => {
							let args = args
								.into_inner()
								.map(|p| {
									let res = match p.as_rule() {
									Rule::expr => Some(Expr::try_from(p)?),
									Rule::any => None,
									_ => bail!(Error::ParseError(line!())),
								};
									Ok(res)
								})
								.collect::<Result<_>>()?;
							let raw_term = RawTerm::PartialApplication(
								PartialApplication { func, args },
							);
							Term {
								raw_term,
								type_ascription: None,
							}
						}
						Rule::call => {
							let args = args
								.into_inner()
								.map(Expr::try_from)
								.collect::<Result<_>>()?;
							let raw_term = RawTerm::FunctionCall(
								FunctionCall { func, args },
							);
							Term {
								raw_term,
								type_ascription: None,
							}
						}
						r => {
							log::error!("{:#?}", r);
							bail!(Error::ParseError(line!()))
						}
					};
				Ok(Expr::Leaf(Box::new(res)))
			})
			.map_infix(|lhs, pair, rhs| match pair.as_rule() {
				Rule::lpipe => {
					let func = lhs?;
					let args = vec![rhs?];
					let raw_term =
						RawTerm::FunctionCall(FunctionCall { func, args });
					Ok(Expr::Leaf(Box::new(Term {
						raw_term,
						type_ascription: None,
					})))
				}
				Rule::rpipe => {
					let func = rhs?;
					let args = vec![lhs?];
					let raw_term =
						RawTerm::FunctionCall(FunctionCall { func, args });
					Ok(Expr::Leaf(Box::new(Term {
						raw_term,
						type_ascription: None,
					})))
				}
				_ => {
					let op = BinaryOperator::try_from(pair)?;
					let left = Box::new(lhs?);
					let right = Box::new(rhs?);
					Ok(Expr::BinOp { left, right, op })
				}
			})
			.parse(pair.into_inner())
	}
}

impl TryFrom<Pair<'_, Rule>> for RawTerm {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		// dbg!(backtrace::Backtrace::new());
		assert_eq!(pair.as_rule(), Rule::raw_term);
		let inner = inner(pair)?;
		let res = match inner.as_rule() {
			Rule::float => RawTerm::Float(inner.as_str().parse()?),
			Rule::integer => RawTerm::Integer(inner.as_str().parse()?),
			Rule::boolean => RawTerm::Boolean(inner.as_str().parse()?),
			Rule::string => RawTerm::String(inner.as_str().into()),
			Rule::char => todo!(),
			Rule::unit => RawTerm::Unit,
			Rule::tuple => RawTerm::Tuple(Tuple::try_from(inner)?),
			Rule::struct_literal => {
				RawTerm::StructLiteral(StructLiteral::try_from(inner)?)
			}
			Rule::block => RawTerm::Block(Block::try_from(inner)?),
			Rule::if_expr => RawTerm::IfExpr(IfExpr::try_from(inner)?),
			Rule::match_expr => RawTerm::MatchExpr(MatchExpr::try_from(inner)?),
			Rule::declaration => RawTerm::Declaration(Declaration::try_from(inner)?),
			Rule::assignment => RawTerm::Assignment(Assignment::try_from(inner)?),
			Rule::function_definition => {
				RawTerm::FunctionDefinition(FunctionDefinition::try_from(inner)?)
			}
			Rule::type_alias => RawTerm::TypeAlias(TypeAlias::try_from(inner)?),
			Rule::var_name => RawTerm::VarName(inner.as_str().into()),
			_ => {
				log::error!(
					"[{}] {}\n{:?}",
					line!(),
					inner.as_str(),
					inner.as_rule()
				);
				bail!(Error::ParseError(line!()))
			}
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for BinaryOperator {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		let res = match pair.as_rule() {
			Rule::exp => BinaryOperator::Exp,
			Rule::times => BinaryOperator::Mul,
			Rule::div => BinaryOperator::Div,
			Rule::rem => BinaryOperator::Rem,
			Rule::plus => BinaryOperator::Add,
			Rule::minus => BinaryOperator::Sub,
			Rule::delta => BinaryOperator::Delta,
			Rule::and => BinaryOperator::And,
			Rule::or => BinaryOperator::Or,
			Rule::xor => BinaryOperator::Xor,
			Rule::dot => BinaryOperator::Dot,
			Rule::eq => BinaryOperator::Eq,
			Rule::neq => BinaryOperator::Neq,
			Rule::greater => BinaryOperator::Gt,
			Rule::greater_eq => BinaryOperator::Gte,
			Rule::less => BinaryOperator::Lt,
			Rule::less_eq => BinaryOperator::Lte,
			Rule::lshift => BinaryOperator::LShift,
			Rule::rshift => BinaryOperator::RShift,
			_ => {
				log::error!("{:#?}\n{:#?}", pair, pair.as_rule());
				bail!(Error::ParseError(line!()))
			}
		};

		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for UnaryOperator {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert!(matches!(pair.as_rule(), Rule::negate | Rule::not));
		let res = match pair.as_rule() {
			Rule::not => UnaryOperator::Not,
			Rule::negate => UnaryOperator::Sub,
			_ => bail!(Error::ParseError(line!())),
		};

		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for Argument {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::argument);
		let inner = pair.into_inner().collect::<SmallVec<[_; 2]>>();
		let res = match inner.as_slice() {
			[name, type_name] => {
				let name = name.as_str().into();
				let type_name = Type::try_from(type_name.clone())?;

				Argument { name, type_name }
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for FunctionDefinition {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::function_definition);
		let span = pair.as_span();
		log::trace!("[{}] {}", line!(), pair.as_str());
		let inner = pair.into_inner().collect::<SmallVec<[_; 4]>>();
		log::trace!("[{}] {:#?}", line!(), &inner);
		let res = match inner.as_slice() {
			[_, args, type_name, expr] => {
				let name = {
					let mut s = SmallString::new();
					write!(&mut s, "lambda_{}_{}", span.start(), span.end())?;
					s
				};
				let args = ArgumentList::try_from(args.clone())?;
				let return_type = ReturnType::try_from(type_name.clone())?
					.0
					.unwrap_or_else(|| EnumType(smallvec![RawType::Unit]));
				let expr = Expr::try_from(expr.clone())?;

				FunctionDefinition {
					name,
					args,
					return_type,
					expr,
				}
			}
			[_, name, args, type_name, expr] => {
				let name = name.as_str().into();
				let args = ArgumentList::try_from(args.clone())?;
				let return_type = ReturnType::try_from(type_name.clone())?
					.0
					.unwrap_or_else(|| EnumType(smallvec![RawType::Unit]));
				let expr = Expr::try_from(expr.clone())?;

				FunctionDefinition {
					name,
					args,
					return_type,
					expr,
				}
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for ReturnType {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::return_type);
		let inner = pair.into_inner().collect::<SmallVec<[_; 2]>>();
		let res = match inner.as_slice() {
			[_, enum_type] => {
				let enum_type = EnumType::try_from(enum_type.clone())?;
				Some(enum_type)
			}
			[] => None,
			_ => {
				log::error!("[{}] {:?}", line!(), &inner);
				bail!(Error::ParseError(line!()))
			}
		};
		Ok(ReturnType(res))
	}
}

impl TryFrom<Pair<'_, Rule>> for ArgumentList {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::argument_list);
		pair.into_inner()
			.map(Argument::try_from)
			.collect::<Result<_>>()
			.map(ArgumentList)
	}
}

impl TryFrom<Pair<'_, Rule>> for Block {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::block);
		pair.into_inner()
			.map(Expr::try_from)
			.collect::<Result<_>>()
			.map(Block)
	}
}

impl TryFrom<Pair<'_, Rule>> for StructField {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::struct_field);
		let inner = pair.into_inner().collect::<SmallVec<[_; 2]>>();
		let res = match inner.as_slice() {
			[name, type_name] => {
				let name = name.as_str().into();
				let type_name = EnumType::try_from(type_name.clone())?;

				StructField { name, type_name }
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for StructType {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::struct_type);
		pair.into_inner()
			.map(StructField::try_from)
			.collect::<Result<_>>()
			.map(StructType)
	}
}

impl TryFrom<Pair<'_, Rule>> for EnumType {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::enum_type);
		pair.into_inner()
			.map(RawType::try_from)
			.collect::<Result<_>>()
			.map(EnumType)
	}
}

impl TryFrom<Pair<'_, Rule>> for TypeAlias {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::type_alias);
		let inner = pair.into_inner().collect::<SmallVec<[_; 3]>>();
		let res = match inner.as_slice() {
			[name, type_literal] => {
				let name = name.as_str().into();
				let mutable = false;
				let type_name = EnumType::try_from(type_literal.clone())?;

				TypeAlias {
					name,
					mutable,
					type_name,
				}
			}
			[name, mutable, type_literal] => {
				let name = name.as_str().into();
				let mutable = mutable.as_str().starts_with("mut");
				let type_name = EnumType::try_from(type_literal.clone())?;

				TypeAlias {
					name,
					mutable,
					type_name,
				}
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for StructFieldLiteral {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::struct_field_use);
		let inner = pair.into_inner().collect::<SmallVec<[_; 2]>>();
		let res = match inner.as_slice() {
			[name] => {
				let name: SmallString = name.as_str().into();
				let expr: Expr = RawTerm::VarName(name.clone()).into();

				StructFieldLiteral { name, expr }
			}
			[name, expr] => {
				let name = name.as_str().into();
				let expr = Expr::try_from(expr.clone())?;

				StructFieldLiteral { name, expr }
			}
			_ => bail!(Error::ParseError(line!())),
		};
		Ok(res)
	}
}

impl TryFrom<Pair<'_, Rule>> for StructLiteral {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		assert_eq!(pair.as_rule(), Rule::struct_literal);
		pair.into_inner()
			.map(StructFieldLiteral::try_from)
			.collect::<Result<_>>()
			.map(StructLiteral)
	}
}

/// Get the first and only inner rule
/// Errors if there is more or less than one inner rule
fn inner(pair: Pair<'_, Rule>) -> Result<Pair<'_, Rule>> {
	let mut inner = pair.into_inner();
	let first = inner.next().ok_or(Error::Internal(line!()))?;
	if inner.next().is_some() {
		bail!(Error::Internal(line!()))
	}
	Ok(first)
}

pub fn order(mut pairs: Pairs<Rule>) -> Result<Vec<Expr>> {
	let count = pairs.clone().count();
	assert_eq!(count, 1);

	pairs.next()
		.unwrap()
		.into_inner()
		.take_while(|p| p.as_rule() != Rule::EOI)
		.map(Expr::try_from)
		.collect()
}
