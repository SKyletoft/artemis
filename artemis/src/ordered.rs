use std::mem;

use anyhow::{bail, Result};
use derive_more::From;
use pest::{
	iterators::{Pair, Pairs},
	Parser,
};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{error::Error, GeneratedParser, Rule};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq)]
struct Type {
	mutable: bool,
	enum_type: EnumType,
}

impl TryFrom<Pair<'_, Rule>> for Type {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<Rule>) -> Result<Self, Self::Error> {
		if pair.as_rule() != Rule::type_name {
			bail!(Error::ParseError(line!()));
		}
		dbg!(pair.into_inner());
		todo!()
	}
}

#[derive(Debug, Clone, PartialEq)]
enum RawType {}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {}

impl TryFrom<Pair<'_, Rule>> for Expr {
	type Error = anyhow::Error;

	fn try_from(pair: Pair<'_, Rule>) -> Result<Self, Self::Error> {
		let inner = inner(pair)?;
		match inner.as_rule() {
			Rule::declaration => todo!(),
			Rule::assignment => todo!(),
			Rule::subexpr => todo!(),
			Rule::unary_expr => todo!(),
			Rule::function_definition => todo!(),
			Rule::type_alias => todo!(),
			_ => {
				log::error!("{}\n{:?}", inner.as_str(), inner.as_rule());
				bail!(Error::ParseError(line!()))
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
enum EnumType {}

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
		.map(Expr::try_from)
		.collect()
}
