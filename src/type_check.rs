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

pub fn check_program(top_level: &[TopLevelConstruct]) -> Result<()> {
	todo!()
}
