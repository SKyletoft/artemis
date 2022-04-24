use std::collections::HashMap;

use anyhow::{bail, Result};
use derive_more::{Add, AddAssign, From};
use smallvec::SmallVec;
use variantly::Variantly;

use crate::{
	error::Error,
	ordered::{BinOp, FunctionCall, IfExpr, Literal, Op, Subexpr},
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
pub struct Register(u32);

#[derive(Debug, Clone, Copy, Add, PartialEq, Eq, PartialOrd, Ord, AddAssign, Default, From)]
pub struct BlockId(u32);
