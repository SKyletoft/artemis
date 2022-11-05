use std::{collections::HashMap, rc::Rc};

use anyhow::{bail, Result};
use derive_more::From;
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};
use variantly::Variantly;

use crate::{
	ast::{self, ActualType, EnumType, RawType, StructType, Type},
	error::Error,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
	pub(crate) variables: HashMap<SmallString, ActualType2>,
	pub(crate) types: HashMap<SmallString, EnumType2>,
}

impl Context {
	pub fn new() -> Self {
		Self {
			variables: HashMap::new(),
			types: HashMap::new(),
		}
	}

	pub fn join(&mut self, other: Context) {
		for (key, val) in other.variables.into_iter() {
			self.variables.insert(key, val);
		}
		for (key, val) in other.types.into_iter() {
			self.types.insert(key, val);
		}
	}

	pub fn try_insert(&mut self, label: &Option<SmallString>, val: ActualType2) {
		if let Some(lbl) = label {
			self.variables.insert(lbl.clone(), val);
		}
	}
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum ActualType2 {
	Declared(Type2),
	Inferred(Rc<Type2>),
}

impl From<EnumType2> for ActualType2 {
	fn from(enum_type: EnumType2) -> Self {
		ActualType2::Declared(enum_type.into())
	}
}

impl From<RawType2> for ActualType2 {
	fn from(raw_type: RawType2) -> Self {
		ActualType2::Declared(raw_type.into())
	}
}

impl ActualType2 {
	pub fn try_from_ast_type(ast_type: &Type, context: &Context) -> Result<Self> {
		Type2::try_from_ast(ast_type, context).map(ActualType2::Declared)
	}

	pub fn try_from_ast(ast_actual_type: &ActualType, context: &Context) -> Result<Self> {
		match ast_actual_type {
			ActualType::Declared(t) => ActualType2::try_from_ast_type(t, context),
			ActualType::Inferred => Ok(ActualType2::Inferred(Rc::new(Type2 {
				mutable: false,
				enum_type: SmallVec::new().into(),
			}))),
		}
	}

	pub fn contains(&self, other: &EnumType2) -> bool {
		match self {
			ActualType2::Declared(t) => t.enum_type.contains(other),
			ActualType2::Inferred(_) => true,
		}
	}

	pub fn or(self, other: EnumType2) -> Type2 {
		match self {
			ActualType2::Declared(t) => t,
			ActualType2::Inferred(t) => t.enum_type.clone().join(other).into(),
		}
	}

	pub fn inner(self) -> Type2 {
		match self {
			ActualType2::Declared(t) => t,
			ActualType2::Inferred(t) => t.as_ref().clone(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type2 {
	pub(crate) mutable: bool,
	pub(crate) enum_type: EnumType2,
}

impl From<EnumType2> for Type2 {
	fn from(enum_type: EnumType2) -> Self {
		Type2 {
			mutable: false,
			enum_type,
		}
	}
}

impl From<RawType2> for Type2 {
	fn from(raw_type: RawType2) -> Self {
		EnumType2::from(raw_type).into()
	}
}

impl Type2 {
	pub fn try_from_ast(ast_type: &Type, context: &Context) -> Result<Self> {
		let enum_type = EnumType2::try_from_ast(&ast_type.enum_type, context)?;
		let mutable = ast_type.mutable;
		let res = Type2 { mutable, enum_type };

		Ok(res)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Variantly)]
pub enum RawType2 {
	Natural,
	Real,
	Integer,
	NumberLiteral,
	Bool,
	Any,
	Type,
	Unit,
	Tuple(Vec<EnumType2>),
	StructType(StructType2),
	EnumType(Box<EnumType2>),
	FunctionType {
		args: Vec<EnumType2>,
		ret: Box<EnumType2>,
	},
}

impl RawType2 {
	pub fn try_from_ast(ast: &RawType, ctx: &Context) -> Result<Self> {
		let res = match ast {
			RawType::StructNameOrAlias(name) => Self::EnumType(Box::new(
				ctx.types
					.get(name)
					.ok_or(Error::UndefinedTypeAlias(line!()))?
					.clone(),
			)),
			RawType::Natural => Self::Natural,
			RawType::Real => Self::Real,
			RawType::Integer => Self::Integer,
			RawType::Bool => Self::Bool,
			RawType::Any => Self::Any,
			RawType::Type => Self::Type,
			RawType::Unit => Self::Unit,
			RawType::Tuple(_) => todo!(),
			RawType::StructType(s) => {
				Self::StructType(StructType2::try_from_ast(s, ctx)?)
			}
		};
		Ok(res)
	}

	pub fn int_eq(&self, rhs: &Self) -> bool {
		match (self, rhs) {
			(Self::NumberLiteral, Self::Integer)
			| (Self::NumberLiteral, Self::Natural)
			| (Self::Integer, Self::NumberLiteral)
			| (Self::Natural, Self::NumberLiteral) => true,
			(a, b) => a == b,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField2 {
	pub(crate) name: SmallString,
	pub(crate) type_name: EnumType2,
}

impl PartialOrd for StructField2 {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.name.partial_cmp(&other.name)
	}
}

impl Ord for StructField2 {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.name.cmp(&other.name)
	}
}

impl StructField2 {
	pub fn try_from_ast(ast: &ast::StructField, ctx: &Context) -> Result<Self> {
		let name = ast.name.clone();
		let type_name = EnumType2::try_from_ast(&ast.type_name, ctx)?;
		Ok(Self { name, type_name })
	}
}

#[derive(Debug, Clone, PartialEq, From, Eq)]
pub struct EnumType2(pub(crate) SmallVec<[RawType2; 1]>);

impl From<RawType2> for EnumType2 {
	fn from(r: RawType2) -> Self {
		EnumType2(smallvec![r])
	}
}

impl EnumType2 {
	/// Check that all types in the right enum are in the left enum
	pub fn contains(&self, rhs: &EnumType2) -> bool {
		rhs.0.iter().all(|x| self.0.iter().any(|y| y.int_eq(x)))
	}

	pub fn try_from_ast(ast: &EnumType, ctx: &Context) -> Result<Self> {
		let contained =
			ast.0.iter()
				.filter(|t| !t.is_struct_name_or_alias())
				.map(|t| RawType2::try_from_ast(t, ctx))
				.collect::<Result<Vec<_>>>()?
				.into_iter();
		let flattened =
			ast.0.iter()
				.filter(|t| t.is_struct_name_or_alias())
				.map(|t| {
					let inner = RawType2::try_from_ast(t, ctx)?
						.enum_type()
						.ok_or(Error::Internal(line!()))?
						.0
						.into_iter();
					Ok(inner)
				})
				.collect::<Result<Vec<_>>>()?
				.into_iter()
				.flatten();
		let res = contained.chain(flattened).collect::<_>();
		Ok(EnumType2(res))
	}

	pub fn join(self, else_type: EnumType2) -> EnumType2 {
		let EnumType2(mut l_inner) = self;
		let EnumType2(r_inner) = else_type;

		for elem in r_inner.into_iter() {
			if l_inner.contains(&elem) {
				continue;
			}
			l_inner.push(elem);
		}

		EnumType2(l_inner)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType2(pub(crate) Vec<StructField2>);

impl StructType2 {
	pub fn try_from_ast(ast: &StructType, ctx: &Context) -> Result<Self> {
		let mut ret =
			ast.0.iter()
				.map(|t| StructField2::try_from_ast(t, ctx))
				.collect::<Result<Vec<_>>>()?;
		ret.sort_unstable();

		Ok(StructType2(ret))
	}
}