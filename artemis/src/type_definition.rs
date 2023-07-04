use std::{
	collections::{hash_map::DefaultHasher, HashMap},
	fmt,
	hash::{Hash, Hasher},
	rc::Rc,
};

use anyhow::Result;
use derive_more::From;
use smallvec::{smallvec, SmallVec};
use variantly::Variantly;

use crate::{
	ast::{self, ActualType, EnumType, RawType, StructType, Type},
	error::Error,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
	pub(crate) variables: HashMap<SmallString, ActualType2>,
	pub(crate) types: HashMap<SmallString, EnumType2>,
	pub(crate) globals: HashMap<SmallString, ActualType2>,
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	    writeln!(f, "Context: {{\n\tvariables: [")?;
	    for (key, typ) in self.variables.iter() {
		    writeln!(f, "\t\t{key}: {typ}")?;
	    }
	    writeln!(f, "\t],\n\ttypes: [")?;
	    for (key, typ) in self.types.iter() {
		    writeln!(f, "\t\t{key}: {typ}")?;
	    }
	    writeln!(f, "\t],\n\tglobals: [")?;
	    for (key, typ) in self.globals.iter() {
		    writeln!(f, "\t\t{key}: {typ}")?;
	    }
	    write!(f, "\t]\n}}")
    }
}

impl Context {
	pub fn new() -> Self {
		Self {
			variables: HashMap::new(),
			types: HashMap::new(),
			globals: HashMap::new(),
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

	pub fn with_builtins() -> Self {
		// TODO: Turn this function into a prelude in the standard library
		let mut ret = Context::default();

		ret.globals.insert(
			"print_n".into(),
			RawType2::FunctionType {
				args: RawType2::Natural.into(),
				ret: RawType2::Unit.into(),
			}
			.into(),
		);
		ret.globals.insert(
			"print_z".into(),
			RawType2::FunctionType {
				args: RawType2::Integer.into(),
				ret: RawType2::Unit.into(),
			}
			.into(),
		);
		ret.globals.insert(
			"print_r".into(),
			RawType2::FunctionType {
				args: RawType2::Real.into(),
				ret: RawType2::Unit.into(),
			}
			.into(),
		);
		ret.globals.insert(
			"print_b".into(),
			RawType2::FunctionType {
				args: RawType2::Bool.into(),
				ret: RawType2::Unit.into(),
			}
			.into(),
		);
		ret.types.insert("Nat".into(), RawType2::Natural.into());
		ret.types.insert("Int".into(), RawType2::Integer.into());
		ret.types.insert("Real".into(), RawType2::Real.into());
		ret.types.insert("Bool".into(), RawType2::Bool.into());

		ret
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Variantly)]
pub enum ActualType2 {
	Declared(Type2),
	Inferred(Rc<Type2>),
}

impl fmt::Display for ActualType2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let inner = self.inner_ref();
		write!(f, "{inner}")
	}
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

impl From<Type2> for ActualType2 {
	fn from(type2: Type2) -> Self {
		ActualType2::Declared(type2)
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

	pub fn inner_ref(&self) -> &Type2 {
		match self {
			ActualType2::Declared(t) => t,
			ActualType2::Inferred(t) => t.as_ref(),
		}
	}

	pub fn mutable(&self) -> bool {
		self.inner_ref().mutable
	}

	pub fn enum_type(&self) -> &EnumType2 {
		&self.inner_ref().enum_type
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type2 {
	pub(crate) mutable: bool,
	pub(crate) enum_type: EnumType2,
}

impl fmt::Display for Type2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.mutable {
			write!(f, "mut ")?;
		}
		write!(f, "{}", self.enum_type)
	}
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

#[derive(Debug, Clone, PartialEq, Eq, Variantly, Hash)]
pub enum RawType2 {
	Real,
	Natural,
	Integer,
	Bool,
	Any,
	Type,
	Interface,
	Unit,
	Tuple(Vec<EnumType2>),
	StructType(StructType2),
	EnumType(Box<EnumType2>),
	FunctionType {
		args: Vec<EnumType2>,
		ret: Box<EnumType2>,
	},
}

impl From<RawType2> for Vec<EnumType2> {
	fn from(val: RawType2) -> Self {
		vec![val.into()]
	}
}

impl From<RawType2> for Box<EnumType2> {
	fn from(val: RawType2) -> Self {
		Box::new(val.into())
	}
}

impl fmt::Display for RawType2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RawType2::Natural => write!(f, "ℕ"),
			RawType2::Real => write!(f, "ℝ"),
			RawType2::Integer => write!(f, "ℤ"),
			RawType2::Bool => write!(f, "𝔹"),
			RawType2::Any => write!(f, "∀"),
			RawType2::Type => write!(f, "𝕋"),
			RawType2::Interface => write!(f, "Iterface"),
			RawType2::Unit => write!(f, "()"),
			RawType2::Tuple(_) => todo!(),
			RawType2::StructType(StructType2(fields)) => {
				write!(f, "{{")?;
				match fields.as_slice() {
					[] => write!(f, "∅")?,
					[x] => write!(f, "{x}")?,
					[x, xs @ ..] => {
						write!(f, "{x}")?;
						for x in xs.iter() {
							write!(f, ", {x}")?;
						}
					}
				}
				write!(f, "}}")
			}
			RawType2::EnumType(e) => write!(f, "{e}"),
			RawType2::FunctionType { args, ret } => {
				write!(f, "λ(")?;
				match args.as_slice() {
					[] => write!(f, "∅")?,
					[x] => write!(f, "{x}")?,
					[x, xs @ ..] => {
						write!(f, "{x}")?;
						for x in xs.iter() {
							write!(f, ", {x}")?;
						}
					}
				}
				write!(f, ") → {ret}")
			}
		}
	}
}

impl RawType2 {
	pub fn try_from_ast(ast: &RawType, ctx: &Context) -> Result<Self> {
		let res = match ast {
			RawType::StructNameOrAlias(name) => Self::EnumType(Box::new(
				ctx.types
					.get(name)
					.ok_or_else(|| {
						log::debug!("{ctx:#?}\n{name}");
						Error::UndefinedTypeAlias(line!())
					})?
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

	pub fn id(&self) -> u64 {
		let mut hasher = DefaultHasher::new();
		self.hash(&mut hasher);
		hasher.finish()
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField2 {
	pub(crate) name: SmallString,
	pub(crate) type_name: EnumType2,
}

impl fmt::Display for StructField2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}: {}", self.name, self.type_name)
	}
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

#[derive(Debug, Clone, PartialEq, From, Eq, Hash)]
pub struct EnumType2(pub(crate) SmallVec<[RawType2; 1]>);

impl fmt::Display for EnumType2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0.as_slice() {
			[] => write!(f, "∅"),
			[x] => write!(f, "{x}"),
			[x, xs @ ..] => {
				write!(f, "{x}")?;
				for x in xs.iter() {
					write!(f, "| {x}")?;
				}
				Ok(())
			}
		}
	}
}

impl From<&RawType2> for EnumType2 {
	fn from(r: &RawType2) -> Self {
		r.clone().into()
	}
}

impl From<RawType2> for EnumType2 {
	fn from(r: RawType2) -> Self {
		EnumType2(smallvec![r])
	}
}

impl EnumType2 {
	pub fn get_only(&self) -> Option<&RawType2> {
		match self.0.as_slice() {
			[x] => Some(x),
			_ => None,
		}
	}

	/// Check that all types in the right enum are in the left enum
	pub fn contains(&self, rhs: &EnumType2) -> bool {
		rhs.0.iter().all(|x| self.0.iter().any(|y| x == y))
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

	pub fn id(&self) -> u64 {
		let mut hasher = DefaultHasher::new();
		self.hash(&mut hasher);
		hasher.finish()
	}

	pub fn get_field(&self, name: &str) -> Result<EnumType2> {
		let mut e = EnumType2(SmallVec::new());
		for t in self.0.iter().map(|t| match t {
			RawType2::StructType(StructType2(fields)) => fields
				.iter()
				.find(|f| f.name == name)
				.map(|f| &f.type_name)
				.ok_or(Error::UnknownStructField(line!())),
			RawType2::EnumType(inner) => Ok(inner.as_ref()),
			_ => Err(Error::NotAStructType(line!())),
		}) {
			e = e.join(t?.clone());
		}
		Ok(e)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
