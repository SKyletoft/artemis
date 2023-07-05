use std::fmt;

use anyhow::Result;
use derive_more::From;
use smallvec::{smallvec, SmallVec};
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub(crate) mutable: bool,
	pub(crate) enum_type: EnumType,
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.mutable {
			write!(f, "mut ")?;
		}
		write!(f, "{}", self.enum_type)
	}
}

impl From<EnumType> for Type {
	fn from(enum_type: EnumType) -> Self {
		Self {
			mutable: false,
			enum_type,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Variantly, Default)]
pub enum ActualType {
	Declared(Type),
	#[default]
	Inferred,
}

impl fmt::Display for ActualType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ActualType::Declared(t) => write!(f, "{t}"),
			ActualType::Inferred => write!(f, "_"),
		}
	}
}

impl From<Type> for ActualType {
	fn from(value: Type) -> Self {
		ActualType::Declared(value.into())
	}
}

impl From<EnumType> for ActualType {
	fn from(value: EnumType) -> Self {
		ActualType::Declared(value.into())
	}
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum RawType {
	Natural,
	Real,
	Integer,
	Bool,
	Any,
	Type,
	StructNameOrAlias(SmallString),
	Unit,
	Tuple(Vec<EnumType>),
	StructType(StructType),
}

impl fmt::Display for RawType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RawType::Natural => write!(f, "ℕ"),
			RawType::Real => write!(f, "ℝ"),
			RawType::Integer => write!(f, "ℤ"),
			RawType::Bool => write!(f, "𝔹"),
			RawType::Any => write!(f, "∀"),
			RawType::Type => write!(f, "𝕋"),
			RawType::Unit => write!(f, "()"),
			RawType::Tuple(_) => todo!(),
			RawType::StructType(StructType(fields)) => {
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
			RawType::StructNameOrAlias(n) => write!(f, "{n}"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple(pub(crate) Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub(crate) pattern: Pattern,
	pub(crate) type_name: ActualType,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub(crate) pattern: Pattern,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub(crate) func: Expr,
	pub(crate) args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	pub(crate) condition: Expr,
	pub(crate) then_branch: Expr,
	pub(crate) else_branch: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
	pub(crate) expr: Expr,
	pub(crate) cases: Vec<Case>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
	pub(crate) pattern: Pattern,
	pub(crate) condition: Option<Expr>,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
	pub(crate) label: Option<SmallString>,
	pub(crate) inner: InnerPattern,
	pub(crate) irrefutable: bool,
}

impl fmt::Display for Pattern {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let Pattern {
			label,
			inner,
			irrefutable,
		} = self;
		let end = if *irrefutable { "!" } else { "" };
		match label {
			Some(l) => write!(f, "{l} @ {inner}{end}"),
			None => write!(f, "{inner}{end}"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum InnerPattern {
	StructPattern(StructPattern),
	TuplePattern(TuplePattern),
	Float(f64),
	Natural(u64),
	Integer(i64),
	Boolean(bool),
	String(SmallString),
	Char(char),
	Var(SmallString),
	Any,
}

impl fmt::Display for InnerPattern {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			InnerPattern::StructPattern(StructPattern { fields, more }) => {
				write!(f, "{{")?;
				match fields.as_slice() {
					[] => {}
					[x] => write!(f, "{x}")?,
					[x, xs @ ..] => {
						write!(f, "{x}")?;
						for x in xs.iter() {
							write!(f, ", {x}")?;
						}
					}
				}
				if *more {
					write!(f, ", ..")?;
				}
				write!(f, "}}")
			}
			InnerPattern::TuplePattern(t) => write!(f, "(tuple {t:#?})"),
			InnerPattern::Float(d) => write!(f, "{d}"),
			InnerPattern::Natural(n) => write!(f, "{n}"),
			InnerPattern::Integer(i) => write!(f, "{i}"),
			InnerPattern::Boolean(b) => write!(f, "{b}"),
			InnerPattern::String(s) => write!(f, "\"{s}\""),
			InnerPattern::Char(n) => write!(f, "'{n}'"),
			InnerPattern::Var(n) => write!(f, "{n}"),
			InnerPattern::Any => write!(f, "_"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldPattern {
	pub(crate) label: Option<SmallString>,
	pub(crate) name: SmallString,
	pub(crate) pattern: Option<Pattern>,
}

impl fmt::Display for StructFieldPattern {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let StructFieldPattern {
			label,
			name,
			pattern,
		} = self;
		if let Some(l) = label {
			write!(f, "{l} @ ")?
		}
		match pattern {
			Some(p) => write!(f, "{name}: {p}"),
			None => write!(f, "{name}"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
	pub(crate) fields: Vec<StructFieldPattern>,
	pub(crate) more: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern(pub(crate) Vec<Pattern>);

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Expr {
	BinOp {
		left: Box<Expr>,
		right: Box<Expr>,
		op: BinaryOperator,
	},
	UnOp {
		op: UnaryOperator,
		right: Box<Expr>,
	},
	Leaf(Box<Term>),
}

impl Default for Expr {
	fn default() -> Self {
		Expr::Leaf(Box::new(Term {
			raw_term: RawTerm::Unit,
			type_ascription: None,
		}))
	}
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expr::BinOp { left, right, op } => write!(f, "{left} {op} {right}"),
			Expr::UnOp { op, right } => write!(f, "{op}{right}"),
			Expr::Leaf(l) => write!(f, "{l}"),
		}
	}
}

impl From<Term> for Expr {
	fn from(t: Term) -> Self {
		Expr::Leaf(Box::new(t))
	}
}

impl From<RawTerm> for Expr {
	fn from(raw_term: RawTerm) -> Self {
		Expr::Leaf(Box::new(Term {
			raw_term,
			type_ascription: None,
		}))
	}
}

impl Expr {
	pub fn leaf_ref(&self) -> Option<&Term> {
		match self {
			Expr::Leaf(l) => Some(&l),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Term {
	pub(crate) raw_term: RawTerm,
	pub(crate) type_ascription: Option<EnumType>,
}

impl fmt::Display for Term {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Term {
				raw_term,
				type_ascription: None,
			} => write!(f, "{raw_term}"),
			Term {
				raw_term,
				type_ascription: Some(typ),
			} => write!(f, "({raw_term}: {typ})"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawTerm {
	Float(f64),
	Natural(u64),
	Integer(i64),
	Boolean(bool),
	String(SmallString),
	Char(char),
	Unit,
	Tuple(Tuple),
	StructLiteral(StructLiteral),
	Block(Block),
	IfExpr(IfExpr),
	MatchExpr(MatchExpr),
	FunctionCall(FunctionCall),
	Lambda(Lambda),
	Declaration(Declaration),
	Assignment(Assignment),
	FunctionDefinition(FunctionDefinition),
	TypeAlias(TypeAlias),
	VarName(SmallString),
}

impl fmt::Display for RawTerm {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			RawTerm::Float(d) => write!(f, "{d}"),
			RawTerm::Natural(n) => write!(f, "{n}"),
			RawTerm::Integer(i) => write!(f, "{i}"),
			RawTerm::Boolean(b) => write!(f, "{b}"),
			RawTerm::String(_) => todo!(),
			RawTerm::Char(_) => todo!(),
			RawTerm::Unit => write!(f, "()"),
			RawTerm::Tuple(t) => write!(f, "(tuple {t:#?})"),
			RawTerm::StructLiteral(StructLiteral(s)) => {
				write!(f, "{{")?;
				match s.as_slice() {
					[] => write!(f, "")?,
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
			RawTerm::Block(Block(exprs)) => {
				write!(f, "(")?;
				for expr in exprs.iter() {
					write!(f, "\n\t{expr}")?;
				}
				write!(f, "\n)")
			}
			RawTerm::IfExpr(_) => todo!(),
			RawTerm::MatchExpr(_) => todo!(),
			RawTerm::FunctionCall(FunctionCall { func, args }) => {
				write!(f, "{func}(")?;
				match args.as_slice() {
					[] => write!(f, "")?,
					[x] => write!(f, "{x}")?,
					[x, xs @ ..] => {
						write!(f, "{x}")?;
						for x in xs.iter() {
							write!(f, ", {x}")?;
						}
					}
				}
				write!(f, ")")
			}
			RawTerm::Declaration(Declaration {
				pattern,
				type_name,
				expr,
			}) => {
				write!(f, "{pattern} : {type_name} = {expr}")
			}
			RawTerm::Assignment(_) => todo!(),
			RawTerm::Lambda(_) => todo!(),
			RawTerm::FunctionDefinition(FunctionDefinition {
				name,
				args,
				return_type,
				expr,
			}) => {
				write!(f, "λ{name} ({args}) → {return_type} = {expr}")
			}
			RawTerm::TypeAlias(t) => write!(f, "{t}"),
			RawTerm::VarName(n) => write!(f, "{n}"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
	Exp,
	Mul,
	Div,
	Rem,
	Add,
	Sub,
	Delta,
	And,
	Or,
	Xor,
	Dot,
	Eq,
	Neq,
	Gt,
	Lt,
	Gte,
	Lte,
	LShift,
	RShift,
}

impl fmt::Display for BinaryOperator {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			BinaryOperator::Exp => write!(f, "^"),
			BinaryOperator::Mul => write!(f, "×"),
			BinaryOperator::Div => write!(f, "÷"),
			BinaryOperator::Rem => write!(f, "%"),
			BinaryOperator::Add => write!(f, "+"),
			BinaryOperator::Sub => write!(f, "-"),
			BinaryOperator::Delta => write!(f, "Δ"),
			BinaryOperator::And => write!(f, "Λ"),
			BinaryOperator::Or => write!(f, "V"),
			BinaryOperator::Xor => write!(f, "⊕"),
			BinaryOperator::Dot => write!(f, "."),
			BinaryOperator::Eq => write!(f, "=="),
			BinaryOperator::Neq => write!(f, "≠"),
			BinaryOperator::Gt => write!(f, ">"),
			BinaryOperator::Lt => write!(f, "<"),
			BinaryOperator::Gte => write!(f, ">="),
			BinaryOperator::Lte => write!(f, "<="),
			BinaryOperator::LShift => write!(f, "<<"),
			BinaryOperator::RShift => write!(f, ">>"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
	Not,
	Sub,
	NatCast,
	IntCast,
	RealCast,
}

impl fmt::Display for UnaryOperator {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			UnaryOperator::Not => write!(f, "¬"),
			UnaryOperator::Sub => write!(f, "-"),
			UnaryOperator::NatCast => write!(f, "@ℕ"),
			UnaryOperator::IntCast => write!(f, "@ℤ"),
			UnaryOperator::RealCast => write!(f, "@ℝ"),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub(crate) name: SmallString,
	pub(crate) type_name: Type,
}

impl fmt::Display for Argument {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let Argument { name, type_name } = self;
		write!(f, "{}: {}", name, type_name)
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct FunctionDefinition {
	pub(crate) name: SmallString,
	pub(crate) args: ArgumentList,
	pub(crate) return_type: EnumType,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Lambda {
	pub(crate) captures: Vec<SmallString>,
	pub(crate) args: ArgumentList,
	pub(crate) return_type: ActualType,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Default, From)]
pub struct ReturnType(pub(crate) Option<EnumType>);

#[derive(Debug, Clone, PartialEq, Default, From)]
pub struct ArgumentList(pub(crate) SmallVec<[Argument; 1]>);

impl fmt::Display for ArgumentList {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0.as_slice() {
			[] => write!(f, ""),
			[x] => write!(f, "{x}"),
			[x, xs @ ..] => {
				write!(f, "{x}")?;
				for x in xs.iter() {
					write!(f, "{x}")?;
				}
				Ok(())
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub(crate) Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
	pub(crate) name: SmallString,
	pub(crate) type_name: EnumType,
}

impl fmt::Display for StructField {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let StructField { name, type_name } = self;
		write!(f, "{}: {}", name, type_name)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType(pub(crate) Vec<StructField>);

#[derive(Debug, Clone, PartialEq, From, Default)]
pub struct EnumType(pub(crate) SmallVec<[RawType; 1]>);

impl From<RawType> for EnumType {
	fn from(value: RawType) -> Self {
		EnumType(smallvec![value])
	}
}

impl fmt::Display for EnumType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0.as_slice() {
			[] => write!(f, "Empty Type"),
			[x] => write!(f, "{x}"),
			[x, xs @ ..] => {
				write!(f, "{x}")?;
				for x in xs.iter() {
					write!(f, " | {x}")?;
				}
				Ok(())
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
	pub(crate) name: SmallString,
	pub(crate) mutable: bool,
	pub(crate) type_name: EnumType,
}

impl fmt::Display for TypeAlias {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let TypeAlias {
			name,
			mutable,
			type_name,
		} = self;
		if *mutable {
			write!(f, "{name} : mut Type = {type_name}")
		} else {
			write!(f, "{name} := {type_name}")
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldLiteral {
	pub(crate) name: SmallString,
	pub(crate) expr: Expr,
}

impl fmt::Display for StructFieldLiteral {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let StructFieldLiteral { name, expr } = self;
		write!(f, "{name}: {expr}")
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral(pub(crate) Vec<StructFieldLiteral>);
