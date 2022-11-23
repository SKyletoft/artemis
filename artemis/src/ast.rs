use std::fmt;

use anyhow::Result;
use derive_more::From;
use smallvec::SmallVec;
use variantly::Variantly;

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub(crate) mutable: bool,
	pub(crate) enum_type: EnumType,
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum ActualType {
	Declared(Type),
	Inferred,
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
pub struct PartialApplication {
	pub(crate) func: Expr,
	pub(crate) args: Vec<Option<Expr>>,
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

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum InnerPattern {
	StructPattern(StructPattern),
	TuplePattern(TuplePattern),
	Float(f64),
	Integer(i64),
	Boolean(bool),
	String(SmallString),
	Char(char),
	Var(SmallString),
	Any,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldPattern {
	pub(crate) name: SmallString,
	pub(crate) pattern: Option<Pattern>,
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
	PartialApplication(PartialApplication),
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
			RawTerm::Integer(i) => write!(f, "{i}"),
			RawTerm::Boolean(b) => write!(f, "{b}"),
			RawTerm::String(_) => todo!(),
			RawTerm::Char(_) => todo!(),
			RawTerm::Unit => write!(f, "()"),
			RawTerm::Tuple(t) => write!(f, "(tuple)"),
			RawTerm::StructLiteral(s) => write!(f, "(struct)"),
			RawTerm::Block(b) => f.debug_tuple("Block").field(b).finish(),
			RawTerm::IfExpr(_) => todo!(),
			RawTerm::MatchExpr(_) => todo!(),
			RawTerm::FunctionCall(_) => todo!(),
			RawTerm::PartialApplication(_) => todo!(),
			RawTerm::Declaration(Declaration {
				pattern,
				type_name,
				expr,
			}) => {
				write!(f, "{pattern} : {type_name} = {expr}")
			}
			RawTerm::Assignment(_) => todo!(),
			RawTerm::FunctionDefinition(_) => todo!(),
			RawTerm::TypeAlias(_) => todo!(),
			RawTerm::VarName(_) => todo!(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
	Not,
	Sub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub(crate) name: SmallString,
	pub(crate) type_name: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
	pub(crate) name: SmallString,
	pub(crate) args: ArgumentList,
	pub(crate) return_type: EnumType,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnType(pub(crate) Option<EnumType>);

#[derive(Debug, Clone, PartialEq)]
pub struct ArgumentList(pub(crate) SmallVec<[Argument; 1]>);

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub(crate) Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
	pub(crate) name: SmallString,
	pub(crate) type_name: EnumType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType(pub(crate) Vec<StructField>);

#[derive(Debug, Clone, PartialEq, From)]
pub struct EnumType(pub(crate) SmallVec<[RawType; 1]>);

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
	pub(crate) name: SmallString,
	pub(crate) mutable: bool,
	pub(crate) type_name: EnumType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldLiteral {
	pub(crate) name: SmallString,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral(pub(crate) Vec<StructFieldLiteral>);
