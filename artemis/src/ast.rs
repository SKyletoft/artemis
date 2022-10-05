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

#[derive(Debug, Clone, PartialEq)]
pub enum RawType {
	Natural,
	Real,
	Integer,
	Bool,
	Any,
	Type,
	StructNameOrAlias(SmallString),
	Unit,
	Tuple(Vec<Type>),
	StructType(StructType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple(pub(crate) Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub(crate) pattern: Pattern,
	pub(crate) type_name: Option<Type>,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub(crate) pattern: Pattern,
	pub(crate) expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub(crate) name: SmallString,
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
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
	Declaration(Declaration),
	Assignment(Assignment),
	FunctionDefinition(FunctionDefinition),
	TypeAlias(TypeAlias),
	VarName(SmallString),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
	Exp,
	Mul,
	Div,
	Add,
	Sub,
	Delta,
	And,
	Or,
	Xor,
	Dot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
	Not,
	Sub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub(crate) name: SmallString,
	pub(crate) type_name: EnumType,
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
