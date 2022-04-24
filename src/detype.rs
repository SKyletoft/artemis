use smallvec::SmallVec;
use variantly::Variantly;

use crate::ordered;

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: SmallString,
	pub arguments: usize,
	pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	pub name: SmallString,
	pub value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
	pub name: SmallString,
	pub value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub function_name: SmallString,
	pub arguments: Vec<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Subexpr(Subexpr),
	Declaration(Declaration),
	Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq, Variantly)]
pub enum Subexpr {
	BinOp(BinOp),
	IfExpr(IfExpr),
	Block(Block),
	Literal(u64),
	Variable(SmallString),
	Tuple(Vec<Subexpr>),
	FunctionCall(FunctionCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
	pub lhs: Box<Subexpr>,
	pub op: Op,
	pub rhs: Box<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	pub condition: Box<Subexpr>,
	pub lhs: Block,
	pub rhs: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Plus,
	Minus,
	Delta,
	Times,
	Div,
	Exp,
	FPlus,
	FMinus,
	FDelta,
	FTimes,
	FDiv,
	FExp,
	Not,
	And,
	Or,
	Xor,
	Dot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}

impl TryFrom<ordered::TopLevelConstruct> for TopLevelConstruct {
	type Error = anyhow::Error;

	fn try_from(value: ordered::TopLevelConstruct) -> Result<Self, Self::Error> {
		todo!()
	}
}
