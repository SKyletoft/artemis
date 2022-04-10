use smallvec::SmallVec;

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = SmallVec<[Expr; 4]>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	name: SmallString,
	arguments: SmallVec<[Argument; 4]>,
	return_type: Type,
	block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
	name: SmallString,
	type_name: Type,
	value: Subexpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	type_name: Type,
	name: SmallString,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Subexpr(Box<Subexpr>),
	Declaration,
	Assignment,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Subexpr {
	BinOp(BinOp),
	If(IfExpr),
	Block(Block),
	Literal(Literal),
	Variable(SmallString),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Integer(u64),
	Float(f64),
	Boolean(bool),
	Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
	lhs: Box<Subexpr>,
	op: Op,
	rhs: Box<Subexpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
	condition: Box<Subexpr>,
	lhs: Block,
	rhs: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Plus,
	Minus,
	Delta,
	Times,
	Div,
	Exp,
	Not,
	And,
	Or,
	Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawType {
	Integer,
	Natural,
	Real,
	Boolean,
	Struct(SmallString),
	Inferred,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	Const(RawType),
	Mutable(RawType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}
