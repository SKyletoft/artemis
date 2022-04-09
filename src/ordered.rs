use smallvec::SmallVec;

type SmallString = smallstr::SmallString<[u8; 16]>;
type Block = SmallVec<[Expr; 4]>;

pub struct Function {
	name: SmallString,
	arguments: SmallVec<[Argument; 4]>,
	return_type: Type,
	block: Block,
}

pub struct Declaration {
	name: SmallString,
	type_name: Type,
	value: Subexpr,
}

pub struct Argument {
	type_name: Type,
	name: SmallString,
}

pub enum Expr {
	Subexpr(Box<Subexpr>),
	Declaration,
	Assignment,
}

pub enum Subexpr {
	BinOp(BinOp),
	If(IfExpr),
	Block(Block),
	Literal(Literal),
	Variable(SmallString),
}

pub enum Literal {
	Integer(u64),
	Float(f64),
	Boolean(bool),
}

pub struct BinOp {
	lhs: Box<Subexpr>,
	op: Op,
	rhs: Box<Subexpr>,
}

pub struct IfExpr {
	condition: Box<Subexpr>,
	lhs: Block,
	rhs: Block,
}

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

pub enum RawType {
	Integer,
	Natural,
	Real,
	Boolean,
	Struct(SmallString),
}

pub enum Type {
	Const(RawType),
	Mutable(RawType),
}

pub enum TopLevelConstruct {
	Function(Function),
	Declaration(Declaration),
}
