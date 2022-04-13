use anyhow::Result;
use artemis::{
	ordered::{BinOp, Literal, Op, Subexpr, AST},
	GeneratedParser, Rule,
};
use pest::Parser;

#[test]
fn add_2() {
	let s = "1 + 2";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
		op: Op::Plus,
		rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}
