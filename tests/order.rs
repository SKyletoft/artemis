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

#[test]
fn add_3() {
	let s = "1 + 2 + 3";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
			op: Op::Plus,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
		})),
		op: Op::Plus,
		rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn mul_front() {
	let s = "1 * 2 + 3";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
			op: Op::Times,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
		})),
		op: Op::Plus,
		rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn mul_back() {
	let s = "1 + 2 * 3";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
		op: Op::Plus,
		rhs: Box::new(Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
			op: Op::Times,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
		})),
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn parenthesis_1() {
	let s = "(1 + 2) * 3";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
			op: Op::Plus,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
		})),
		op: Op::Times,
		rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn parenthesis_2() {
	let s = "1 * (2 + 3)";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
		op: Op::Times,
		rhs: Box::new(Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
			op: Op::Plus,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
		})),
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}
