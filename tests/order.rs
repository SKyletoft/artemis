use anyhow::Result;
use artemis::{
	ordered::{BinOp, FunctionCall, Literal, Op, Subexpr, AST},
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

#[test]
fn function_call_0() {
	let s = "f()";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::FunctionCall(FunctionCall {
		function_name: "f".into(),
		arguments: vec![],
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn function_call_1() {
	let s = "f(x)";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::FunctionCall(FunctionCall {
		function_name: "f".into(),
		arguments: vec![Subexpr::Variable("x".into())],
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn function_call_2() {
	let s = "f(x, y)";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::FunctionCall(FunctionCall {
		function_name: "f".into(),
		arguments: vec![Subexpr::Variable("x".into()), Subexpr::Variable("y".into())],
	}));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn function_call_3() {
	let s = "f(x, y, x + y)";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::FunctionCall(FunctionCall {
		function_name: "f".into(),
		arguments: vec![
			Subexpr::Variable("x".into()),
			Subexpr::Variable("y".into()),
			Subexpr::BinOp(BinOp {
				lhs: Box::new(Subexpr::Variable("x".into())),
				op: Op::Plus,
				rhs: Box::new(Subexpr::Variable("y".into())),
			}),
		],
	}));
	dbg!(&res);
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn function_call_trailing_comma() {
	let s = "f(x,)";
	let res = GeneratedParser::parse(Rule::function_call, s);
	dbg!(&res);
	assert!(res.is_err());
}
