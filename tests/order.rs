use anyhow::Result;
use artemis::{
	ordered::{
		self, BinOp, Declaration, Expr, FunctionCall, Literal, Op, RawType, Subexpr, Type, AST,
	},
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

#[test]
fn tuple_1() {
	let s = "(x, y)";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::Tuple(vec![
		Subexpr::Variable("x".into()),
		Subexpr::Variable("y".into()),
	]));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn tuple_2() {
	let s = "(1 + 2, (2 + 3) * 2)";
	let res = GeneratedParser::parse(Rule::subexpr, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = AST::Subexpr(Subexpr::Tuple(vec![
		Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
			op: Op::Plus,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
		}),
		Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::BinOp(BinOp {
				lhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
				op: Op::Plus,
				rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
			})),
			op: Op::Times,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
		}),
	]));
	assert_eq!(res.len(), 1);
	assert_eq!(res[0], expected);
}

#[test]
fn op_assign() {
	let a = "a = a + 1";
	let b = "a += 1";
	let res_a = GeneratedParser::parse(Rule::expr, a)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let res_b = GeneratedParser::parse(Rule::expr, b)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	assert_eq!(res_a.len(), 1);
	assert_eq!(res_b.len(), 1);
	assert_eq!(res_a, res_b);
}

#[test]
fn else_if() {
	let a = "if () {()} else if () {()} else {()}";
	let b = "if () {()} else { if () {()} else {()} }";
	let res_a = GeneratedParser::parse(Rule::expr, a)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let res_b = GeneratedParser::parse(Rule::expr, b)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	assert_eq!(res_a.len(), 1);
	assert_eq!(res_b.len(), 1);
	assert_eq!(res_a, res_b);
}

#[test]
fn declaration_with_variable_init() {
	let s = "{
		x : ℤ = 1
		y : ℤ = x
	}";
	let res = GeneratedParser::parse(Rule::block, s)
		.unwrap()
		.map(AST::try_from)
		.collect::<Result<Vec<_>>>()
		.unwrap();
	let expected = vec![AST::Block(vec![
		Expr::Declaration(Declaration {
			name: "x".into(),
			type_name: Type::Const(RawType::Integer),
			value: Subexpr::Literal(Literal::Integer(1)),
		}),
		Expr::Declaration(Declaration {
			name: "y".into(),
			type_name: Type::Const(RawType::Integer),
			value: Subexpr::Variable("x".into()),
		}),
	])];
	assert_eq!(res.len(), 1);
	assert_eq!(res, expected);
}
