use std::collections::HashMap;

use anyhow::Result;
use artemis::{
	detype::{
		self, Assignment as DetypedAssignment, BinOp as DetypedBinOp,
		Declaration as DetypedDeclaration, Expr as DetypedExpr,
		Function as DetypedFunctions, Function as DetypedFunction,
		FunctionCall as DetypedFunctionCall, IfExpr as DetypedIfExpr, Op as DetypedOp,
		Subexpr as DetypedSubexpr, TopLevelConstruct as DetypedTopLevelConstruct,
	},
	ordered::{
		Assignment as OrderedAssignment, BinOp as OrderedBinOp,
		Declaration as OrderedDeclaration, Expr as OrderedExpr,
		Function as OrderedFunctions, Function as OrderedFunction,
		FunctionCall as OrderedFunctionCall, IfExpr as OrderedIfExpr, Literal,
		Op as OrderedOp, RawType, Subexpr as OrderedSubexpr,
		TopLevelConstruct as OrderedTopLevelConstruct,
	},
};

#[test]
fn int_add() -> Result<()> {
	let ordered = OrderedExpr::Subexpr(OrderedSubexpr::BinOp(OrderedBinOp {
		lhs: Box::new(OrderedSubexpr::Literal(Literal::Integer(1))),
		op: OrderedOp::Plus,
		rhs: Box::new(OrderedSubexpr::Literal(Literal::Integer(1))),
	}));
	let expected = DetypedExpr::Subexpr(DetypedSubexpr::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedSubexpr::Literal(1)),
		op: DetypedOp::Plus,
		rhs: Box::new(DetypedSubexpr::Literal(1)),
	}));
	let (res, _) = detype::detype_expr(&ordered, &mut HashMap::new())?;

	assert_eq!(res, expected);
	Ok(())
}

#[test]
fn float_add() -> Result<()> {
	let ordered = OrderedExpr::Subexpr(OrderedSubexpr::BinOp(OrderedBinOp {
		lhs: Box::new(OrderedSubexpr::Literal(Literal::Float(1f64))),
		op: OrderedOp::Plus,
		rhs: Box::new(OrderedSubexpr::Literal(Literal::Float(1f64))),
	}));
	let expected = DetypedExpr::Subexpr(DetypedSubexpr::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedSubexpr::Literal(1f64.to_bits())),
		op: DetypedOp::FPlus,
		rhs: Box::new(DetypedSubexpr::Literal(1f64.to_bits())),
	}));
	let (res, _) = detype::detype_expr(&ordered, &mut HashMap::new())?;

	assert_eq!(res, expected);
	Ok(())
}
