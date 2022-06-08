use std::collections::HashMap;

use anyhow::Result;
use artemis::{
	detype::{
		self, BinOp as DetypedBinOp, Expr as DetypedExpr, Op as DetypedOp,
		Subexpr as DetypedSubexpr,
	},
	ordered::{
		BinOp as OrderedBinOp, Declaration as OrderedDeclaration, Expr as OrderedExpr,
		Literal, Op as OrderedOp, RawType, Subexpr as OrderedSubexpr, Type,
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

#[test]
fn int_add_var() -> Result<()> {
	let ordered = [
		OrderedExpr::Declaration(OrderedDeclaration {
			name: "x".into(),
			type_name: Type {
				mutable: false,
				raw: RawType::Integer,
			},
			value: OrderedSubexpr::Literal(Literal::Integer(1)),
		}),
		OrderedExpr::Declaration(OrderedDeclaration {
			name: "y".into(),
			type_name: Type {
				mutable: false,
				raw: RawType::Integer,
			},
			value: OrderedSubexpr::Literal(Literal::Integer(1)),
		}),
		OrderedExpr::Subexpr(OrderedSubexpr::BinOp(OrderedBinOp {
			lhs: Box::new(OrderedSubexpr::Variable("x".into())),
			op: OrderedOp::Plus,
			rhs: Box::new(OrderedSubexpr::Variable("y".into())),
		})),
	];
	let expected = DetypedExpr::Subexpr(DetypedSubexpr::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedSubexpr::Variable("x".into())),
		op: DetypedOp::Plus,
		rhs: Box::new(DetypedSubexpr::Variable("y".into())),
	}));
	let (res, _) = detype::detype_block(&ordered, &mut HashMap::new())?;

	assert_eq!(res.last().unwrap(), &expected);
	Ok(())
}

#[test]
fn float_add_var() -> Result<()> {
	let ordered = [
		OrderedExpr::Declaration(OrderedDeclaration {
			name: "x".into(),
			type_name: Type {
				mutable: false,
				raw: RawType::Real,
			},
			value: OrderedSubexpr::Literal(Literal::Float(1f64)),
		}),
		OrderedExpr::Declaration(OrderedDeclaration {
			name: "y".into(),
			type_name: Type {
				mutable: false,
				raw: RawType::Real,
			},
			value: OrderedSubexpr::Literal(Literal::Float(1f64)),
		}),
		OrderedExpr::Subexpr(OrderedSubexpr::BinOp(OrderedBinOp {
			lhs: Box::new(OrderedSubexpr::Variable("x".into())),
			op: OrderedOp::Plus,
			rhs: Box::new(OrderedSubexpr::Variable("y".into())),
		})),
	];
	let expected = DetypedExpr::Subexpr(DetypedSubexpr::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedSubexpr::Variable("x".into())),
		op: DetypedOp::FPlus,
		rhs: Box::new(DetypedSubexpr::Variable("y".into())),
	}));
	let (res, _) = detype::detype_block(&ordered, &mut HashMap::new())?;

	assert_eq!(res.last().unwrap(), &expected);
	Ok(())
}
