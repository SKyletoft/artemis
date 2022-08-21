use std::collections::HashMap;

use anyhow::Result;
use artemis::{
	detype::{
		self, BinOp as DetypedBinOp, Expr as DetypedExpr, Op as DetypedOp,
		Term as DetypedTerm,
	},
	ordered::{
		BinOp as OrderedBinOp, Declaration as OrderedDeclaration, Expr as OrderedExpr,
		Literal, Op as OrderedOp, RawType, Term as OrderedTerm, Type,
	},
};

#[test]
fn int_add() -> Result<()> {
	let ordered = OrderedExpr::Term(OrderedTerm::BinOp(OrderedBinOp {
		lhs: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Integer(1)))),
		op: OrderedOp::Plus,
		rhs: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Integer(1)))),
	}));
	let expected = DetypedExpr::Term(DetypedTerm::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedExpr::Term(DetypedTerm::Literal(1))),
		op: DetypedOp::Plus,
		rhs: Box::new(DetypedExpr::Term(DetypedTerm::Literal(1))),
	}));
	let (res, _) = detype::detype_expr(&ordered, &mut HashMap::new())?;

	assert_eq!(res, expected);
	Ok(())
}

#[test]
fn float_add() -> Result<()> {
	let ordered = OrderedExpr::Term(OrderedTerm::BinOp(OrderedBinOp {
		lhs: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Float(1f64)))),
		op: OrderedOp::Plus,
		rhs: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Float(1f64)))),
	}));
	let expected = DetypedExpr::Term(DetypedTerm::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedExpr::Term(DetypedTerm::Literal(1f64.to_bits()))),
		op: DetypedOp::FPlus,
		rhs: Box::new(DetypedExpr::Term(DetypedTerm::Literal(1f64.to_bits()))),
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
			value: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Integer(1)))),
		}),
		OrderedExpr::Declaration(OrderedDeclaration {
			name: "y".into(),
			type_name: Type {
				mutable: false,
				raw: RawType::Integer,
			},
			value: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Integer(1)))),
		}),
		OrderedExpr::Term(OrderedTerm::BinOp(OrderedBinOp {
			lhs: Box::new(OrderedExpr::Term(OrderedTerm::Variable("x".into()))),
			op: OrderedOp::Plus,
			rhs: Box::new(OrderedExpr::Term(OrderedTerm::Variable("y".into()))),
		})),
	];
	let expected = DetypedExpr::Term(DetypedTerm::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedExpr::Term(DetypedTerm::Variable("x".into()))),
		op: DetypedOp::Plus,
		rhs: Box::new(DetypedExpr::Term(DetypedTerm::Variable("y".into()))),
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
			value: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Float(1f64)))),
		}),
		OrderedExpr::Declaration(OrderedDeclaration {
			name: "y".into(),
			type_name: Type {
				mutable: false,
				raw: RawType::Real,
			},
			value: Box::new(OrderedExpr::Term(OrderedTerm::Literal(Literal::Float(1f64)))),
		}),
		OrderedExpr::Term(OrderedTerm::BinOp(OrderedBinOp {
			lhs: Box::new(OrderedExpr::Term(OrderedTerm::Variable("x".into()))),
			op: OrderedOp::Plus,
			rhs: Box::new(OrderedExpr::Term(OrderedTerm::Variable("y".into()))),
		})),
	];
	let expected = DetypedExpr::Term(DetypedTerm::BinOp(DetypedBinOp {
		lhs: Box::new(DetypedExpr::Term(DetypedTerm::Variable("x".into()))),
		op: DetypedOp::FPlus,
		rhs: Box::new(DetypedExpr::Term(DetypedTerm::Variable("y".into()))),
	}));
	let (res, _) = detype::detype_block(&ordered, &mut HashMap::new())?;

	assert_eq!(res.last().unwrap(), &expected);
	Ok(())
}
