use artemis::{
	ordered::{BinOp, Literal, Op, Subexpr},
	simplify::{self, Context, SimpleBinOp, SimpleExpression, SimpleOp, Source},
};

#[test]
fn nested_addition() {
	let s = Subexpr::BinOp(BinOp {
		lhs: Box::new(Subexpr::BinOp(BinOp {
			lhs: Box::new(Subexpr::Literal(Literal::Integer(1))),
			op: Op::Plus,
			rhs: Box::new(Subexpr::Literal(Literal::Integer(2))),
		})),
		op: Op::Plus,
		rhs: Box::new(Subexpr::Literal(Literal::Integer(3))),
	});
	let mut res = Vec::new();
	let mut ctx = Context::default();
	simplify::simplify_subexpr(&s, &mut res, &mut ctx).unwrap();
	let expected = vec![
		SimpleExpression::BinOp(SimpleBinOp {
			target: 0.into(),
			op: SimpleOp::Add,
			lhs: Source::Integer(1),
			rhs: Source::Integer(2),
		}),
		SimpleExpression::BinOp(SimpleBinOp {
			target: 1.into(),
			op: SimpleOp::Add,
			lhs: Source::Register(0.into()),
			rhs: Source::Integer(3),
		}),
	];

	assert_eq!(res, expected);
}
