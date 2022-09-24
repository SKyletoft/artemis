use air::simplify::{Block, Context, SimpleBinOp, SimpleExpression, SimpleOp, Source};
use artemis::{
	detype::{BinOp, Declaration, Expr, IfExpr, Op, Term},
	simplify,
};
use smallvec::{smallvec, SmallVec};

#[test]
fn nested_addition() {
	let s = Term::BinOp(BinOp {
		lhs: Box::new(Expr::Term(Term::BinOp(BinOp {
			lhs: Box::new(Expr::Term(Term::Literal(1))),
			op: Op::Plus,
			rhs: Box::new(Expr::Term(Term::Literal(2))),
		}))),
		op: Op::Plus,
		rhs: Box::new(Expr::Term(Term::Literal(3))),
	});
	let mut blocks = Vec::new();
	let mut ctx = Context::default();
	let mut block = Block::default();
	simplify::simplify_term(&s, &mut block, &mut blocks, &mut ctx).unwrap();

	let expected: SmallVec<[SimpleExpression; 4]> = smallvec![
		SimpleExpression::BinOp(SimpleBinOp {
			target: 0usize.into(),
			op: SimpleOp::Add,
			lhs: Source::Value(1),
			rhs: Source::Value(2),
		}),
		SimpleExpression::BinOp(SimpleBinOp {
			target: 1usize.into(),
			op: SimpleOp::Add,
			lhs: Source::Register(0usize.into()),
			rhs: Source::Value(3),
		}),
	];

	assert_eq!(block.block, expected);
}

#[test]
fn simple_branch() {
	let s = Expr::Declaration(Declaration {
		name: "x".into(),
		value: Box::new(Expr::Term(Term::IfExpr(IfExpr {
			condition: Box::new(Expr::Term(Term::Literal(1))),
			lhs: Box::new(Expr::Term(Term::Literal(2))),
			rhs: Box::new(Expr::Term(Term::Literal(3))),
		}))),
	});

	let mut block = Block::default();
	let mut blocks = Vec::new();
	let mut ctx = Context::default();
	simplify::simplify_expr(&s, &mut block, &mut blocks, &mut ctx).unwrap();

	// Assert that the block links are correct, but not the order of the output
	assert_eq!(block.intro[0].value[0].from, blocks[0].out.two().unwrap().1);
	assert_eq!(block.intro[0].value[1].from, blocks[0].out.two().unwrap().2);
}
