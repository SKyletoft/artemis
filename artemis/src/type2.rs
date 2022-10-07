use anyhow::{bail, Result};

use crate::{
	ast::{Expr, Term},
	error::Error,
};

struct Context {}

pub fn check_and_infer(program: &mut [Expr]) -> Result<()> {
	let top_level = program.iter().all(is_allowed_at_top_level);
	if !top_level {
		bail!(Error::ForbiddenExprAtTopLevel(line!()));
	}

	Ok(())
}

fn is_allowed_at_top_level(e: &Expr) -> bool {
	if let Expr::Leaf(t) = e {
		let term = t.as_ref();
		matches!(
			term,
			Term::Declaration(_) | Term::FunctionDefinition(_) | Term::TypeAlias(_)
		)
	} else {
		false
	}
}
