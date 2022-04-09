extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::{env, fs};

use anyhow::Result;
use once_cell::sync::Lazy;
use pest::{
	iterators::Pair,
	prec_climber::{Assoc, Operator, PrecClimber},
	Parser,
};

mod ordered;

static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
	use Assoc::*;
	use Rule::*;
	PrecClimber::new(vec![
		Operator::new(plus, Left) | Operator::new(minus, Left),
		Operator::new(times, Left) | Operator::new(div, Left),
		Operator::new(exp, Right),
	])
});

#[derive(Parser)]
#[grammar = "artemis.pest"]
struct GeneratedParser;

fn main() -> Result<()> {
	let source_file_name = env::args().skip(1).next().expect("No source file provided");
	let source = fs::read_to_string(&source_file_name)?;
	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	dbg!(&ast);
	let ordered = PREC_CLIMBER.climb(ast, |pair: Pair<Rule>| {}, |lhs, pair: Pair<Rule>, rhs| ());

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
