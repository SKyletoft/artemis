use std::{env, fs};

use anyhow::Result;
use pest::{iterators::Pair, Parser};
use simple_logger::SimpleLogger;

use artemis::{GeneratedParser, Rule, PREC_CLIMBER};

fn main() -> Result<()> {
	SimpleLogger::new().init().expect("Logging init failure");
	log::warn!("Logging initialised");

	let source_file_name = env::args().skip(1).next().expect("No source file provided");
	let source = fs::read_to_string(&source_file_name)?;
	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	dbg!(&ast);
	let ordered = PREC_CLIMBER.climb(ast, |pair: Pair<Rule>| {}, |lhs, pair: Pair<Rule>, rhs| ());

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
