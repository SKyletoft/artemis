use std::{env, fs};

use air::register_allocation::{self, Configuration};
use anyhow::Result;
use artemis::{detype, ordered, simplify, type_check, GeneratedParser, Rule};
use pest::Parser;
use simple_logger::SimpleLogger;

fn main() -> Result<()> {
	SimpleLogger::new().init().expect("Logging init failure");
	log::info!("Logging initialised");

	let source_file_name = env::args().nth(1).expect("No source file provided");
	let source = fs::read_to_string(&source_file_name)?;
	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	let mut ordered = ordered::order(ast)?;
	type_check::check_program(&mut ordered)?;

	let detyped = detype::detype(&ordered)?;

	let ssa = simplify::simplify(&detyped)?;
	dbg!(&ssa);

	let allocated =
		register_allocation::register_allocate(&ssa, &Configuration::new(7, 0, 4, 0))?;
	dbg!(allocated);

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
