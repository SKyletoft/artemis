use std::{env, fs};

use anyhow::Result;
use artemis::{ordered, type_check, GeneratedParser, Rule};
use pest::Parser;
use simple_logger::SimpleLogger;

fn main() -> Result<()> {
	SimpleLogger::new().init().expect("Logging init failure");
	log::info!("Logging initialised");

	let source_file_name = env::args().nth(1).expect("No source file provided");
	let source = fs::read_to_string(&source_file_name)?;
	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	dbg!(&ast);
	let ordered = ordered::order(ast)?;
	dbg!(&ordered);
	type_check::check_program(&ordered)?;

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
