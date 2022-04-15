use std::{env, fs};

use anyhow::Result;
use pest::Parser;
use simple_logger::SimpleLogger;
use smallvec::SmallVec;

use artemis::{ordered::AST, GeneratedParser, Rule};

fn main() -> Result<()> {
	SimpleLogger::new().init().expect("Logging init failure");
	log::info!("Logging initialised");

	let source_file_name = env::args().nth(1).expect("No source file provided");
	let source = fs::read_to_string(&source_file_name)?;
	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	dbg!(&ast);
	let ordered = ast
		.map(AST::try_from)
		.collect::<Result<SmallVec<[AST; 8]>>>()?;
	dbg!(&ordered);

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
