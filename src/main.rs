use std::{env, fs};

use anyhow::Result;
use artemis::{
	detype, ordered,
	register_allocation::{self, Configuration},
	simplify, type_check, GeneratedParser, Rule,
};
use pest::Parser;
use simple_logger::SimpleLogger;

fn main() -> Result<()> {
	SimpleLogger::new().init().expect("Logging init failure");
	log::info!("Logging initialised");

	let source_file_name = env::args().nth(1).expect("No source file provided");
	let source = fs::read_to_string(&source_file_name)?;
	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	// dbg!(&ast);
	let mut ordered = ordered::order(ast)?;
	dbg!(&ordered);
	type_check::check_program(&mut ordered)?;
	dbg!(&ordered);

	let detyped = detype::detype(&ordered)?;
	dbg!(&detyped);

	let ssa = simplify::simplify(&detyped)?;
	dbg!(&ssa);

	let allocated = register_allocation::register_allocate(
		&ssa,
		&Configuration {
			general_purpose_registers: 20,
			floating_point_registers: 2,
			argument_registers: 2,
			temporary_registers: 0,
		},
	)?;
	dbg!(allocated);

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
