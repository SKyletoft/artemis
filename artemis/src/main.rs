use std::{env, fs, process::Command};

use air::{
	register_allocation::{self, Configuration},
	x86_64,
};
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

	let allocated =
		register_allocation::register_allocate(&ssa, &Configuration::new(7, 0, 4, 0))?;

	let assembler = x86_64::assemble(&allocated[0])?;
	dbg!(&assembler);

	fs::write("a.asm", assembler)?;

	let nasm_raw = Command::new("nasm")
		.arg("a.asm")
		.args(["-o", "a.o"])
		.args(["-f", "elf64"])
		.output()?
		.stderr;
	let nasm_string = String::from_utf8(nasm_raw)?;
	if !nasm_string.is_empty() {
		log::error!("Nasm: {nasm_string}");
	}

	println!("\n---------------------------------------------------\n\n{source}");

	Ok(())
}
