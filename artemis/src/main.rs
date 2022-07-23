use std::{fs, path::PathBuf, process::Command, str::FromStr};

use air::{
	register_allocation::{self, Configuration},
	x86_64,
};
use anyhow::Result;
use artemis::{detype, error::Error, ordered, simplify, type_check, GeneratedParser, Rule};
use clap::Parser as ClapParser;
use pest::Parser as PestParser;
use simple_logger::SimpleLogger;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Target {
	LinuxX64,
	LinuxAarch64,
}

impl FromStr for Target {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"x86" | "x64" | "x86_64" | "X86" | "X64" | "X86_64" => Ok(Target::LinuxX64),

			"aarch64" | "arm" | "arm64" | "Aarch64" | "Arm64" | "AARCH64" | "ARM"
			| "ARM64" => Ok(Target::LinuxAarch64),

			_ => Err(Error::InvalidTarget),
		}
	}
}

#[derive(ClapParser, Debug, Clone, PartialEq)]
#[clap(author, version, about, long_about = None)]
struct Config {
	#[clap(long)]
	target: Option<Target>,

	files: Vec<PathBuf>,

	#[clap(long, short, default_value_t = 0)]
	optimisation: u8,
}

fn main() -> Result<()> {
	SimpleLogger::new().init().expect("Logging init failure");
	log::info!("Logging initialised");

	let config = Config::parse();
	if config.files.is_empty() {
		log::error!("No input files provided");
		return Ok(());
	}

	let sources = config
		.files
		.iter()
		.map(fs::read_to_string)
		.collect::<Result<Vec<String>, _>>()?;
	let source = sources
		.into_iter()
		.reduce(|l, r| l + &r)
		.expect("There should be at least one file in the input");
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
