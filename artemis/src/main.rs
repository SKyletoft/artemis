use std::{fs, path::PathBuf, process::Command, str::FromStr};

use air::{
	register_allocation::{self, Configuration},
	x86_64_codegen,
};
use anyhow::Result;
use artemis::{detype, error::Error, ordered, simplify, type_check, GeneratedParser, Rule};
use clap::Parser as ClapParser;
use log::LevelFilter;
use pest::Parser as PestParser;
use simple_logger::SimpleLogger;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Target {
	LinuxX64,
	LinuxAarch64,
}

impl Default for Target {
	fn default() -> Self {
		#[cfg(target_arch = "x86_64")]
		return Target::LinuxX64;
		#[cfg(target_arch = "aarch64")]
		return Target::LinuxAarch64;
		#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
		panic!("On a non x64 or Aarch64 platform you must explicitly specify a target");
	}
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

	#[clap(long, short = 'O', default_value_t = 0)]
	optimisation: u8,

	#[clap(long, short, default_value = "a.o")]
	output: String,

	#[clap(long, default_value = "/tmp/a.asm")]
	working_files: String,

	files: Vec<PathBuf>,
}

fn main() -> Result<()> {
	SimpleLogger::new()
		.with_level(LevelFilter::Warn) // Default
		.env() // But overwrite from environment
		.init()?;
	log::debug!("Logging initialised");

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
	log::debug!("Raw source code:\n{:#?}", &sources);

	let source = sources
		.into_iter()
		.reduce(|l, r| l + &r)
		.expect("There should be at least one file in the input");
	log::debug!("Preprocessed source code:\n{source}");

	let ast = GeneratedParser::parse(Rule::function_definition, &source)?;
	log::debug!("Pest output:\n{ast:#?}");

	let mut ordered = ordered::order(ast)?;
	log::debug!("AST:\n{ordered:#?}");

	type_check::check_program(&mut ordered)?;
	log::debug!("Inferred types:\n{ordered:#?}");

	let detyped = detype::detype(&ordered)?;
	log::debug!("Detyped:\n{detyped:#?}");

	let ssa = simplify::simplify(&detyped)?;
	log::debug!("SSA:\n{ssa:#?}");

	match config.target.unwrap_or_default() {
		Target::LinuxAarch64 => {
			let allocated = register_allocation::register_allocate(
				&ssa,
				&Configuration::AARCH64,
			)?;
			log::debug!("Allocated Registers:\n{allocated:#?}");

			todo!("aarch64 backend")
		}
		Target::LinuxX64 => {
			let allocated = register_allocation::register_allocate(
				&ssa,
				&Configuration::X86_64,
			)?;
			log::debug!("Allocated Registers:\n{allocated:#?}");

			let assembler = x86_64_codegen::assemble(&allocated[0])?;
			log::debug!("ASM:\n{assembler}");

			fs::write(&config.working_files, assembler)?;

			let nasm_raw = Command::new("nasm")
				.arg(&config.working_files)
				.args(["-o", &config.output])
				.args(["-f", "elf64"])
				.output()?
				.stderr;
			let nasm_string = String::from_utf8(nasm_raw)?;
			if !nasm_string.is_empty() {
				log::error!("Nasm: {nasm_string}");
				return Ok(());
			}

			let runner = "c_working_files/runtime.o";
			let crt1_o = format!("{}/lib/crt1.o", "musl"); //std::env!("MUSL"));
			let libc_a = format!("{}/lib/libc.a", "musl"); //std::env!("MUSL"));

			let mold_raw = Command::new("mold")
				.arg(&config.output)
				.args([&crt1_o, &libc_a, runner])
				.output()?
				.stderr;
			let mold_string = String::from_utf8(mold_raw)?;
			if !mold_string.is_empty() {
				log::error!("Mold: {mold_string}");
				return Ok(());
			}
		}
	}

	Ok(())
}
