use std::{
	env, fs,
	path::PathBuf,
	process::{Command, ExitCode},
	str::FromStr,
};

use air::{
	register_allocation::{self, Configuration},
	x86_64_codegen,
};
use anyhow::{bail, Result};
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

			_ => Err(Error::InvalidTarget(line!())),
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

	#[clap(long, short, default_value = "a.out")]
	output: String,

	#[clap(short = 'c')]
	object: bool,

	#[clap(short = 'S')]
	assembly: bool,

	#[clap(long, default_value = "/tmp")]
	working_files: String,

	files: Vec<PathBuf>,
}

struct Paths {
	mold: String,
	musl_x86: String,
	musl_arm: String,
	nasm: String,
	gnu_as: String,
	lib_x86: String,
	lib_arm: String,
}

fn compile(config: Config, paths: Paths) -> Result<()> {
	if config.files.is_empty() {
		bail!(Error::NoInputFiles(line!()));
	}

	let assembly_path = {
		let base = if config.assembly {
			"."
		} else {
			&config.working_files
		};
		format!("{base}/{}.S", &config.output)
	};
	let object_path = {
		let base = if config.object {
			"."
		} else {
			&config.working_files
		};
		format!("{base}/{}.o", &config.output)
	};

	let sources = config
		.files
		.iter()
		.map(fs::read_to_string)
		.collect::<Result<Vec<String>, _>>()?;
	log::debug!("Raw source code:\n{:#?}", &sources);

	let source = sources
		.into_iter()
		.reduce(|l, r| l + &r)
		.ok_or(Error::NoInputFiles(line!()))?;
	log::debug!("Preprocessed source code:\n{source}");

	let ast = GeneratedParser::parse(Rule::top, &source)?;
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
				&register_allocation::AARCH64,
			)?;
			log::debug!("Allocated Registers:\n{allocated:#?}");

			todo!("aarch64 backend")
		}
		Target::LinuxX64 => {
			let allocated = register_allocation::register_allocate(
				&ssa,
				&register_allocation::X86_64,
			)?;
			log::debug!("Allocated Registers:\n{allocated:#?}");

			let assembler = x86_64_codegen::assemble(&allocated)?;
			log::debug!("ASM:\n{assembler}");

			fs::write(&assembly_path, assembler)?;

			let mut nasm = Command::new(paths.nasm);
			nasm.arg(&assembly_path)
				.args(["-o", &object_path])
				.arg("-g")
				.args(["-f", "elf64"]);
			log::debug!("Assembling: {:#?}", &nasm);

			let nasm_output = String::from_utf8(nasm.output()?.stderr)?;
			if !nasm_output.is_empty() {
				bail!(Error::External(nasm_output));
			}

			let runtime = format!("{}/runtime.o", paths.lib_x86);
			let crt1_o = format!("{}/crt1.o", paths.musl_x86);
			let libc_a = format!("{}/libc.a", paths.musl_x86);

			let mut mold = Command::new(paths.mold);
			mold.args(["-m", "elf_x86_64"])
				.args(["-o", &config.output])
				.args([&crt1_o, &libc_a, &runtime, &object_path]);
			log::debug!("Linking: {:?}", &mold);

			let mold_output = String::from_utf8(mold.output()?.stderr)?;
			if !mold_output.is_empty() {
				bail!(Error::External(mold_output));
			}
		}
	}

	Ok(())
}

fn main() -> ExitCode {
	let logger_err = SimpleLogger::new()
		.with_level(LevelFilter::Warn) // Default
		.env() // But overwrite from environment
		.init();
	if let Err(e) = logger_err {
		eprintln!("Logger error: {e:#?}");
	}
	log::trace!("Logging initialised");

	let config = Config::parse();

	// Get overrideable paths for dependencies. Else rely on $PATH
	let mold = env::var("MOLD").unwrap_or_else(|_| String::from("mold"));
	log::trace!("mold:     {}", &mold);
	let musl_x86 = env::var("MUSL_x86")
		.unwrap_or_else(|_| String::from("/usr/lib/x86_64-linux-musl/lib"));
	log::trace!("musl_x86: {}", &musl_x86);
	let musl_arm = env::var("MUSL_ARM")
		.unwrap_or_else(|_| String::from("/usr/lib/aarch64-linux-musl/lib"));
	log::trace!("musl_arm: {}", &musl_arm);
	let nasm = env::var("NASM").unwrap_or_else(|_| String::from("nasm"));
	log::trace!("nasm:     {}", &nasm);
	let gnu_as = env::var("GNU_AS").unwrap_or_else(|_| String::from("aarch64-linux-gnu-as"));
	log::trace!("gnu_as:   {}", &gnu_as);
	let lib_x86 =
		env::var("ARTEMIS_RUNTIME_x86").unwrap_or_else(|_| String::from("./lib-x86_64"));
	log::trace!("lib_x86:  {}", &lib_x86);
	let lib_arm =
		env::var("ARTEMIS_RUNTIME_ARM").unwrap_or_else(|_| String::from("./lib-aarch64"));
	log::trace!("lib_arm:  {}", &lib_arm);

	let paths = Paths {
		mold,
		musl_x86,
		musl_arm,
		nasm,
		gnu_as,
		lib_x86,
		lib_arm,
	};

	let compiler_err = compile(config, paths);
	if let Err(e) = compiler_err {
		log::error!("{e}");
		ExitCode::FAILURE
	} else {
		ExitCode::SUCCESS
	}
}
