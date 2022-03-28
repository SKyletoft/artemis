use std::{process::Command, path::PathBuf, env};
use bindgen::{Builder, CargoCallbacks};
use cc::Build;

fn main() {
	println!("cargo:rerun-if-changed=src/parser.cf");
	println!("cargo:rerun-if-changed=src/build.rs");

	// Generate parser
	Command::new("bnfc")
		.arg("--c")
		.arg("--outputdir=parser")
		.arg("src/parser.cf")
		.spawn()
		.expect("Failed to generate a parser");
	// Compile parser
	Build::new()
		.file("parser/Absyn.c")
		.file("parser/Buffer.c")
		.file("parser/Printer.c")
		.compiler("clang")
		.opt_level(3) // Same as cargo's release
		.flag("-march=native") // Same as cargo
		.flag("-w") // Code is generated, so I don't care about warnings
		.compile("parser");
	// Rust bindings for parser
	Builder::default()
		.header("parser/Absyn.h")
		.header("parser/Buffer.h")
		.header("parser/Parser.h")
		.header("parser/Printer.h")
		.header("parser/Skeleton.h")
		.parse_callbacks(Box::new(CargoCallbacks))
		.impl_debug(true)
		.impl_partialeq(true)
		.derive_debug(true)
		.derive_eq(true)
		.rustfmt_bindings(true)
		.size_t_is_usize(true)
		.generate()
		.expect("Failed to generate bindings")
		.write_to_file(PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs"))
		.expect("Failed to save bindings");
}
