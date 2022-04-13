use std::fs;

use artemis::{GeneratedParser, Rule};
use pest::Parser;

#[test]
fn examples() {
	let mut files = fs::read_dir("examples")
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.collect::<Vec<_>>();
	files.sort_unstable();
	for file in files {
		let contents = fs::read_to_string(&file).unwrap();
		let res = GeneratedParser::parse(Rule::function_definition, &contents);

		if let Err(e) = &res {
			eprintln!("{:?}:\n{e}", &file);
		}
		assert!(res.is_ok());
	}
}

#[test]
fn success_files() {
	let mut files = fs::read_dir("tests/parse/ok")
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.collect::<Vec<_>>();
	files.sort_unstable();
	for file in files {
		let contents = fs::read_to_string(&file).unwrap();
		let res = GeneratedParser::parse(Rule::function_definition, &contents);

		if let Err(e) = &res {
			eprintln!("{:?}:\n{e}", &file);
		}
		assert!(res.is_ok());
	}
}

#[test]
fn fail_files() {
	let mut files = fs::read_dir("tests/parse/fail")
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.collect::<Vec<_>>();
	files.sort_unstable();
	for file in files {
		let contents = fs::read_to_string(&file).unwrap();
		let res = GeneratedParser::parse(Rule::function_definition, &contents);

		if let Ok(e) = &res {
			eprintln!("{:?}:\n{e}", &file);
		}
		assert!(res.is_err());
	}
}
