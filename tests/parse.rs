use std::fs;

use artemis::{GeneratedParser, Rule};
use pest::Parser;

#[test]
fn success_files() {
	let files = fs::read_dir("tests/parse/ok").unwrap();
	for file in files {
		let file = file.unwrap();
		let contents = fs::read_to_string(file.path()).unwrap();
		let res = GeneratedParser::parse(Rule::function_definition, &contents);

		if let Err(e) = &res {
			eprintln!("{:?}:\n{e}", file.path());
		}
		assert!(res.is_ok());
	}
}

#[test]
fn fail_files() {
	let files = fs::read_dir("tests/parse/fail").unwrap();
	for file in files {
		let file = file.unwrap();
		let contents = fs::read_to_string(file.path()).unwrap();
		let res = GeneratedParser::parse(Rule::function_definition, &contents);

		if let Ok(e) = &res {
			eprintln!("{:?}:\n{e}", file.path());
		}
		assert!(res.is_err());
	}
}

