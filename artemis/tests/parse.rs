use std::fs;

use artemis::{GeneratedParser, Rule};
use pest::Parser;

#[ignore] // the examples folder currently contains files that aren't intended to be correct code
#[test]
fn examples() {
	let mut files = fs::read_dir("../examples")
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.filter(|entry| entry.is_file())
		.collect::<Vec<_>>();
	files.sort_unstable();
	for file in files.iter() {
		let contents = fs::read_to_string(file).unwrap();
		let res = GeneratedParser::parse(Rule::top, &contents);

		if let Err(e) = &res {
			eprintln!("{:?}:\n{e}\n\n{contents}", &file);
		}
		assert!(res.is_ok());
	}
}

#[ignore] // the examples folder currently contains files that aren't intended to be correct code
#[test]
fn success_files() {
	let mut files = fs::read_dir("tests/parse/ok")
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.collect::<Vec<_>>();
	files.sort_unstable();
	for file in files.iter() {
		let contents = fs::read_to_string(file).unwrap();
		let res = GeneratedParser::parse(Rule::top, &contents);

		if let Err(e) = &res {
			eprintln!("{:?}:\n{e}\n\n{contents}", &file);
		}
		assert!(res.is_ok());
	}
}

#[ignore] // the examples folder currently contains files that aren't intended to be correct code
#[test]
fn fail_files() {
	let mut files = fs::read_dir("tests/parse/fail")
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.collect::<Vec<_>>();
	files.sort_unstable();
	for file in files.iter() {
		let contents = fs::read_to_string(file).unwrap();
		let res = GeneratedParser::parse(Rule::top, &contents);

		if let Ok(e) = &res {
			eprintln!("{:?}:\n{e}\n\n{contents}", &file);
		}
		assert!(res.is_err());
	}
}
