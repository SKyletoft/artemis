use anyhow::Result;
use artemis::{ordered, type_check, GeneratedParser, Rule};
use once_cell::sync::Lazy;
use pest::Parser;
use simple_logger::SimpleLogger;

#[allow(unused_variables, dead_code)]
static LOGGER: Lazy<()> = Lazy::new(|| SimpleLogger::new().init().expect("Logger init failure"));

#[test]
fn assignment_to_same() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		y : ℤ = x;
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	type_check::check_program(&ordered)?;
	Ok(())
}

#[test]
fn literals_as_either_type() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		y : ℕ = 1;
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	type_check::check_program(&ordered)?;
	Ok(())
}

#[test]
fn reject_integer_conversion() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		y : ℕ = x;
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_non_bool_condition() -> Result<()> {
	let s = "λf() {
		if 1 {()} else {()}
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_same_scope_same_type_shadowing() -> Result<()> {
	let s = "λf() {
		x := 1
		x := 1
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_same_scope_different_type_shadowing() -> Result<()> {
	let s = "λf() {
		x := 1
		x := true
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn accept_inner_scope_shadowing() -> Result<()> {
	let s = "λf() {
		x := 1
		y := {
			x := true
			x
		}
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	type_check::check_program(&ordered)?;
	Ok(())
}

#[test]
fn reject_use_of_undeclared_variable() -> Result<()> {
	let s = "λf() -> ℕ {
		x
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_use_of_undeclared_function() -> Result<()> {
	let s = "λf() -> ℕ {
		g()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_use_of_variable_as_function() -> Result<()> {
	let s = "λf() {
		x := 1
		y := x()
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_assignment_to_const() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		x = 2
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn accept_assignment_to_mut() -> Result<()> {
	let s = "λf() {
		x : mut ℤ = 1
		x = 2
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	type_check::check_program(&ordered)?;

	Ok(())
}

#[test]
fn accept_if_returns_same() -> Result<()> {
	let s = "λf() {
		x : ℝ = if true {
			1.0
		} else {
			2.0
		}
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	type_check::check_program(&ordered)?;

	Ok(())
}
#[test]
fn reject_if_returns_different() -> Result<()> {
	let s = "λf() {
		x := if true {
			true
		} else {
			1.0
		}
		()
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	let res = type_check::check_program(&ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn accept_block_return_of_inner_variable() -> Result<()> {
	let s = "λf (x: ℤ) → ℤ {
		a := {
			a : mut ℤ = 1
			a = x + a
			a
		}
		a + 2
	}";
	let ordered = ordered::order(GeneratedParser::parse(Rule::function_definition, s.trim())?)?;
	type_check::check_program(&ordered)?;

	Ok(())
}
