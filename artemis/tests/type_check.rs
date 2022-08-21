use anyhow::Result;
use artemis::{
	ordered::{
		self, Assignment, Declaration, Expr, Function, Literal, RawType, Term,
		TopLevelConstruct, Type,
	},
	type_check, GeneratedParser, Rule,
};
use once_cell::sync::Lazy;
use pest::Parser;
use simple_logger::SimpleLogger;
use smallvec::SmallVec;

#[allow(unused_variables, dead_code)]
static LOGGER: Lazy<()> = Lazy::new(|| SimpleLogger::new().init().expect("Logger init failure"));

#[test]
fn assignment_to_same() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		y : ℤ = x;
		()
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;
	Ok(())
}

#[test]
fn literals_as_either_type() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		y : ℕ = 1;
		()
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;
	Ok(())
}

#[test]
fn reject_integer_conversion() -> Result<()> {
	let s = "λf() {
		x : ℤ = 1
		y : ℕ = x;
		()
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_non_bool_condition() -> Result<()> {
	let s = "λf() {
		if 1 {()} else {()}
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;
	Ok(())
}

#[test]
fn reject_use_of_undeclared_variable() -> Result<()> {
	let s = "λf() -> ℕ {
		x
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

	assert!(res.is_err());
	Ok(())
}

#[test]
fn reject_use_of_undeclared_function() -> Result<()> {
	let s = "λf() -> ℕ {
		g()
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	let res = type_check::check_program(&mut ordered);

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
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;

	Ok(())
}

#[test]
fn infer_types() -> Result<()> {
	let s = "λf () {
		x := 1
		y := true
		z := -2
		w := 4.0
		()
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;
	let expected = vec![TopLevelConstruct::Function(Function {
		name: "f".into(),
		arguments: SmallVec::new(),
		return_type: RawType::Unit,
		expr: Expr::Term(Term::Block(vec![
			Expr::Declaration(Declaration {
				name: "x".into(),
				value: Box::new(Expr::Term(Term::Literal(Literal::Integer(
					1,
				)))),
				type_name: Type {
					mutable: false,
					raw: RawType::Integer,
				},
			}),
			Expr::Declaration(Declaration {
				name: "y".into(),
				value: Box::new(Expr::Term(Term::Literal(Literal::Boolean(
					true,
				)))),
				type_name: Type {
					mutable: false,
					raw: RawType::Boolean,
				},
			}),
			Expr::Declaration(Declaration {
				name: "z".into(),
				value: Box::new(Expr::Term(Term::Literal(Literal::Integer(
					-2i64 as u64,
				)))),
				type_name: Type {
					mutable: false,
					raw: RawType::Integer,
				},
			}),
			Expr::Declaration(Declaration {
				name: "w".into(),
				value: Box::new(Expr::Term(Term::Literal(Literal::Float(
					4.0,
				)))),
				type_name: Type {
					mutable: false,
					raw: RawType::Real,
				},
			}),
			Expr::Term(Term::Literal(Literal::Unit)),
		])),
	})];
	assert_eq!(ordered, expected);

	Ok(())
}

#[test]
fn accept_ending_on_declaration() -> Result<()> {
	let s = "λf () → ℤ {
		x := 5
	}";
	let mut ordered = ordered::order(GeneratedParser::parse(Rule::top, s.trim())?)?;
	type_check::check_program(&mut ordered)?;

	Ok(())
}
