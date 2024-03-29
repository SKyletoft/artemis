use anyhow::Result;
use artemis::{error::Error, ordered, preprocess, types, GeneratedParser, Rule};
use once_cell::sync::Lazy;
use pest::Parser;
use simple_logger::SimpleLogger;

#[allow(unused_variables, dead_code)]
static LOGGER: Lazy<()> = Lazy::new(|| SimpleLogger::new().init().expect("Logger init failure"));

fn compile_and_typecheck(code: String) -> Result<()> {
	eprintln!("IN: {}", &code);

	let source = preprocess::remove_comments(&[code]);
	// Conversion Result<_, ParseError> -> Result<_, AnyhowError>
	let res = (|| Ok(GeneratedParser::parse(Rule::top, &source)?))()
		.and_then(ordered::order)
		.and_then(types::check_and_infer)
		.map(|_| ());

	eprintln!("OUT: {:#?}", &res);
	res
}

#[ignore]
#[test]
fn assignment_to_same() {
	let s = "λf () = (
		x : ℤ = 1
		y : ℤ = x
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn literals_as_either_type() {
	let s = "λf () = (
		x : ℤ = 1
		y : ℕ = 1
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn reject_integer_conversion() {
	let s = "λf () = (
		x : ℤ = 1
		y : ℕ = x
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::MismatchedTypes(_))
	));
}

#[ignore]
#[test]
fn reject_non_bool_condition() {
	let s = "λf () = (
		if 1 then () else ()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::ConditionIsntBoolean(_))
	));
}

#[ignore]
#[test]
fn reject_same_scope_same_type_shadowing() {
	let s = "λf() = (
		x := 1
		x := 1
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn reject_same_scope_different_type_shadowing() {
	let s = "λf() = (
		x := 1
		x := true
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn accept_inner_scope_shadowing() {
	let s = "λf() = (
		x := 1
		y := (
			x := true
			x
		)
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn reject_use_of_undeclared_variable() {
	let s = "λf() → ℕ = (
		x
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::UndefinedVariable(_))
	));
}

#[ignore]
#[test]
fn reject_use_of_undeclared_function() {
	let s = "λf() → ℕ = (
		g()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::UndefinedVariable(_))
	));
}

#[ignore]
#[test]
fn reject_use_of_variable_as_function() {
	let s = "λf () = (
		x := 1
		y := x()
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::TypeNonFunctionAsFunction(_))
	));
}

#[ignore]
#[test]
fn reject_assignment_to_const() {
	let s = "λf() = (
		x : ℤ = 1
		x = 2
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::AssignmentToConst(_))
	));
}

#[ignore]
#[test]
fn accept_assignment_to_mut() {
	let s = "λf() = (
		x : mut ℤ = 1
		x = 2
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn accept_if_returns_same() {
	let s = "λf () = (
		x : ℝ = if true then (
			1.0
		) else (
			2.0
		)
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn reject_if_returns_different() {
	let s = "λf () = (
		x : ℝ = if true then (
			true
		) else (
			1.0
		)
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(res.is_err());
	assert!(matches!(
		res.unwrap_err().downcast_ref::<Error>(),
		Some(Error::MismatchedTypes(_))
	));
}

#[ignore]
#[test]
fn accept_if_returns_different() {
	let s = "λf () = (
		x := if true then (
			true
		) else (
			1.0
		)
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn accept_block_return_of_inner_variable() {
	let s = "λf (x: ℤ) → ℤ = (
		a := (
			a : mut ℤ = 1
			a = x + a
			a
		)
		a + 2
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn infer_types() {
	let s = "λf () = (
		x := 1
		y := true
		z := -2
		w := 4.0
		()
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}

#[ignore]
#[test]
fn accept_ending_on_declaration() {
	let s = "λf () → ℤ = (
		x := 5
	)"
	.into();
	let res = compile_and_typecheck(s);
	assert!(matches!(res, Ok(())))
}
