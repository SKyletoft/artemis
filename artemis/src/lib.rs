use std::f64::EPSILON;

extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod ast;
pub mod ast2;
pub mod detype2;
pub mod detype2_types;
pub mod error;
pub mod ordered;
pub mod preprocess;
pub mod simplify;
pub mod type_definition;
pub mod types;

#[derive(Parser)]
#[grammar = "artemis.pest"]
pub struct GeneratedParser;

pub fn float_eq(a: f64, b: f64) -> bool {
	(a - b).abs() < EPSILON || (b - a).abs() < EPSILON
}

pub fn split_vec<T, U>(v: Vec<(T, U)>) -> (Vec<T>, Vec<U>) {
	let mut l_vec = Vec::with_capacity(v.len());
	let mut r_vec = Vec::with_capacity(v.len());
	for (l, r) in v.into_iter() {
		l_vec.push(l);
		r_vec.push(r);
	}
	(l_vec, r_vec)
}
