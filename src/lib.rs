extern crate pest;
#[macro_use]
extern crate pest_derive;

use once_cell::sync::Lazy;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

pub mod error;
pub mod ordered;
pub mod type_check;

pub static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
	use Assoc::*;
	use Rule::*;
	PrecClimber::new(vec![
		Operator::new(plus, Left) | Operator::new(minus, Left),
		Operator::new(times, Left) | Operator::new(div, Left),
		Operator::new(exp, Right),
	])
});

#[derive(Parser)]
#[grammar = "artemis.pest"]
pub struct GeneratedParser;
