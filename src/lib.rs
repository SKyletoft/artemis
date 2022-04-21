extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod error;
pub mod ordered;
pub mod type_check;

#[derive(Parser)]
#[grammar = "artemis.pest"]
pub struct GeneratedParser;
