extern crate pest;
#[macro_use]
extern crate pest_derive;

// pub mod detype;
pub mod ast;
pub mod error;
pub mod ordered;
pub mod preprocess;
pub mod simplify;
pub mod types;
pub mod type_definition;
// pub mod type_check;

#[derive(Parser)]
#[grammar = "artemis.pest"]
pub struct GeneratedParser;
