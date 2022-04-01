extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "artemis.pest"]
struct GeneratedParser;

fn main() {
	println!("Hi");
}
