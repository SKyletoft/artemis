extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ordered;

#[derive(Parser)]
#[grammar = "artemis.pest"]
struct GeneratedParser;

fn main() {
	println!("Hi");
}
