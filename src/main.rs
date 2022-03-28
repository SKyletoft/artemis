use bnf::Grammar;
use smallvec::{SmallVec, smallvec};
use smallstr::SmallString;

fn main() {
    let input = include_str!("artemis.bnf");
    
    let grammar: Result<Grammar, _> = input.parse();
    match grammar {
        Ok(g) => println!("{:#?}", g),
        Err(e) => println!("Failed to make grammar from String: {}", e),
    }
}
