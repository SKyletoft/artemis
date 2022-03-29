pub mod bindings;

use bindings::AST;

fn main() {
	let parse_tree = AST::from_stdin();
	println!("{0}\n{0:?}", &parse_tree);
}
