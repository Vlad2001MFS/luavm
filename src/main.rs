extern crate hexf;

mod lexer;

pub use lexer::*;

fn main() {
    let test_source = std::fs::read_to_string("./test_lexer.lua").unwrap();
    let tokens = Lexer::parse(&test_source, "test_source");
    println!("token = [");
    for token in tokens.iter() {
        println!("  {:?}", token);
    }
    println!("]");
}
