extern crate hexf;

mod lexer;
mod parser;

pub use lexer::*;
pub use parser::*;

fn main() {
    let test_source = std::fs::read_to_string("./test_lexer.lua").unwrap();
    let tokens = Lexer::parse(&test_source, "test_source");
    println!("token = [");
    for token in tokens.iter() {
        println!("  {:?}", token.token());
        if let Token::Break = token.token() {
            let pointer = " ".repeat(token.begin_location().column() - 1) + &"^".repeat(token.end_location().column() - token.begin_location().column());
            println!("{}:{}: DESC\n{}\n{}", token.begin_location().source_name(), token.begin_location(), token.begin_location().content(), pointer);
        }
    }
    println!("]");
}
