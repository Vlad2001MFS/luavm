extern crate hexf;

mod lexer;

pub use lexer::*;

fn main() {
    let test_source = std::fs::read_to_string("./test_lexer.lua").unwrap();
    let tokens = Lexer::parse(&test_source, "test_source");
    println!("token = [");
    for token in tokens.iter() {
        println!("  {:?}", token.token());
        if let Token::Break = token.token() {
            let pointer = " ".repeat(token.begin_pos().column() - 1) + &"^".repeat(token.end_pos().column() - token.begin_pos().column());
            println!("{}:{}: DESC\n{}\n{}", token.begin_pos().source_name(), token.begin_pos(), token.begin_pos().content(), pointer);
        }
    }
    println!("]");
}
