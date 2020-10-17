extern crate hexf;

pub mod lexer;
pub mod parser;

use lexer::{
    Lexer, Token,
};
use parser::{
    Parser,
};

fn main() {
    let test_source = std::fs::read_to_string("./test_parser.lua").unwrap();

    let tokens = Lexer::parse(&test_source, "test_source");
    if false {
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

    let chunk = Parser::parse(tokens);
    if true {
        println!("### AST >>>");
        println!("{:#?}", chunk);
        println!("### AST <<<");
    }
}
