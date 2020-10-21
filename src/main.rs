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

    let tokens = Lexer::parse(&test_source, "test_parser");
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
        let ast_string: String = format!("{:#?}", chunk).lines().map(|line| {
            let mut spaces_count = 0;
            for ch in line.chars() {
                match ch {
                    ' ' => spaces_count += 1,
                    _ => break,
                }
            }
            " ".repeat(spaces_count / 2) + line.trim_start() + "\n"
        }).collect();
        println!("### AST >>>");
        println!("{}", ast_string);
        println!("### AST <<<");
    }
}
