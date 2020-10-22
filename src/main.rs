extern crate hexf;

pub mod lexer;
pub mod parser;

use lexer::{
    Lexer,
};
use parser::{
    Parser,
};
use std::{
    path::{
        Path, PathBuf,
    },
    fs::File,
    io::Write,
};

fn run_tests(test_dir_path: &str) {
    let result_dir = PathBuf::from(test_dir_path).join("../").join(Path::new(test_dir_path).file_name().unwrap().to_string_lossy().to_string() + "-result");

    if let Ok(meta) = std::fs::metadata(&result_dir) {
        if meta.is_dir() {
            std::fs::remove_dir_all(&result_dir).unwrap();
        }
    }

    std::fs::create_dir(&result_dir).unwrap();

    for entry in std::fs::read_dir(test_dir_path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let test_file_name = path.file_name().unwrap().to_str().unwrap();
        let test_file_stem = path.file_stem().unwrap().to_str().unwrap();

        if path.extension().unwrap() == "lua" {
            let test_source = std::fs::read_to_string(&path).unwrap();

            let tokens = Lexer::parse(&test_source, test_file_name);

            let mut tokens_file = File::create(result_dir.join(test_file_stem.to_owned() + ".tokens.txt")).unwrap();
            for token in tokens.iter() {
                writeln!(tokens_file, "  {:?}", token.token()).unwrap();
            }

            let chunk = Parser::parse(tokens);

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

            let mut ast_file = File::create(result_dir.join(test_file_stem.to_owned() + ".ast.txt")).unwrap();
            writeln!(ast_file, "{}", ast_string).unwrap();

            println!("### TEST: {} --- OK ###", test_file_name);
        }
    }
}

fn main() {
    run_tests("./lua-5.4.1-tests");
}
