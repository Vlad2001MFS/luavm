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
    time::{
        SystemTime, Duration,
    },
};

fn run_tests(test_dir_path: &str) {
    let result_dir = PathBuf::from(test_dir_path).join("../").join(Path::new(test_dir_path).file_name().unwrap().to_string_lossy().to_string() + "-result");

    if let Ok(meta) = std::fs::metadata(&result_dir) {
        if meta.is_dir() {
            std::fs::remove_dir_all(&result_dir).unwrap();
        }
    }

    std::fs::create_dir(&result_dir).unwrap();

    let mut lexer_total_time = Duration::new(0, 0);
    let mut parser_total_time = Duration::new(0, 0);

    let mut skip_test = false;
    for entry in std::fs::read_dir(test_dir_path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let test_file_name = path.file_name().unwrap().to_str().unwrap();
        let test_file_stem = path.file_stem().unwrap().to_str().unwrap();

        //if test_file_stem != "" { continue; }

        if test_file_stem == "" {
            skip_test = false;
        }

        if !skip_test {
            if let Some(Some("lua")) = path.extension().map(|ext| ext.to_str()) {
                let test_source_bytes = std::fs::read(&path).expect(&format!("Failed to load test '{}'", path.to_string_lossy()));
                let test_source = String::from_utf8_lossy(&test_source_bytes).to_string();

                let lexer_start_time = SystemTime::now();
                let tokens = Lexer::parse(&test_source, test_file_name);
                let lexer_elapsed_time = lexer_start_time.elapsed().unwrap();
                lexer_total_time += lexer_elapsed_time;

                let mut tokens_file = File::create(result_dir.join(test_file_stem.to_owned() + ".tokens.txt")).unwrap();
                for token in tokens.iter() {
                    writeln!(tokens_file, "{:?}", token.token()).unwrap();
                }

                let parser_start_time = SystemTime::now();
                let chunk = Parser::parse(tokens);
                let parser_elapsed_time = parser_start_time.elapsed().unwrap();
                parser_total_time += parser_elapsed_time;

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

                println!("### TEST: {} --- OK ### LexerTime: {} ms || ParserTime: {} ms", test_file_name, lexer_elapsed_time.as_millis(), parser_elapsed_time.as_millis());
            }
        }
    }

    println!("###### Total time ######");
    println!("Lexer: {} s || {} ms", lexer_total_time.as_secs_f64(), lexer_total_time.as_millis());
    println!("Parser: {} s || {} ms", parser_total_time.as_secs_f64(), parser_total_time.as_millis())
}

fn main() {
    if true {
        run_tests("./lua-5.4.1-tests");
    }
    else {
        let test_source = r#"
        a = 0xFFFFFFFFFFFFFFFF
        assert(a == -1 and a & -1 == a and a & 35 == 35)
        "#;

        let tokens = Lexer::parse(&test_source, "test_source");
        if false {
            println!("token = [");
            for token in tokens.iter() {
                println!("  {:?}", token.token());
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
}
