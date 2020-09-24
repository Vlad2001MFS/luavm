mod lexer;

pub use lexer::*;

const TEST_SOURCE1: &str = r#"
io.write
(
   "Hello world, from ",_VERSION,"!\n"
)
"#;

fn main() {
    let tokens = Lexer::parse(TEST_SOURCE1, "test_source");
    println!("token = [");
    for token in tokens.iter() {
        println!("  {:?}", token);
    }
    println!("]");

    std::io::stdin().read_line(&mut String::new()).unwrap();
}
