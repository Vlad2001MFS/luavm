mod lexer;

pub use lexer::*;

const TEST_SOURCE1: &str = r#"
io.write
(--[[ my first-- hjglit769780y906 --[[ my --[[ my-- hjglit769780y906 first program in Lua --]]first program in Lua --]]program in Lua --]]
   "Hello world, from ",_VERSION,"!\n"
   --ghkghkfkgjfkjffkjgj -- hjglit769780y906
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
