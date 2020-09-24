mod lexer;

pub use lexer::*;

const TEST_SOURCE1: &str = r#"
io.write
(--[[ my first-- hjglit769780y906 --[[ my --[[ my-- hjglit769780y906 first program in Lua --]]first program in Lua --]]program in Lua --]]
   "Hello \"world, \\from ",_VERSION,"!\n"
   --ghkghkfkgjfkjffkjgj -- hjglit769780y906
)
"#;

fn main() {
    let tokens = Lexer::parse(TEST_SOURCE1, "test_source");
    let mut tokens_str = String::new();
    tokens_str.push_str("token = [\n");
    for token in tokens.iter() {
        tokens_str.push_str(&format!("  {:?}\n", token));
    }
    tokens_str.push_str("]");
    println!("{}", tokens_str);
}
