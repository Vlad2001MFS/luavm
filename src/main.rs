mod lexer;

pub use lexer::*;

const TEST_SOURCE1: &str = r#"
io.write
(--[[ my first-- hjglit769780y906 --[[ my --[[ my-- hjglit769780y906 first program in Lua --]]first program in Lua --]]program in Lua --]]
   "Hello \"world, \\from ",_VERSION,"!\n"
   --ghkghkfkgjfkjffkjgj -- hjglit769780y906
)
"#;

const TEST_SOURCE2: &str = r#"
t = {10,52,34,44,86,38}
local v = table.binsearch(t, 6, function(v) return v % 10 end); assert(v[1] == 5)
"#;

fn main() {
    let tokens = Lexer::parse(TEST_SOURCE2, "test_source");
    println!("token = [");
    for token in tokens.iter() {
        println!("  {:?}", token);
    }
    println!("]");
}
