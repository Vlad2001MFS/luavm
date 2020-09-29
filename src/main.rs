extern crate hexf;

mod lexer;

pub use lexer::*;

const TEST_SOURCE: &str = r#"
io.write
(--[[ my first-- hjglit769780y906 --[[ my --[[ my-- hjglit769780y906 first program in Lua --]]first program in Lua --]]program in Lua --]]
   "Hello \"world, \\from ",_VERSION,"!\n"
   --ghkghkfkgjfkjffkjgj -- hjglit769780y906
)

test = {
    3,   345,   0xff,   0xBEBADA,
    -1.0, 3.0,     3.1416,     314.16e-2,     0.31416E1,     34e1,
    -0x0.1E, 0x0.1E,  0xA23p-4,   0X1.921FB54442D18P+1
}

for k, v in pairs(test) do
    print(v)
end
"#;

fn main() {
    let tokens = Lexer::parse(TEST_SOURCE, "test_source");
    println!("token = [");
    for token in tokens.iter() {
        println!("  {:?}", token);
    }
    println!("]");
}
