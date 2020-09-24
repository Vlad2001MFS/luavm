use crate::lexer::{
    TextStream,
};

const OTHER_TOKENS: [&str; 33] = [
    "+",    "-",    "*",    "/",    "%",    "^",   "#",
    "&",    "~",    "|",    "<<",   ">>",   "//",
    "==",   "~=",   "<=",   ">=",   "<",    ">",   "=",
    "(",    ")",    "{",    "}",    "[",    "]",   "::",
    ";",    ":",    ",",    ".",    "..",   "...",
];

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    ShortLiteral(String),
    Other(String),
}

pub struct Lexer {
    name: String,
    stream: TextStream,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn parse(src: &str, name: &str) -> Vec<Token> {
        let mut lexer = Lexer {
            name: name.to_string(),
            stream: TextStream::new(src.to_string()),
            tokens: Vec::new(),
        };
        lexer.process();
        lexer.tokens
    }

    fn process(&mut self) {
        while !self.stream.is_eof() {
            self.stream.skip();

            if self.stream.look_for("--[[", true) {
                let mut depth = 1;
                while depth > 0 {
                    if self.stream.look_for("--]]", true) {
                        depth -= 1;
                    }
                    else if self.stream.look_for("--[[", true) {
                        depth += 1;
                    }
                    self.stream.next(false);
                }
            }
            else if self.stream.last_char() == '\'' || self.stream.last_char() == '"' {
                let mut short_literal = String::new();
                while self.stream.next(false) {
                    if self.stream.last_char() == '\'' || self.stream.last_char() == '"' {
                        self.stream.next(false);
                        break;
                    }
                    else {
                        short_literal.push(self.stream.last_char());
                    }
                }
                self.tokens.push(Token::ShortLiteral(short_literal));
            }
            else if self.stream.last_char().is_alphabetic() || self.stream.last_char() == '_' {
                let mut name = self.stream.last_char().to_string();
                while self.stream.next(false) {
                    if self.stream.last_char().is_alphanumeric() {
                        name.push(self.stream.last_char());
                    }
                    else {
                        break;
                    }
                }
                self.tokens.push(Token::Identifier(name));
            }
            else if OTHER_TOKENS.iter().any(|a| a.starts_with(self.stream.last_char())) {
                let mut lexem = self.stream.last_char().to_string();
                while self.stream.next(false) {
                    let new_lexem = lexem.clone() + &self.stream.last_char().to_string();
                    if OTHER_TOKENS.iter().any(|a| *a == &new_lexem) {
                        break;
                    }
                    else if OTHER_TOKENS.iter().any(|a| a.starts_with(&new_lexem)) {
                        lexem = new_lexem;
                    }
                    else {
                        break;
                    }
                }
                self.tokens.push(Token::Other(lexem));
            }
            else {
                self.error(&format!("Unknown character '{}'", self.stream.last_char()));
            }
        }
    }

    fn error(&self, desc: &str) {
        panic!(format!("{}:{}: {}", self.name, self.stream.position(), desc))
    }
}