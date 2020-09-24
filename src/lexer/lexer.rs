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
            else if self.stream.look_for("--", true) {
                while self.stream.last_char() != '\n' {
                    self.stream.next(false);
                }
            }
            else if self.stream.last_char() == '\'' || self.stream.last_char() == '"' {
                let mut short_literal = String::new();
                while self.stream.next(false) {
                    if self.stream.last_char() == '\\' {
                        self.stream.next(false);
                        short_literal.push(self.stream.last_char());
                    }
                    else if self.stream.last_char() == '\'' || self.stream.last_char() == '"' {
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
                    if self.stream.last_char().is_alphanumeric() || self.stream.last_char() == '_' {
                        name.push(self.stream.last_char());
                    }
                    else {
                        break;
                    }
                }
                self.tokens.push(Token::Identifier(name));
            }
            else if let Some(token) = OTHER_TOKENS.iter().find(|a| self.stream.look_for(&a, true)) {
                self.tokens.push(Token::Other(token.to_string()));
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