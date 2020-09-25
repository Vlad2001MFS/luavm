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

const HEX_CHARS: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    ShortLiteral(String),
    Int(i64),
    Real(f64),
    Other(String),

    // Keywords
    Function,
    End,

    If,
    Then,
    Else,
    ElseIf,

    For,
    While,
    Do,
    Repeat,
    Until,

    Or,
    And,
    Not,

    Goto,
    Break,
    Return,

    In,
    Local,
    Nil,
    True,
    False,
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

                self.tokens.push(match name.as_str() {
                    "function" => Token::Function,
                    "end" => Token::End,

                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "elseif" => Token::ElseIf,

                    "for" => Token::For,
                    "while" => Token::While,
                    "do" => Token::Do,
                    "repeat" => Token::Repeat,
                    "until" => Token::Until,

                    "or" => Token::Or,
                    "and" => Token::And,
                    "not" => Token::Not,

                    "goto" => Token::Goto,
                    "break" => Token::Break,
                    "return" => Token::Return,

                    "in" => Token::In,
                    "local" => Token::Local,
                    "nil" => Token::Nil,
                    "true" => Token::True,
                    "false" => Token::False,

                    _ => Token::Identifier(name),
                });
            }
            else if self.stream.last_char().is_digit(10) || (self.stream.last_char() == '-' && self.stream.look(1).unwrap_or('\0').is_digit(10)) {
                if self.stream.look(1).unwrap_or('\0').to_ascii_lowercase() == 'x' {
                    self.stream.next(false);

                    let mut number = String::new();
                    while self.stream.next(false) {
                        if HEX_CHARS.iter().any(|a| *a == self.stream.last_char()) {
                            number.push(self.stream.last_char());
                        }
                        else {
                            break;
                        }
                    }
                    
                    self.tokens.push(Token::Int(i64::from_str_radix(&number, 16).unwrap()));
                }
                else {
                    let mut number = self.stream.last_char().to_string();
                    while self.stream.next(false) {
                        if self.stream.last_char().is_digit(10) {
                            number.push(self.stream.last_char());
                        }
                        else {
                            break;
                        }
                    }
    
                    self.tokens.push(Token::Int(number.parse().unwrap()));
                }
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