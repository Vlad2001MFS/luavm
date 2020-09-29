use crate::lexer::{
    TextStream,
};

const SYMBOLIC_TOKENS: [&str; 33] = [
    "+",    "-",    "*",    "/",    "%",    "^",   "#",
    "&",    "~",    "|",    "<<",   ">>",   "//",
    "==",   "~=",   "<=",   ">=",   "<",    ">",   "=",
    "(",    ")",    "{",    "}",    "[",    "]",   "::",
    ";",    ":",    ",",    ".",    "..",   "...",
];

const HEX_DIGIT: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

fn is_hex_digit(ch: char) -> bool {
    HEX_DIGIT.contains(&ch.to_ascii_uppercase())
}

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    String(String),
    Number(f64),
    Symbol(String),

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
            
            if self.try_process_block_comment()
            || self.try_process_line_comment()
            || self.try_process_short_string_literal()
            || self.try_process_identifier()
            || self.try_process_number()
            || self.try_process_symbolic_tokens() {
                continue;
            }
            else {
                self.error(&format!("Unknown character '{}'", self.stream.last_char()));
            }
        }
    }

    fn try_process_block_comment(&mut self) -> bool {
        if self.stream.look_for("--[[", 0, false, true) {
            let mut depth = 1;
            while depth > 0 {
                if self.stream.look_for("--[[", 0, false, true) {
                    depth += 1;
                }
                else if self.stream.look_for("--]]", 0, false, true) {
                    depth -= 1;
                }
                self.stream.next(false);
            }
            return true
        }
        false
    }

    fn try_process_line_comment(&mut self) -> bool {
        if self.stream.look_for("--", 0, false, true) {
            while self.stream.last_char() != '\n' {
                self.stream.next(false);
            }
            return true;
        }
        false
    }

    fn try_process_short_string_literal(&mut self) -> bool {
        if self.stream.last_char() == '\'' || self.stream.last_char() == '"' {
            let mut string = String::new();

            while self.stream.next(false) && self.stream.last_char() != '\'' && self.stream.last_char() != '"' {
                if self.stream.last_char() == '\\' {
                    self.stream.next(false);
                }
                
                string.push(self.stream.last_char());
            }
            self.stream.next(false);

            self.tokens.push(Token::String(string));
            return true;
        }
        false
    }

    fn try_process_identifier(&mut self) -> bool {
        if self.stream.last_char().is_alphabetic() || self.stream.last_char() == '_' {
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
            return true;
        }
        false
    }

    fn try_process_number(&mut self) -> bool {
        let (is_negative, start_offset) = match self.stream.last_char() {
            '+' => (false, 1),
            '-' => (true, 1),
            _ => (false, 0),
        };

        if self.stream.look_for("0x", start_offset, false, false) {
            if start_offset > 0 {
                self.stream.next(false);
            }
            self.stream.next(false);

            let mut number = "0x".to_string();
            let mut has_digit = false;
            let mut has_dot = false;
            let mut has_exponent = false;

            while self.stream.next(false) {
                if is_hex_digit(self.stream.last_char()) {
                    has_digit = true;
                    number.push(self.stream.last_char());
                }
                else if self.stream.last_char() == '.' {
                    if has_dot {
                        self.error("Invalid hexadecimal number. More than 1 dot in a number");
                    }

                    has_dot = true;
                    number.push(self.stream.last_char());
                }
                else if self.stream.last_char().eq_ignore_ascii_case(&'p') {
                    if has_exponent {
                        self.error("Invalid hexadecimal number. More than 1 exponent in a number");
                    }
                    if !has_digit {
                        self.error("Invalid hexadecimal number. The exponent requires at least one digit in a number");
                    }

                    has_exponent = true;
                    number.push(self.stream.last_char());

                    if self.stream.look_for("-", 1, false, true) {
                        number.push('-');
                    }
                    else if self.stream.look_for("+", 1, false, true) {
                        number.push('+');
                    }
                }
                else {
                    break;
                }
            }
            
            let num = match has_dot {
                true => match has_exponent {
                    true => hexf::parse_hexf64(&number, false).unwrap(),
                    false => hexf::parse_hexf64(&(number + "p0"), false).unwrap(),
                }
                false => match has_exponent {
                    true => {
                        let insert_dot_idx = number.find(|a: char| a.eq_ignore_ascii_case(&'p')).unwrap();
                        number.insert(insert_dot_idx, '.');
                        hexf::parse_hexf64(&number, false).unwrap()
                    },
                    false => i64::from_str_radix(&number.trim_start_matches("0x"), 16).unwrap() as f64,
                }
            };

            self.tokens.push(Token::Number(match is_negative {
                true => -num,
                false => num
            }));
            return true;
        }
        else if self.stream.look(start_offset).map_or(false, |a| a.is_digit(10)) {
            if start_offset > 0 {
                self.stream.next(false);
            }
            
            let mut number = self.stream.last_char().to_string();
            let mut has_digit = false;
            let mut has_dot = false;
            let mut has_exponent = false;

            while self.stream.next(false) {
                if self.stream.last_char().is_digit(10) {
                    has_digit = true;
                    number.push(self.stream.last_char());
                }
                else if self.stream.last_char() == '.' {
                    if has_dot {
                        self.error("Invalid number. More than 1 dot in a number");
                    }

                    has_dot = true;
                    number.push(self.stream.last_char());
                }
                else if self.stream.last_char().eq_ignore_ascii_case(&'e') {
                    if has_exponent {
                        self.error("Invalid number. More than 1 exponent in a number");
                    }
                    if !has_digit {
                        self.error("Invalid number. The exponent requires at least one digit in a number");
                    }

                    has_exponent = true;
                    number.push(self.stream.last_char());

                    if self.stream.look_for("-", 1, false, true) {
                        number.push('-');
                    }
                    else if self.stream.look_for("+", 1, false, true) {
                        number.push('+');
                    }
                }
                else {
                    break;
                }
            }

            let num = number.parse::<f64>().unwrap();
            self.tokens.push(Token::Number(match is_negative {
                true => -num,
                false => num
            }));
        }
        false
    }

    fn try_process_symbolic_tokens(&mut self) -> bool {
        if let Some(token) = SYMBOLIC_TOKENS.iter().find(|a| self.stream.look_for(&a, 0, false, true)) {
            self.tokens.push(Token::Symbol(token.to_string()));
            return true;
        }
        false
    }

    fn error(&self, desc: &str) {
        let pointer = " ".repeat(self.stream.position().column() - 1) + "^";
        panic!(format!("{}:{}: {}\n{}\n{}", self.name, self.stream.position(), desc, self.stream.current_line(), pointer))
    }
}