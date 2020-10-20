use crate::lexer::{
    TextStream, Location,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    String(String),
    Number(f64),

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
 
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Pow,
    Mod,
    Len,
    BitNotXor,
    BitAnd,
    BitOr,
    ShiftRight,
    ShiftLeft,
    Dots2,
    Dots3,
    Assign,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    Dot,
    SemiColon,
    Colon,
    DoubleColon,
    Comma,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

pub struct TokenInfo {
    token: Token,
    begin_location: Location,
    end_location: Location,
}

impl TokenInfo {
    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn begin_location(&self) -> &Location {
        &self.begin_location
    }

    pub fn end_location(&self) -> &Location {
        &self.end_location
    }
}

pub struct Lexer {
    stream: TextStream,
    tokens: Vec<TokenInfo>,
    begin_location: Location,
}

impl Lexer {
    pub fn parse(src: &str, name: &str) -> Vec<TokenInfo> {
        let src = src.replace("\r\n", "\n").replace("\n\r", "\n").replace("\r", "\n");
        let mut lexer = Lexer {
            stream: TextStream::new(src, name.to_string()),
            tokens: Vec::new(),
            begin_location: Location::default(),
        };
        lexer.process();
        lexer.tokens
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(TokenInfo {
            token,
            begin_location: self.begin_location.clone(),
            end_location: self.stream.location().clone(),
        });
    }

    fn process(&mut self) {
        self.stream.skip();
        while !self.stream.is_eof() {
            self.begin_location = self.stream.location().clone();

            if self.try_process_block_comment()
            || self.try_process_line_comment()
            || self.try_process_short_string_literal()
            || self.try_process_long_string_literal()
            || self.try_process_identifier()
            || self.try_process_number()
            || self.try_process_symbolic_tokens() {
                self.stream.skip();
                continue;
            }
            else {
                match self.stream.look(0) {
                    Some(ch) => self.error(&format!("Unknown character '{}'", ch)),
                    None => self.error(&format!("Unexpected end of source"))
                }
            }
        }
    }

    fn try_process_line_comment(&mut self) -> bool {
        if self.stream.look_for_str("--", 0, false, true) {
            while self.stream.last_char() != '\n' {
                if !self.stream.next() {
                    break;
                }
            }
            return true;
        }
        false
    }

    fn try_process_block_comment(&mut self) -> bool {
        if self.stream.look_for_str("--[[", 0, false, true) {
            while !self.stream.look_for_str("]]", 0, false, true) {
                if !self.stream.next() {
                    break;
                }
            }
            return true;
        }
        false
    }

    fn try_process_short_string_literal(&mut self) -> bool {
        if self.stream.last_char() == '\'' || self.stream.last_char() == '"' {
            let str_open_symbol = self.stream.last_char();
            let mut string = String::new();

            while self.stream.next() && self.stream.last_char() != str_open_symbol {
                if self.stream.last_char() == '\\' {
                    self.stream.next();
                    match self.stream.last_char() {
                        'a' => string.push(0x07 as char),
                        'b' => string.push(0x07 as char),
                        'f' => string.push(0x0C as char),
                        'n' => string.push('\n'),
                        'r' => string.push('\r'),
                        't' => string.push('\t'),
                        'v' => string.push(0x0B as char),
                        '\\' => string.push('\\'),
                        '\"' => string.push('\"'),
                        '\'' => string.push('\''),
                        'z' => self.stream.skip(),
                        'u' => {
                            self.stream.next();
                            if self.stream.look_for_str("{", 0, false, false) {
                                let mut num = 0;

                                let mut i = 0;
                                while self.stream.look(i + 1).map_or(false, |ch| ch != '}') {
                                    i += 1;
                                }

                                for _ in 0..i {
                                    if self.stream.next() && self.stream.last_char().is_digit(16) {
                                        num += self.stream.last_char().to_digit(16).unwrap()*16_u32.pow((i - 1) as u32);
                                    }
                                    else {
                                        self.error("Invalid escaped byte in hexadecimal representation")
                                    }

                                    i -= 1;
                                }
                                self.stream.next();

                                match std::char::from_u32(num) {
                                    Some(num) => string.push(num),
                                    None => self.error("Invalid unicode value"),
                                }
                            }
                        }
                        ch if ch.is_digit(10) => {
                            let mut num = 0;

                            for i in 0..3 {
                                if self.stream.last_char().is_digit(10) {
                                    num += self.stream.last_char().to_digit(10).unwrap()*10_u32.pow(2 - i);

                                    if i < 2 && self.stream.look(1).map_or(false, |ch| ch.is_digit(10)) {
                                        self.stream.next();
                                    }
                                    else {
                                        num /= 10_u32.pow(2 - i);
                                        break;
                                    }
                                }
                            }

                            if num < 256 {
                                string.push(num as u8 as char);
                            }
                            else {
                                self.error(&format!("Invalid value of escaped byte '{}'", num));
                            }
                        }
                        ch if ch == 'x' => {
                            let mut num = 0;

                            for i in 0..2 {
                                if self.stream.next() && self.stream.last_char().is_digit(16) {
                                    num += self.stream.last_char().to_digit(16).unwrap()*16_u32.pow(1 - i);
                                }
                                else {
                                    self.error("Invalid escaped byte in hexadecimal representation")
                                }
                            }

                            string.push(num as u8 as char);
                        }
                        _ => self.error("Unknown escaped sequence"),
                    }
                }
                else if !self.stream.last_char().is_ascii_control() {
                    string.push(self.stream.last_char());
                }
                else {
                    self.error("Short string literal can not contain unescaped control symbols");
                }
            }
            self.stream.next();
            
            self.add_token(Token::String(string));
            return true;
        }
        false
    }

    fn try_process_long_string_literal(&mut self) -> bool {
        if self.stream.look_for_str("[[", 0, false, true) {
            let mut string = String::new();

            let mut first = true;
            while !self.stream.look_for_str("]]", 0, false, true) {
                if self.stream.look_for_str("\n", 0, false, true) && !first {
                    string.push('\n');
                }
                else {
                    string.push(self.stream.last_char());
                }

                if !self.stream.next() {
                    break;
                }
                
                first = false;
            }

            self.add_token(Token::String(string));
            return true;
        }
        false
    }

    fn try_process_identifier(&mut self) -> bool {
        if self.stream.last_char().is_alphabetic() || self.stream.last_char() == '_' {
            let mut name = self.stream.last_char().to_string();

            while self.stream.next() && (self.stream.last_char().is_alphanumeric() || self.stream.last_char() == '_') {
                name.push(self.stream.last_char());
            }

            self.add_token(match name.as_str() {
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
        if self.stream.look_for_str("0x", 0, false, false) {
            self.stream.next();

            let mut number = "0x".to_string();
            let mut has_digit = false;
            let mut has_dot = false;
            let mut has_exponent = false;

            while self.stream.next() {
                if self.stream.last_char().is_digit(16) {
                    has_digit = true;
                    number.push(self.stream.last_char());
                }
                else if self.stream.last_char() == '.' && !self.stream.look_for_str("..", 0, false, false) {
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

                    if self.stream.look_for_str("-", 1, false, true) {
                        number.push('-');
                    }
                    else if self.stream.look_for_str("+", 1, false, true) {
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

            self.add_token(Token::Number(num));
            return true;
        }
        else if self.stream.look(0).map_or(false, |a| a.is_digit(10)) {
            let mut number = self.stream.last_char().to_string();
            let mut has_digit = false;
            let mut has_dot = false;
            let mut has_exponent = false;

            while self.stream.next() {
                if self.stream.last_char().is_digit(10) {
                    has_digit = true;
                    number.push(self.stream.last_char());
                }
                else if self.stream.last_char() == '.' && !self.stream.look_for_str("..", 0, false, false) {
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

                    if self.stream.look_for_str("-", 1, false, true) {
                        number.push('-');
                    }
                    else if self.stream.look_for_str("+", 1, false, true) {
                        number.push('+');
                    }
                }
                else {
                    break;
                }
            }

            let num = number.parse::<f64>().unwrap();
            self.add_token(Token::Number(num));
            return true;
        }
        false
    }

    fn try_process_symbolic_tokens(&mut self) -> bool {
        if self.stream.look_for_str("...", 0, false, true) {
            self.add_token(Token::Dots3);
            return true;
        }
        else if self.stream.look_for_str("<<", 0, false, true) {
            self.add_token(Token::ShiftLeft);
            return true;
        }
        else if self.stream.look_for_str(">>", 0, false, true) {
            self.add_token(Token::ShiftRight);
            return true;
        }
        else if self.stream.look_for_str("//", 0, false, true) {
            self.add_token(Token::IDiv);
            return true;
        }
        else if self.stream.look_for_str("==", 0, false, true) {
            self.add_token(Token::Equal);
            return true;
        }
        else if self.stream.look_for_str("~=", 0, false, true) {
            self.add_token(Token::NotEqual);
            return true;
        }
        else if self.stream.look_for_str("<=", 0, false, true) {
            self.add_token(Token::LessEqual);
            return true;
        }
        else if self.stream.look_for_str(">=", 0, false, true) {
            self.add_token(Token::GreaterEqual);
            return true;
        }
        else if self.stream.look_for_str("::", 0, false, true) {
            self.add_token(Token::DoubleColon);
            return true;
        }
        else if self.stream.look_for_str("..", 0, false, true) {
            self.add_token(Token::Dots2);
            return true;
        }
        else if self.stream.look_for_str("+", 0, false, true) {
            self.add_token(Token::Add);
            return true;
        }
        else if self.stream.look_for_str("-", 0, false, true) {
            self.add_token(Token::Sub);
            return true;
        }
        else if self.stream.look_for_str("*", 0, false, true) {
            self.add_token(Token::Mul);
            return true;
        }
        else if self.stream.look_for_str("/", 0, false, true) {
            self.add_token(Token::Div);
            return true;
        }
        else if self.stream.look_for_str("%", 0, false, true) {
            self.add_token(Token::Mod);
            return true;
        }
        else if self.stream.look_for_str("^", 0, false, true) {
            self.add_token(Token::Pow);
            return true;
        }
        else if self.stream.look_for_str("#", 0, false, true) {
            self.add_token(Token::Len);
            return true;
        }
        else if self.stream.look_for_str("&", 0, false, true) {
            self.add_token(Token::BitAnd);
            return true;
        }
        else if self.stream.look_for_str("~", 0, false, true) {
            self.add_token(Token::BitNotXor);
            return true;
        }
        else if self.stream.look_for_str("|", 0, false, true) {
            self.add_token(Token::BitOr);
            return true;
        }
        else if self.stream.look_for_str("<", 0, false, true) {
            self.add_token(Token::LessThan);
            return true;
        }
        else if self.stream.look_for_str(">", 0, false, true) {
            self.add_token(Token::GreaterThan);
            return true;
        }
        else if self.stream.look_for_str("=", 0, false, true) {
            self.add_token(Token::Assign);
            return true;
        }
        else if self.stream.look_for_str("(", 0, false, true) {
            self.add_token(Token::LeftParen);
            return true;
        }
        else if self.stream.look_for_str(")", 0, false, true) {
            self.add_token(Token::RightParen);
            return true;
        }
        else if self.stream.look_for_str("{", 0, false, true) {
            self.add_token(Token::LeftBrace);
            return true;
        }
        else if self.stream.look_for_str("}", 0, false, true) {
            self.add_token(Token::RightBrace);
            return true;
        }
        else if self.stream.look_for_str("[", 0, false, true) {
            self.add_token(Token::LeftBracket);
            return true;
        }
        else if self.stream.look_for_str("]", 0, false, true) {
            self.add_token(Token::RightBracket);
            return true;
        }
        else if self.stream.look_for_str(";", 0, false, true) {
            self.add_token(Token::SemiColon);
            return true;
        }
        else if self.stream.look_for_str(":", 0, false, true) {
            self.add_token(Token::Colon);
            return true;
        }
        else if self.stream.look_for_str(",", 0, false, true) {
            self.add_token(Token::Comma);
            return true;
        }
        else if self.stream.look_for_str(".", 0, false, true) {
            self.add_token(Token::Dot);
            return true;
        }
        false
    }

    fn error(&self, desc: &str) {
        let pointer_len = match self.stream.location().column() > 0 {
            true => self.stream.location().column() - 1,
            false => 1,
        };
        let pointer = " ".repeat(pointer_len) + "^";
        panic!(format!("{}:{}: Lexer error: {}\n{}\n{}", self.stream.location().source_name(), self.stream.location(), desc, self.stream.location().content(), pointer))
    }
}