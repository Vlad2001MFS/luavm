use crate::lexer::{
    TextStream, Location,
};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    String(String),
    Number(f64),
    IntNumber(i64),

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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(name) => f.write_fmt(format_args!("{}", name)),
            Token::String(string) => f.write_fmt(format_args!("{}", string)),
            Token::Number(number) => f.write_fmt(format_args!("{}", number)),
            Token::IntNumber(number) => f.write_fmt(format_args!("{}", number)),
            Token::Function => f.write_str("function"),
            Token::End => f.write_str("end"),
            Token::If => f.write_str("if"),
            Token::Then => f.write_str("then"),
            Token::Else => f.write_str("else"),
            Token::ElseIf => f.write_str("elseif"),
            Token::For => f.write_str("for"),
            Token::While => f.write_str("while"),
            Token::Do => f.write_str("do"),
            Token::Repeat => f.write_str("repeat"),
            Token::Until => f.write_str("until"),
            Token::Or => f.write_str("or"),
            Token::And => f.write_str("and"),
            Token::Not => f.write_str("not"),
            Token::Goto => f.write_str("goto"),
            Token::Break => f.write_str("break"),
            Token::Return => f.write_str("return"),
            Token::In => f.write_str("in"),
            Token::Local => f.write_str("local"),
            Token::Nil => f.write_str("nil"),
            Token::True => f.write_str("true"),
            Token::False => f.write_str("false"),
            Token::Add => f.write_str("+"),
            Token::Sub => f.write_str("-"),
            Token::Mul => f.write_str("*"),
            Token::Div => f.write_str("/"),
            Token::IDiv => f.write_str("//"),
            Token::Pow => f.write_str("^"),
            Token::Mod => f.write_str("%"),
            Token::Len => f.write_str("#"),
            Token::BitNotXor => f.write_str("~"),
            Token::BitAnd => f.write_str("&"),
            Token::BitOr => f.write_str("|"),
            Token::ShiftRight => f.write_str(">>"),
            Token::ShiftLeft => f.write_str("<<"),
            Token::Dots2 => f.write_str(".."),
            Token::Dots3 => f.write_str("..."),
            Token::Assign => f.write_str("="),
            Token::LessThan => f.write_str("<"),
            Token::LessEqual => f.write_str("<="),
            Token::GreaterThan => f.write_str(">"),
            Token::GreaterEqual => f.write_str(">="),
            Token::Equal => f.write_str("=="),
            Token::NotEqual => f.write_str("~="),
            Token::Dot => f.write_str("."),
            Token::SemiColon => f.write_str(";"),
            Token::Colon => f.write_str(":"),
            Token::DoubleColon => f.write_str("::"),
            Token::Comma => f.write_str(","),
            Token::LeftParen => f.write_str("("),
            Token::RightParen => f.write_str(")"),
            Token::LeftBracket => f.write_str("["),
            Token::RightBracket => f.write_str("]"),
            Token::LeftBrace => f.write_str("{"),
            Token::RightBrace => f.write_str("}"),
        }
    }
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
        let src = match src.starts_with("#") {
            true => "--".to_owned() + &src,
            false => src,
        };
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

            if self.try_process_comment()
            || self.try_process_short_string_literal()
            || self.try_process_long_string_literal()
            || self.try_process_identifier()
            || self.try_process_number()
            || self.try_process_symbolic_tokens() {
                self.stream.skip();
                continue;
            }
            else {
                let ch = self.stream.extract();
                self.error(&format!("Unknown character '{}'", ch))
            }
        }
    }

    fn try_process_block(&mut self, gather_content: bool) -> Option<String> {
        let saved_position_stream = self.stream.position();

        if self.eat_char('[') {
            let mut depth = 0;
            while self.eat_char('=') {
                depth += 1;
            }

            if !self.eat_char('[') {
                self.stream.set_position(saved_position_stream);
                return None;
            }

            let closing_brackets = format!("]{}]", "=".repeat(depth));
            let mut content = String::new();

            if gather_content {
                let mut count = 0;
                while !self.match_string(count, &closing_brackets) {
                    count += 1;
                }
                content.reserve(count*4);

                self.eat_char('\n');
                while !self.eat_string(&closing_brackets) {
                    content.push(self.stream.extract());
                }
            }
            else {
                while !self.eat_string(&closing_brackets) {
                    self.stream.next();
                }
            }

            return Some(content);
        }
        None
    }

    fn try_process_comment(&mut self) -> bool {
        if self.eat_string("--") {
            if self.try_process_block(false).is_none() {
                while !self.eat_char('\n') {
                    self.stream.next();
                }
            }
            return true
        }
        false
    }

    fn try_process_short_string_literal(&mut self) -> bool {
        if let Some(str_open_symbol) = self.eat_char_any_of(&['\'', '"']) {
            let mut string = String::new();

            let mut count = 0;
            while !self.match_char(count, str_open_symbol) {
                count += 1;
            }
            string.reserve(count*4);

            while !self.eat_char(str_open_symbol) {
                if self.eat_char('\\') {
                    match self.stream.extract() {
                        '\n' => (),
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
                        'z' => while let Some(_) = self.stream.extract_if(|ch| ch.is_ascii_whitespace()) {},
                        'u' => {
                            self.expect_char('{');

                            let mut number = String::with_capacity(128);
                            while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(16)) {
                                number.push(ch);
                            }

                            self.expect_char('}');

                            let num = match u32::from_str_radix(&number, 16) {
                                Ok(num) => num,
                                Err(err) => self.error(&err.to_string()),
                            };

                            match std::char::from_u32(num) {
                                Some(num) => string.push(num),
                                None => string.push(std::char::REPLACEMENT_CHARACTER),
                            }
                        }
                        'x' => {
                            let mut number = String::with_capacity(128);

                            for _ in 0..2 {
                                match self.stream.extract_if(|ch| ch.is_digit(16)) {
                                    Some(ch) => number.push(ch),
                                    None => self.error("Invalid escaped byte in hexadecimal representation"),
                                };
                            }

                            let num = match u32::from_str_radix(&number, 16) {
                                Ok(num) => num,
                                Err(err) => self.error(&err.to_string()),
                            };

                            string.push(num as u8 as char);
                        }
                        ch if ch.is_digit(10) => {
                            let mut number = ch.to_string();

                            for _ in 0..2 {
                                match self.stream.extract_if(|ch| ch.is_digit(10)) {
                                    Some(ch) => number.push(ch),
                                    None => break,
                                }
                            }

                            let num = match number.parse::<u32>() {
                                Ok(num) => num,
                                Err(err) => self.error(&err.to_string()),
                            };

                            match num < 256 {
                                true => string.push(num as u8 as char),
                                false => self.error(&format!("Invalid value of escaped byte '{}'", num)),
                            };
                        }
                        _ => self.error("Unknown escaped sequence"),
                    };
                }
                else if let Some(ch) = self.stream.extract_if(|ch| !ch.is_ascii_control()) {
                    string.push(ch);
                }
                else {
                    self.error("Short string literal can not contain unescaped control symbols")
                }
            }
            
            self.add_token(Token::String(string));
            return true;
        }
        false
    }

    fn try_process_long_string_literal(&mut self) -> bool {
        match self.try_process_block(true) {
            Some(string) => {
                self.add_token(Token::String(string));
                true
            }
            None => false,
        }
    }

    fn try_process_identifier(&mut self) -> bool {
        if let Some(ch) = self.stream.extract_if(|ch| ch.is_alphabetic() || ch == '_') {
            let mut name = String::with_capacity(256);
            name.push(ch);

            while let Some(ch) = self.stream.extract_if(|ch| ch.is_alphanumeric() || ch == '_') {
                name.push(ch);
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
        if self.eat_string("0x") || self.eat_string("0X") {
            let mut number = String::with_capacity(128);
            let mut is_float = false;

            while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(16)) {
                number.push(ch);
            }

            if !self.match_string(0, "..") && self.eat_char('.') {
                is_float = true;
                number.push('.');
            }

            let mut fractional_digits_count = 0;
            while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(16)) {
                if fractional_digits_count < 13 {
                    fractional_digits_count += 1;
                    number.push(ch);
                }
            }

            if let Some(_) = self.eat_char_any_of(&['p', 'P']) {
                if !is_float {
                    is_float = true;
                    number.push('.');
                }
                number.push('p');

                if let Some(ch) = self.eat_char_any_of(&['+', '-']) {
                    number.push(ch);
                }

                while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(10)) {
                    number.push(ch);
                }
            }
            else if is_float {
                number.push_str("p0");
            }
            
            self.add_token(match is_float {
                true => {
                    match hexf::parse_hexf64(&("0x".to_string() + &number), false) {
                        Ok(number) => Token::Number(number),
                        Err(err) => self.error(&format!("Invalid float hexadecimal number: {}", err)),
                    }
                }
                false => {
                    match i64::from_str_radix(&number, 16) {
                        Ok(number) => Token::IntNumber(number),
                        Err(_) => {
                            match u128::from_str_radix(&number, 16) {
                                Ok(number) => Token::IntNumber(number as i64),
                                Err(err) => self.error(&format!("Invalid hexadecimal number: {}", err)),
                            }
                        }
                    }
                }
            });

            return true;
        }
        else if self.stream.look_if(0, |ch| ch.is_digit(10)).is_some()
             || self.match_char(0, '.') && self.stream.look_if(1, |ch| ch.is_digit(10)).is_some() {
            let mut number = String::with_capacity(128);
            let mut is_float = false;

            while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(10)) {
                number.push(ch);
            }

            if !self.match_string(0, "..") && self.eat_char('.') {
                is_float = true;
                number.push('.');
            }

            let mut fractional_digits_count = 0;
            while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(10)) {
                if fractional_digits_count < 13 {
                    fractional_digits_count += 1;
                    number.push(ch);
                }
            }

            if let Some(_) = self.eat_char_any_of(&['e', 'E']) {
                number.push('e');

                if let Some(ch) = self.eat_char_any_of(&['+', '-']) {
                    number.push(ch);
                }

                while let Some(ch) = self.stream.extract_if(|ch| ch.is_digit(10)) {
                    number.push(ch);
                }
            }

            self.add_token(match is_float {
                true => {
                    let num = match number.parse::<f64>() {
                        Ok(number) => number,
                        Err(err) => self.error(&format!("Invalid float number: {}", err)),
                    };
                    Token::Number(num)
                }
                false => {
                    match number.parse::<i64>() {
                        Ok(number) => Token::IntNumber(number),
                        Err(_) => {
                            match number.parse::<f64>() {
                                Ok(number) => Token::Number(number),
                                Err(err) => self.error(&format!("Invalid number: {}", err)),
                            }
                        }
                    }
                }
            });
            return true;
        }
        false
    }

    fn try_process_symbolic_tokens(&mut self) -> bool {
        if self.eat_string("...") {
            self.add_token(Token::Dots3);
            return true;
        }
        else if self.eat_string("<<") {
            self.add_token(Token::ShiftLeft);
            return true;
        }
        else if self.eat_string(">>") {
            self.add_token(Token::ShiftRight);
            return true;
        }
        else if self.eat_string("//") {
            self.add_token(Token::IDiv);
            return true;
        }
        else if self.eat_string("==") {
            self.add_token(Token::Equal);
            return true;
        }
        else if self.eat_string("~=") {
            self.add_token(Token::NotEqual);
            return true;
        }
        else if self.eat_string("<=") {
            self.add_token(Token::LessEqual);
            return true;
        }
        else if self.eat_string(">=") {
            self.add_token(Token::GreaterEqual);
            return true;
        }
        else if self.eat_string("::") {
            self.add_token(Token::DoubleColon);
            return true;
        }
        else if self.eat_string("..") {
            self.add_token(Token::Dots2);
            return true;
        }
        else if self.eat_char('+') {
            self.add_token(Token::Add);
            return true;
        }
        else if self.eat_char('-') {
            self.add_token(Token::Sub);
            return true;
        }
        else if self.eat_char('*') {
            self.add_token(Token::Mul);
            return true;
        }
        else if self.eat_char('/') {
            self.add_token(Token::Div);
            return true;
        }
        else if self.eat_char('%') {
            self.add_token(Token::Mod);
            return true;
        }
        else if self.eat_char('^') {
            self.add_token(Token::Pow);
            return true;
        }
        else if self.eat_char('#') {
            self.add_token(Token::Len);
            return true;
        }
        else if self.eat_char('&') {
            self.add_token(Token::BitAnd);
            return true;
        }
        else if self.eat_char('~') {
            self.add_token(Token::BitNotXor);
            return true;
        }
        else if self.eat_char('|') {
            self.add_token(Token::BitOr);
            return true;
        }
        else if self.eat_char('<') {
            self.add_token(Token::LessThan);
            return true;
        }
        else if self.eat_char('>') {
            self.add_token(Token::GreaterThan);
            return true;
        }
        else if self.eat_char('=') {
            self.add_token(Token::Assign);
            return true;
        }
        else if self.eat_char('(') {
            self.add_token(Token::LeftParen);
            return true;
        }
        else if self.eat_char(')') {
            self.add_token(Token::RightParen);
            return true;
        }
        else if self.eat_char('{') {
            self.add_token(Token::LeftBrace);
            return true;
        }
        else if self.eat_char('}') {
            self.add_token(Token::RightBrace);
            return true;
        }
        else if self.eat_char('[') {
            self.add_token(Token::LeftBracket);
            return true;
        }
        else if self.eat_char(']') {
            self.add_token(Token::RightBracket);
            return true;
        }
        else if self.eat_char(';') {
            self.add_token(Token::SemiColon);
            return true;
        }
        else if self.eat_char(':') {
            self.add_token(Token::Colon);
            return true;
        }
        else if self.eat_char(',') {
            self.add_token(Token::Comma);
            return true;
        }
        else if self.eat_char('.') {
            self.add_token(Token::Dot);
            return true;
        }
        false
    }

    fn eat_char(&mut self, expected_char: char) -> bool {
        self.stream.extract_if(|ch| ch == expected_char).is_some()
    }

    fn eat_char_any_of(&mut self, expected_chars: &[char]) -> Option<char> {
        for ch in expected_chars.iter().copied() {
            if self.eat_char(ch) {
                return Some(ch);
            }
        }
        None
    }

    fn eat_string(&mut self, expected_string: &str) -> bool {
        let saved_stream_pos = self.stream.position();

        for ch in expected_string.chars() {
            if !self.eat_char(ch) {
                self.stream.set_position(saved_stream_pos);
                return false;
            }
        }

        true
    }

    fn match_char(&mut self, offset: usize, expected_ch: char) -> bool {
        match self.stream.look(offset) {
            Some(ch) if ch != expected_ch => return false,
            Some(_) => (),
            None => return false,
        };

        true
    }

    fn match_string(&mut self, offset: usize, expected_string: &str) -> bool {
        for (i, expected_ch) in expected_string.chars().enumerate() {
            match self.stream.look(offset + i) {
                Some(ch) if ch != expected_ch => return false,
                Some(_) => (),
                None => return false,
            };
        }

        true
    }

    #[track_caller]
    fn expect_char(&mut self, expected_char: char) -> char {
        let ch = self.stream.last_char();
        if ch != expected_char {
            self.error(&format!("Expected '{}' instead '{}", expected_char, ch))
        }
        self.stream.next();
        ch
    }

    #[track_caller]
    fn error<T>(&self, desc: &str) -> T {
        fn build_pointer_str(loc: &Location, len: usize) -> String {
            match loc.column() > 1 {
                true => " ".repeat(loc.column() - 1) + &"^".repeat(len),
                false => "^".repeat(len),
            }
        }

        let token_begin_loc = &self.begin_location;
        let token_end_loc = self.stream.location();

        let mut message = format!("\n{}:{}: Lexer error: {}\n", token_begin_loc.source_name(), token_begin_loc, desc);

        if let Some(line) = self.stream.lines().get(token_begin_loc.line() - 1) {
            let pointer_len = match token_begin_loc.line() == token_end_loc.line() {
                true => token_end_loc.column() - token_begin_loc.column(),
                false => line.chars().count() - token_begin_loc.column() + 1,
            };
            let pointer = build_pointer_str(token_begin_loc, pointer_len);
            message += &format!("{}\n{}\n", line, pointer);

            for i in token_begin_loc.line() + 1..token_end_loc.line() {
                if let Some(line) = self.stream.lines().get(i - 1) {
                    let pointer_len = line.chars().count();
                    let pointer = "^".repeat(pointer_len);
                    message += &format!("{}\n{}\n", line, pointer);
                }
            }

            if token_begin_loc.line() != token_end_loc.line() {
                if let Some(line) = self.stream.lines().get(token_end_loc.line() - 1) {
                    let pointer_len = token_end_loc.column();
                    let pointer = build_pointer_str(token_end_loc, pointer_len);
                    message += &format!("{}\n{}\n", line, pointer);
                }
            }
        }
        panic!(message);
    }
}