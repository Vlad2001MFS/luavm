use std::{
    fmt::Display,
    rc::Rc,
};

#[derive(Clone)]
pub struct SourceInfo {
    name: String,
    lines: Vec<String>,
}

#[derive(Clone)]
pub struct Location {
    line: usize,
    column: usize,
    source_info: Rc<SourceInfo>,
}

impl Location {
    pub fn new(lines: Vec<String>, source_name: String) -> Location {
        Location {
            source_info: Rc::new(SourceInfo {
                name: source_name,
                lines,
            }),
            ..Location::default()
        }
    }

    pub fn update(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
        }
        else {
            self.column += 1;
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn lines(&self) -> &[String] {
        &self.source_info.lines
    }

    pub fn content(&self) -> &str {
        &self.source_info.lines.get(self.line - 1).expect(&format!("Failed to get content of line {} of source {}", self.line(), self.source_name()))
    }

    pub fn source_name(&self) -> &str {
        &self.source_info.name
    }
}

impl Default for Location {
    fn default() -> Self {
        Location {
            line: 1,
            column: 0,
            source_info: Rc::new(SourceInfo {
                name: String::new(),
                lines: Vec::new(),
            }),
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}


pub struct TextStream {
    data: Vec<char>,
    next_idx: usize,
    location: Location,
}

impl TextStream {
    pub fn new(src: String, name: String) -> TextStream {
        let mut stream = TextStream {
            data: src.chars().collect(),
            next_idx: 0,
            location: Location::new(src.lines().map(|a| a.to_owned()).collect(), name),
        };
        stream.next();
        stream
    }

    pub fn look(&self, offset: usize) -> Option<char> {
        self.data.get(self.next_idx + offset - 1).copied()
    }

    pub fn look_if_digit(&self, offset: usize, radix: u32) -> Option<char> {
        match self.look(offset) {
            Some(ch) if ch.is_digit(radix) => Some(ch),
            _ => None,
        }
    }

    pub fn extract(&mut self) -> char {
        let ch = self.last_char();
        self.next();
        ch
    }
    
    pub fn extract_if_digit(&mut self, radix: u32) -> Option<char> {
        match self.look(0) {
            Some(ch) if ch.is_digit(radix) => {
                self.next();
                Some(ch)
            },
            _ => None,
        }
    }
    
    pub fn extract_if_alphabetic(&mut self, allow_underscore: bool) -> Option<char> {
        match self.look(0) {
            Some(ch) if ch.is_alphabetic() || ch == '_' && allow_underscore => {
                self.next();
                Some(ch)
            },
            _ => None,
        }
    }
    
    pub fn extract_if_alphanumeric(&mut self, allow_underscore: bool) -> Option<char> {
        match self.look(0) {
            Some(ch) if ch.is_alphanumeric() || ch == '_' && allow_underscore => {
                self.next();
                Some(ch)
            },
            _ => None,
        }
    }

    pub fn skip(&mut self) {
        while let Some(ch) = self.look(0) {
            match ch.is_ascii_whitespace() {
                true => self.next(),
                false => break,
            };
        }
    }

    pub fn next(&mut self) -> bool {
        match self.data.get(self.next_idx) {
            Some(ch) => {
                self.location.update(*ch);
                self.next_idx += 1;
                true
            }
            None => {
                self.next_idx += 1;
                false
            },
        }
    }

    pub fn set_position(&mut self, position: (usize, Location)) {
        self.next_idx = position.0;
        self.location = position.1;
    }

    pub fn position(&self) -> (usize, Location) {
        (self.next_idx, self.location.clone())
    }

    pub fn lines(&self) -> &[String] {
        self.location.lines()
    }

    #[track_caller]
    pub fn last_char(&self) -> char {
        self.look(0).expect(&format!("Unexpected end of stream of source '{}'", self.location.source_name()))
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn is_eof(&self) -> bool {
        self.look(0).is_none()
    }
}