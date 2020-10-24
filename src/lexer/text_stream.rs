use std::{
    fmt::Display,
    rc::Rc,
};

#[derive(Clone)]
pub struct Location {
    line: usize,
    column: usize,
    lines: Rc<Vec<String>>,
    source_name: String,
}

impl Location {
    pub fn new(lines: Rc<Vec<String>>, source_name: String) -> Location {
        Location {
            lines,
            source_name,
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
        &self.lines
    }

    pub fn content(&self) -> &str {
        &self.lines.get(self.line - 1).expect(&format!("Failed to get content of line {} of source {}", self.line, self.source_name))
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
}

impl Default for Location {
    fn default() -> Self {
        Location {
            line: 1,
            column: 0,
            lines: Rc::new(Vec::new()),
            source_name: String::new(),
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
    lines: Vec<String>,
    next_idx: usize,
    location: Location,
}

impl TextStream {
    pub fn new(src: String, name: String) -> TextStream {
        let lines: Vec<String> = src.lines().map(|a| a.to_owned()).collect();
        let mut stream = TextStream {
            data: src.chars().collect(),
            lines: lines.clone(),
            next_idx: 0,
            location: Location::new(Rc::new(lines), name),
        };
        stream.next();

        stream
    }

    pub fn look(&self, offset: usize) -> Option<char> {
        self.data.get(self.next_idx + offset - 1).copied()
    }

    pub fn look_if<P: FnOnce(char) -> bool>(&self, offset: usize, pred: P) -> Option<char> {
        self.look(offset).filter(|ch| pred(*ch))
    }

    pub fn extract(&mut self) -> char {
        let ch = self.last_char();
        self.next();
        ch
    }

    pub fn extract_if<P: FnOnce(char) -> bool>(&mut self, pred: P) -> Option<char> {
        let ch = self.last_char();
        match pred(ch) {
            true => {
                self.next();
                Some(ch)
            },
            false => None,
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
        &self.lines
    }

    #[track_caller]
    pub fn last_char(&self) -> char {
        self.look(0).expect(&format!("Unexpected end of stream of source '{}'", self.location.source_name))
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn is_eof(&self) -> bool {
        self.look(0).is_none()
    }
}