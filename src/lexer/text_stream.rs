use std::{
    fmt::Display,
};

#[derive(Clone)]
pub struct Location {
    line: usize,
    column: usize,
    content: String,
    source_name: String,
}

impl Location {
    pub fn new(content: String, source_name: String) -> Location {
        Location {
            content,
            source_name,
            ..Location::default()
        }
    }

    pub fn update(&mut self, ch: char, lines: &[String]) {
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
            self.content = lines[self.line - 1].clone();
        }
        else {
            self.column += 1;
        }
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
}

impl Default for Location {
    fn default() -> Self {
        Location {
            line: 1,
            column: 1,
            content: String::new(),
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
    current_idx: usize,
    location: Location,
}

impl TextStream {
    pub fn new(src: String, name: String) -> TextStream {
        TextStream {
            data: src.chars().collect(),
            lines: src.lines().map(|a| a.to_owned()).collect(),
            current_idx: 1,
            location: Location::new(src.lines().nth(0).unwrap().to_string(), name),
        }
    }

    pub fn look(&self, offset: usize) -> Option<char> {
        self.data.get(self.current_idx + offset - 1).copied()
    }

    pub fn look_for_str(&mut self, s: &str, start_offset: usize, case_sensitive: bool, extract_readed: bool) -> bool {
        for (i, ch) in s.chars().enumerate() {
            match self.look(start_offset + i) {
                Some(look_ch) => {
                    let is_equal = match case_sensitive {
                        true => look_ch == ch,
                        false => look_ch.to_lowercase().eq(ch.to_lowercase()),
                    };
                    if !is_equal {
                        return false;
                    }
                }
                None => return false,
            }
        }

        if extract_readed {
            for _ in 0..s.len() {
                self.next();
            }
        }

        true
    }

    pub fn skip(&mut self) {
        while !self.is_eof() && self.last_char().is_ascii_whitespace() {
            self.next();
        }
    }

    pub fn next(&mut self) -> bool {
        match self.data.get(self.current_idx) {
            Some(ch) => {
                self.location.update(*ch, &self.lines);
                self.current_idx += 1;
                true
            }
            None => {
                self.current_idx += 1;
                false
            },
        }
    }

    pub fn last_char(&self) -> char {
        self.look(0).unwrap()
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn is_eof(&self) -> bool {
        self.current_idx >= self.data.len()
    }
}