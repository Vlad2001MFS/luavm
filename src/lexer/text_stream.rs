use std::{
    fmt::Display,
};

pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    pub fn update(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        }
        else {
            self.column += 1;
        }
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

impl Default for Position {
    fn default() -> Self {
        Position {
            line: 1,
            column: 1,
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}


pub struct TextStream {
    data: Vec<char>,
    lines: Vec<String>,
    current_idx: usize,
    position: Position,
}

impl TextStream {
    pub fn new(src: String) -> TextStream {
        TextStream {
            data: src.chars().collect(),
            lines: src.lines().map(|a| a.to_owned()).collect(),
            current_idx: 1,
            position: Position::default(),
        }
    }

    pub fn look(&self, offset: usize) -> Option<char> {
        self.data.get(self.current_idx + offset - 1).map(|a| *a)
    }

    pub fn look_for_str(&mut self, s: &str, start_offset: usize, case_sensitive: bool, extract_readed: bool) -> bool {
        for (i, ch) in s.chars().enumerate() {
            match self.look(start_offset + i) {
                Some(look_ch) => {
                    match case_sensitive {
                        true => if ch != look_ch {
                            return false;
                        }
                        false => if !ch.to_lowercase().eq(look_ch.to_lowercase()) {
                            return false;
                        }
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
        while self.last_char().is_ascii_whitespace() {
            self.next();
        }
    }

    pub fn next(&mut self) -> bool {
        match self.data.get(self.current_idx) {
            Some(ch) => {
                self.position.update(*ch);
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

    pub fn position(&self) -> &Position {
        &self.position
    }

    pub fn current_line(&self) -> &str {
        &self.lines[self.position.line - 1]
    }

    pub fn is_eof(&self) -> bool {
        self.current_idx >= self.data.len()
    }
}