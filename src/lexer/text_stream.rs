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
    last_char: char,
    current_idx: usize,
    position: Position,
}

impl TextStream {
    pub fn new(src: String) -> TextStream {
        let data: Vec<char> = src.chars().collect();
        let last_char = data[0];
        TextStream {
            data,
            last_char,
            current_idx: 1,
            position: Position::default(),
        }
    }

    pub fn look_for(&mut self, s: &str, extract: bool) -> bool {
        for (i, ch) in s.chars().enumerate() {
            match self.look(i) {
                Some(look_ch) => {
                    if ch != look_ch {
                        return false;
                    }
                }
                None => return false,
            }
        }

        if extract {
            for _ in 0..s.len() {
                self.next(false);
            }
        }

        true
    }

    pub fn look(&self, offset: usize) -> Option<char> {
        self.data.get(self.current_idx + offset - 1).map(|a| *a)
    }

    pub fn skip(&mut self) {
        while self.last_char.is_whitespace() {
            self.next(false);
        }
    }

    pub fn next(&mut self, skip_whitespace: bool) -> bool {
        if skip_whitespace {
            self.skip()
        }

        match self.data.get(self.current_idx) {
            Some(ch) => {
                self.position.update(self.last_char);
                self.last_char = *ch;
                self.current_idx += 1;
                true
            }
            None => false,
        }
    }

    pub fn last_char(&self) -> char {
        self.last_char
    }

    pub fn position(&self) -> &Position {
        &self.position
    }

    pub fn is_eof(&self) -> bool {
        if self.current_idx < self.data.len() {
            false
        }
        else {
            true
        }
    }
}