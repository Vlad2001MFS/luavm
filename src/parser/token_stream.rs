use crate::{
    lexer::TokenInfo,
};

pub struct TokenStream {
    data: Vec<TokenInfo>,
    current_idx: usize,
}

impl TokenStream {
    pub fn new(tokens: Vec<TokenInfo>) -> TokenStream {
        TokenStream {
            data: tokens,
            current_idx: 1,
        }
    }

    pub fn look(&self, offset: usize) -> Option<&TokenInfo> {
        self.data.get(self.current_idx + offset - 1)
    }

    pub fn next(&mut self) -> bool {
        match self.data.get(self.current_idx) {
            Some(_) => {
                self.current_idx += 1;
                true
            }
            None => {
                self.current_idx += 1;
                false
            },
        }
    }

    pub fn last_char(&self) -> &TokenInfo {
        self.look(0).unwrap()
    }

    pub fn is_eof(&self) -> bool {
        self.current_idx >= self.data.len()
    }
}