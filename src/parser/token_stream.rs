use crate::{
    lexer::{
        TokenInfo, Token,
    },
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

    pub fn look_token(&self, offset: usize) -> Option<&Token> {
        self.look_token_info(offset).map(|info| info.token())
    }

    pub fn look_token_info(&self, offset: usize) -> Option<&TokenInfo> {
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

    pub fn set_position(&mut self, pos: usize) {
        self.current_idx = pos;
    }

    pub fn position(&self) -> usize {
        self.current_idx
    }

    pub fn is_eof(&self) -> bool {
        self.look_token_info(0).is_none()
    }

    pub fn last_token_info(&self) -> Option<&TokenInfo> {
        self.look_token_info(0).or(self.data.last())
    }
}