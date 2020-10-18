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

    pub fn is_eof(&self) -> bool {
        self.current_idx - 1 >= self.data.len()
    }

    pub fn last_token_info(&self) -> Option<&TokenInfo> {
        self.data.last()
    }
}