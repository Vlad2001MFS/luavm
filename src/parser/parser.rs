use crate::{
    lexer::{
        TokenInfo, Token,
    },
    parser::{
        TokenStream,
        ast::{
            Chunk, Block,
            Statement, ReturnStatement,
            Expression,
        },
    },
};

pub struct Parser {
    stream: TokenStream,
}

impl Parser {
    pub fn parse(tokens: Vec<TokenInfo>) -> Chunk {
        let mut parser = Parser {
            stream: TokenStream::new(tokens),
        };

        Chunk {
            block: parser.parse_block()
        }
    }

    fn parse_block(&mut self) -> Block {
        let mut result = Block {
            statements: Vec::new(),
            return_statement: None,
        };

        while !self.stream.is_eof() {
            if let Some(statement) = self.try_parse_statement() {
                result.statements.push(statement);
            }
            else if let Some(return_statement) = self.try_parse_return_statement() {
                result.return_statement = Some(return_statement);
                break;
            }
            else {
                self.error("Unknown syntax construction");
            }
        }

        result
    }

    fn try_parse_statement(&mut self) -> Option<Statement> {
        if let Token::SemiColon = self.stream.last_token() {
            while let Token::SemiColon = self.stream.last_token() {
                self.stream.next();
            }
        }
        None
    }

    fn try_parse_return_statement(&mut self) -> Option<ReturnStatement> {
        if let Token::Return = self.stream.last_token() {
            self.stream.next();

            let mut result = ReturnStatement {
                expression_list: Vec::new(),
            };

            if let Some(expression_list) = self.try_parse_expression_list() {
                result.expression_list = expression_list;
            }
            
            if let Token::SemiColon = self.stream.last_token() {
                self.stream.next();
            }

            return Some(result);
        }
        None
    }

    fn try_parse_expression_list(&mut self) -> Option<Vec<Expression>> {
        if let Some(expr) = self.try_parse_expression() {
            let mut result = vec![expr];

            while let Token::Comma = self.stream.last_token() {
                if let Some(expr) = self.try_parse_expression() {
                    result.push(expr);
                }
                else {
                    self.error("Expected an expression");
                }
            }

            return Some(result);
        }
        None
    }

    fn try_parse_expression(&mut self) -> Option<Expression> {
        if let Token::Number(number) = self.stream.last_token() {
            return Some(Expression::Number(*number));
        }
        None
    }

    fn error(&self, desc: &str) {
        let token_info = self.stream.last_token_info();
        let token_begin_loc = token_info.begin_location();
        let token_end_loc = token_info.end_location();
        let pointer = " ".repeat(token_begin_loc.column() - 1) + &"^".repeat(token_end_loc.column() - token_begin_loc.column());
        panic!(format!("{}:{}: Parser error: {}\n{}\n{}", token_begin_loc.source_name(), token_begin_loc, desc, token_begin_loc.content(), pointer))
    }
}