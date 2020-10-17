/* The list of implemented the Lua grammar at the moment

chunk ::= block
block ::= {stat} [retstat]
stat ::=  ‘;’
retstat ::= return [explist] [‘;’]
explist ::= exp {‘,’ exp}
exp ::=  nil | false | true | Numeral | LiteralString |
         prefixexp | exp binop exp | unop exp
prefixexp ::= ‘(’ exp ‘)’
binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |
           ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
           ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
           and | or
unop ::= ‘-’ | not | ‘#’ | ‘~’

*/

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
        match self.stream.look_token(0) {
            Some(Token::SemiColon) => {
                while let Some(Token::SemiColon) = self.stream.look_token(0) {
                    self.stream.next();
                }
                None
            }
            _ => None,
        }
    }

    fn try_parse_return_statement(&mut self) -> Option<ReturnStatement> {
        match self.stream.look_token(0) {
            Some(Token::Return) => {
                self.stream.next();

                let mut result = ReturnStatement {
                    expression_list: Vec::new(),
                };

                if let Some(expression_list) = self.try_parse_expression_list() {
                    result.expression_list = expression_list;
                }
                
                if let Some(Token::SemiColon) = self.stream.look_token(0) {
                    self.stream.next();
                }

                Some(result)
            }
            _ => None,
        }
    }

    fn try_parse_expression_list(&mut self) -> Option<Vec<Expression>> {
        match self.try_parse_expression() {
            Some(expr) => {
                let mut result = vec![expr];

                while let Some(Token::Comma) = self.stream.look_token(0) {
                    self.stream.next();

                    if let Some(expr) = self.try_parse_expression() {
                        result.push(expr);
                    }
                    else {
                        self.error("Expected an expression");
                    }
                }

                Some(result)
            }
            _ => None,
        }
    }

    /*
       or
       and
       <     >     <=    >=    ~=    ==
    */

    fn try_parse_expression(&mut self) -> Option<Expression> {
        match self.try_parse_expression_and() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(Token::Or) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression() {
                            Some(Expression::BinaryOp {
                                op: Token::Or,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_and(&mut self) -> Option<Expression> {
        match self.try_parse_expression_cmp() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(Token::And) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_and() {
                            Some(Expression::BinaryOp {
                                op: Token::And,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_cmp(&mut self) -> Option<Expression> {
        match self.try_parse_expression_bit_or() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(token) if [Token::LessThan, Token::GreaterThan, Token::LessEqual, Token::GreaterEqual, Token::NotEqual, Token::Equal].contains(&token) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_cmp() {
                            Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_bit_or(&mut self) -> Option<Expression> {
        match self.try_parse_expression_bit_xor() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(Token::BitOr) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_bit_or() {
                            Some(Expression::BinaryOp {
                                op: Token::BitOr,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_bit_xor(&mut self) -> Option<Expression> {
        match self.try_parse_expression_bit_and() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(Token::BitNotXor) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_bit_xor() {
                            Some(Expression::BinaryOp {
                                op: Token::BitNotXor,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_bit_and(&mut self) -> Option<Expression> {
        match self.try_parse_expression_shift() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(Token::BitAnd) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_bit_and() {
                            Some(Expression::BinaryOp {
                                op: Token::BitAnd,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_shift(&mut self) -> Option<Expression> {
        match self.try_parse_expression_concat() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(token) if [Token::ShiftLeft, Token::ShiftRight].contains(&token) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_shift() {
                            Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_concat(&mut self) -> Option<Expression> {
        match self.try_parse_expression_arithm() {
            Some(left_expr) => {
                match self.stream.look_token(0) {
                    Some(Token::Dots2) => {
                        self.stream.next();
                        if let Some(right_expr) = self.try_parse_expression_concat() {
                            Some(Expression::BinaryOp {
                                op: Token::Dots2,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            })
                        }
                        else {
                            self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_arithm(&mut self) -> Option<Expression> {
        match self.try_parse_expression_arithm_term() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(token) if [Token::Add, Token::Sub].contains(&token) => {
                        self.stream.next();
                        match self.try_parse_expression_arithm() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected a right expression"),
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            _ => None,
        }
    }

    fn try_parse_expression_arithm_term(&mut self) -> Option<Expression> {
        match self.try_parse_expression_arithm_factor() {
            Some(left_expr) => {
                match self.stream.look_token(0).cloned() {
                    Some(token) if [Token::Mul, Token::Div, Token::IDiv, Token::Mod].contains(&token) => {
                        self.stream.next();
                        match self.try_parse_expression_arithm_term() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected a right term"),
                        }
                    }
                    _ => Some(left_expr),
                }
            }
            _ => None,
        }
    }

    
    fn try_parse_expression_arithm_factor(&mut self) -> Option<Expression> {
        let factor = match self.stream.look_token(0).cloned() {
            Some(Token::Nil) => {
                self.stream.next();
                Some(Expression::Nil)
            }
            Some(Token::True) => {
                self.stream.next();
                Some(Expression::Bool(true))
            }
            Some(Token::False) => {
                self.stream.next();
                Some(Expression::Bool(false))
            }
            Some(Token::Number(number)) => {
                self.stream.next();
                Some(Expression::Number(number))
            }
            Some(Token::String(string)) => {
                self.stream.next();
                Some(Expression::String(string.clone()))
            }
            Some(token) if [Token::Not, Token::Len, Token::Sub, Token::BitNotXor].contains(&token) => {
                self.stream.next();
                match self.try_parse_expression_arithm_factor() {
                    Some(expr) => Some(Expression::UnaryOp {
                        op: token,
                        expr: Box::new(expr),
                    }),
                    None => self.error_none("Expected a factor"),
                }
            }
            Some(Token::LeftParen) => {
                self.stream.next();
                if let Some(expr) = self.try_parse_expression() {
                    if let Some(Token::RightParen) = self.stream.look_token(0) {
                        self.stream.next();
                        Some(expr)
                    }
                    else {
                        self.error_none("Expected ')'")
                    }
                }
                else {
                    self.error_none("Expected an expression")
                }
            }
            _ => None,
        };

        match factor {
            Some(factor) => {
                match self.stream.look_token(0) {
                    Some(Token::Pow) => {
                        self.stream.next();
                        match self.try_parse_expression() {
                            Some(pow) => {
                                Some(Expression::BinaryOp {
                                    op: Token::Pow,
                                    left_expr: Box::new(factor),
                                    right_expr: Box::new(pow),
                                })
                            }
                            None => self.error_none("Expected an expression")
                        }
                    }
                    _ => Some(factor),
                }
            }
            None => None,
        }
    }

    #[track_caller]
    fn error_none<T>(&self, desc: &str) -> Option<T> {
        match self.stream.look_token_info(0) {
            Some(token_info) => {
                let token_begin_loc = token_info.begin_location();
                let token_end_loc = token_info.end_location();
                let pointer = " ".repeat(token_begin_loc.column() - 1) + &"^".repeat(token_end_loc.column() - token_begin_loc.column());
                panic!(format!("{}:{}: Parser error: {}\n{}\n{}", token_begin_loc.source_name(), token_begin_loc, desc, token_begin_loc.content(), pointer))
            }
            None => panic!(format!("Parser error: {}", desc))
        }
    }

    #[track_caller]
    fn error(&self, desc: &str) {
        self.error_none::<()>(desc);
    }
}