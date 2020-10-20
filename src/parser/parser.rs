/* The list of implemented the Lua grammar at the moment

$$$ - fully implemented relational to original Lua grammar

chunk               ::= block                                                                       $$$
block               ::= {stat} [retstat]                                                            $$$
stat                ::= ‘;’ |
                        varlist ‘=’ explist |
                        functioncall |
                        label |
                        break |
                        goto Name
retstat             ::= return [explist] [‘;’]                                                      $$$
varlist             ::= var {‘,’ var}
var                 ::= Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name                           $$$
namelist            ::= Name {‘,’ Name}                                                             $$$
explist             ::= exp {‘,’ exp}                                                               $$$
exp                 ::= nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |        $$$
                        prefixexp | tableconstructor | exp binop exp | unop exp                     $$$
prefixexp           ::= var | functioncall | ‘(’ exp ‘)’                                            $$$
functioncall        ::= prefixexp args | prefixexp ‘:’ Name args                                    $$$
args                ::= ‘(’ [explist] ‘)’ | tableconstructor | LiteralString                        $$$
functiondef         ::= function funcbody                                                           $$$
funcbody            ::= ‘(’ [parlist] ‘)’ block end                                                 $$$
parlist             ::= namelist [‘,’ ‘...’] | ‘...’                                                $$$
tableconstructor    ::= ‘{’ [fieldlist] ‘}’                                                         $$$
fieldlist           ::= field {fieldsep field} [fieldsep]                                           $$$
field               ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp                                    $$$
fieldsep            ::= ‘,’ | ‘;’                                                                   $$$
binop               ::= ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |                                  $$$
                        ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |                                      $$$
                        ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ |                                     $$$
                        and | or                                                                    $$$
unop                ::= ‘-’ | not | ‘#’ | ‘~’                                                       $$$

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
            Expression, FunctionBody, Suffix, CallArgs, TableField,
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
            block: parser.parse_block(None),
        }
    }

    fn parse_block(&mut self, ending_token: Option<Token>) -> Block {
        let mut result = Block {
            statements: Vec::new(),
            return_statement: None,
        };

        let mut is_found_ending_token = false;
        while !self.stream.is_eof() {
            if let Some(statement) = self.try_parse_statement() {
                result.statements.push(statement);
            }
            else if let Some(return_statement) = self.try_parse_return_statement() {
                match result.return_statement.is_none() {
                    true => result.return_statement = Some(return_statement),
                    false => self.error("Expected an end of block after first return"),
                }
            }
            else if self.stream.look_token(0) == ending_token.as_ref() {
                self.stream.next();
                is_found_ending_token = true;
                break;
            }
            else {
                self.error("Unknown syntax construction");
            }
        }

        if let Some(ending_token) = ending_token {
            if !is_found_ending_token {
                self.error(&format!("Expected the ending token '{:?}'", ending_token));
            }
        }

        result
    }

    fn try_parse_statement(&mut self) -> Option<Statement> {
        while let Some(Token::SemiColon) = self.stream.look_token(0) {
            self.stream.next();
        }

        if let Some(assignment) = self.try_parse_assignment_statement() {
            Some(assignment)
        }
        else if let Some(function_call) = self.try_parse_function_call_statement() {
            Some(function_call)
        }
        else if let Some(label) = self.try_parse_label_statement() {
            Some(label)
        }
        else if let Some(break_statement) = self.try_parse_break_statement() {
            Some(break_statement)
        }
        else if let Some(goto) = self.try_parse_goto_statement() {
            Some(goto)
        }
        else {
            None
        }
    }

    fn try_parse_assignment_statement(&mut self) -> Option<Statement> {
        match self.try_parse_variables_list() {
            Some(var_list) => {
                match self.stream.look_token(0) {
                    Some(Token::Assign) => {
                        self.stream.next();

                        match self.try_parse_expression_list() {
                            Some(expr_list) => Some(Statement::Assignment {
                                var_list,
                                expr_list,
                            }),
                            None => self.error_none("Expected an expression list"),
                        }
                    },
                    _ => self.error_none("Expected '='"),
                }
            }
            None => None,
        }
    }

    fn try_parse_variables_list(&mut self) -> Option<Vec<Expression>> {
        let saved_stream_pos = self.stream.position();
        
        let mut vars = Vec::new();
        loop {
            match self.stream.look_token(0) {
                Some(Token::Comma) => match vars.is_empty() {
                    true => self.error_bool("Expected a variable"),
                    false => self.stream.next(),
                },
                _ => match vars.is_empty() {
                    true => true,
                    false => break,
                },
            };

            match self.try_parse_suffixed_expression() {
                Some(Expression::Named(name)) => vars.push(Expression::Named(name)),
                Some(Expression::Suffixed{expr, suffixes}) if matches!(suffixes.last().unwrap(), Suffix::Index(_)) => {
                    vars.push(Expression::Suffixed {
                        expr,
                        suffixes,
                    })
                }
                _ => match vars.is_empty() {
                    true => {
                        self.stream.set_position(saved_stream_pos);
                        break;
                    },
                    false => self.error("Expected a variable"),
                },
            };
        }

        match vars.is_empty() {
            true => None,
            false => Some(vars),
        }
    }

    fn try_parse_function_call_statement(&mut self) -> Option<Statement> {
        let saved_stream_pos = self.stream.position();

        match self.try_parse_suffixed_expression() {
            Some(Expression::Suffixed{expr, suffixes}) if matches!(suffixes.last().unwrap(), Suffix::CallFree(_) | Suffix::CallMethod(_)) => {
                Some(Statement::FunctionCall(Expression::Suffixed{
                    expr,
                    suffixes
                }))
            }
            _ => {
                self.stream.set_position(saved_stream_pos);
                None
            }
        }
    }

    fn try_parse_label_statement(&mut self) -> Option<Statement> {
        match self.stream.look_token(0) {
            Some(Token::DoubleColon) => {
                self.stream.next();
                match self.stream.look_token(0).cloned() {
                    Some(Token::Identifier(name)) => {
                        self.stream.next();
                        match self.stream.look_token(0) {
                            Some(Token::DoubleColon) => self.stream.next(),
                            _ => self.error_bool("Expected '::'"),
                        };
                        Some(Statement::Label(name))
                    }
                    _ => self.error_none("Expected a label name"),
                }
            }
            _ => None,
        }
    }

    fn try_parse_break_statement(&mut self) -> Option<Statement> {
        match self.stream.look_token(0) {
            Some(Token::Break) => {
                self.stream.next();
                Some(Statement::Break)
            }
            _ => None,
        }
    }

    fn try_parse_goto_statement(&mut self) -> Option<Statement> {
        match self.stream.look_token(0) {
            Some(Token::Goto) => {
                self.stream.next();
                match self.stream.look_token(0).cloned() {
                    Some(Token::Identifier(name)) => {
                        self.stream.next();
                        Some(Statement::Goto(name))
                    }
                    _ => self.error_none("Expected a target label name"),
                }
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
            Some(Token::Dots3) => {
                self.stream.next();
                Some(Expression::VarArg)
            }
            Some(Token::Function) => {
                self.stream.next();
                match self.try_parse_function_body() {
                    Some(body_expr) => {
                        Some(Expression::FunctionDef(body_expr))
                    }
                    None => self.error_none("Expected a function body")
                }
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
            Some(Token::LeftBrace) => {
                match self.try_parse_table_constructor() {
                    Some(table_constructor) => Some(table_constructor),
                    None => self.error_none("Expected a table constructor"),
                }
            }
            _ => self.try_parse_suffixed_expression(),
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

    fn try_parse_suffixed_expression(&mut self) -> Option<Expression> {
        let main_expr = match self.stream.look_token(0).cloned() {
            Some(Token::Identifier(name)) => {
                self.stream.next();
                Some(Expression::Named(name))
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

        match main_expr {
            Some(main_expr) => {
                match self.try_parse_suffixes() {
                    Some(suffixes) => {
                        Some(Expression::Suffixed {
                            expr: Box::new(main_expr),
                            suffixes,
                        })
                    },
                    None => Some(main_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_suffixes(&mut self) -> Option<Vec<Suffix>> {
        let mut suffixes = Vec::new();
        loop {
            match self.stream.look_token(0).cloned() {
                Some(Token::Dot) => {
                    self.stream.next();
                    match self.stream.look_token(0).cloned() {
                        Some(Token::Identifier(name)) => {
                            self.stream.next();
                            suffixes.push(Suffix::Index(Box::new(Expression::Named(name))))
                        }
                        _ => self.error("Expected a suffixed expression"),
                    };
                }
                Some(Token::LeftBracket) => {
                    self.stream.next();
                    match self.try_parse_expression() {
                        Some(expr) => {
                            match self.stream.look_token(0) {
                                Some(Token::RightBracket) => self.stream.next(),
                                _ => self.error_bool("Expected ']'"),
                            };

                            suffixes.push(Suffix::Index(Box::new(expr)));
                        },
                        None => self.error("Expected a suffixed expression"),
                    }
                }
                _ => {
                    if let Some(args) = self.try_parse_call_arguments() {
                        suffixes.push(Suffix::CallFree(args))
                    }
                    else if let Some(Token::Colon) = self.stream.look_token(0) {
                        self.stream.next();
                        match self.stream.look_token(0) {
                            Some(Token::Identifier(_)) => {
                                self.stream.next();
                                match self.try_parse_call_arguments() {
                                    Some(args) => suffixes.push(Suffix::CallMethod(args)),
                                    None => self.error("Expected arguments or ()"),
                                }
                            },
                            _ => self.error("Expected an identifier"),
                        };
                    }
                    else {
                        break;
                    }
                },
            }
        }

        match suffixes.is_empty() {
            true => None,
            false => Some(suffixes),
        }
    }

    fn try_parse_call_arguments(&mut self) -> Option<CallArgs> {
        match self.stream.look_token(0) {
            Some(Token::LeftParen) => {
                self.stream.next();

                let expr_list = self.try_parse_expression_list();

                match self.stream.look_token(0) {
                    Some(Token::RightParen) => self.stream.next(),
                    _ => self.error_bool("Expected ')'"),
                };

                match expr_list {
                    Some(expr_list) => Some(CallArgs::ExpressionList(expr_list)),
                    None => Some(CallArgs::ExpressionList(Vec::new())),
                }
            }
            Some(Token::LeftBrace) => {
                match self.try_parse_table_constructor() {
                    Some(table_constructor) => Some(CallArgs::Table(Box::new(table_constructor))),
                    None => self.error_none("Expected a table constructor"),
                }
            }
            Some(Token::String(string)) => {
                Some(CallArgs::String(string.clone()))
            }
            _ => None,
        }
    }

    fn try_parse_table_constructor(&mut self) -> Option<Expression> {
        match self.stream.look_token(0) {
            Some(Token::LeftBrace) => {
                self.stream.next();

                let mut fields = Vec::new();
                loop {
                    if let Some(Token::LeftBracket) = self.stream.look_token(0) {
                        self.stream.next();

                        match self.try_parse_expression() {
                            Some(key_expr) => {
                                match self.stream.look_token(0) {
                                    Some(Token::RightBracket) => {
                                        self.stream.next();

                                        match self.stream.look_token(0) {
                                            Some(Token::Assign) => {
                                                self.stream.next();
                                                
                                                match self.try_parse_expression() {
                                                    Some(value_expr) => fields.push(TableField {
                                                        key: Some(key_expr),
                                                        value: value_expr,
                                                    }),
                                                    None => self.error("Expected an expression"),
                                                }
                                            },
                                            _ => self.error("Expected '='"),
                                        };
                                    },
                                    _ => self.error("Expected ']'"),
                                };
                            },
                            None => self.error("Expected an expression"),
                        }
                    }
                    else if let Some(Token::Identifier(name)) = self.stream.look_token(0).cloned() {
                        self.stream.next();

                        match self.stream.look_token(0) {
                            Some(Token::Assign) => {
                                self.stream.next();
                                
                                match self.try_parse_expression() {
                                    Some(value_expr) => fields.push(TableField {
                                        key: Some(Expression::Named(name)),
                                        value: value_expr,
                                    }),
                                    None => self.error("Expected an expression"),
                                }
                            },
                            _ => self.error("Expected '='"),
                        };
                    }
                    else if let Some(expr) = self.try_parse_expression() {
                        fields.push(TableField {
                            key: None,
                            value: expr,
                        });
                    }
                    else if let Some(Token::RightBrace) = self.stream.look_token(0) {
                        self.stream.next();
                        break;
                    }
                    else {
                        self.error("Unexpected token");
                    }

                    if !fields.is_empty() {
                        if self.stream.look_token(0).map_or(false, |a| a == &Token::Comma || a == &Token::SemiColon) {
                            self.stream.next();
                        }
                        else if self.stream.look_token(0).map_or(false, |a| a != &Token::RightBrace) {
                            self.error("Expected a separator ',' or ';'");
                        }
                    }
                }

                Some(Expression::Table(fields))
            }
            _ => None,
        }
    }

    fn try_parse_function_body(&mut self) -> Option<FunctionBody> {
        match self.stream.look_token(0) {
            Some(Token::LeftParen) => {
                self.stream.next();

                let param_list = self.try_parse_name_list(true);

                match self.stream.look_token(0) {
                    Some(Token::RightParen) => self.stream.next(),
                    _ => self.error_bool("Expected ')'"),
                };

                let block = self.parse_block(Some(Token::End));

                match param_list {
                    Some(param_list) => Some(FunctionBody {
                        param_list: param_list.0,
                        param_list_has_vararg: param_list.1,
                        block,
                    }),
                    None => Some(FunctionBody {
                        param_list: Vec::new(),
                        param_list_has_vararg: false,
                        block,
                    })
                }
            }
            _ => None,
        }
    }

    fn try_parse_name_list(&mut self, can_has_vararg: bool) -> Option<(Vec<String>, bool)> {
        match self.stream.look_token(0).cloned() {
            Some(Token::Identifier(name)) => {
                self.stream.next();

                let mut names = vec![name];
                let mut found_vararg = false;

                while self.stream.look_token(0) == Some(&Token::Comma) {
                    self.stream.next();

                    match self.stream.look_token(0).cloned() {
                        Some(Token::Identifier(name)) => {
                            self.stream.next();
                            names.push(name.clone());
                        }
                        Some(Token::Dots3) => {
                            match can_has_vararg {
                                true => {
                                    self.stream.next();
                                    found_vararg = true;
                                    break;
                                },
                                false => self.error("Variadic arguments is not allowed in this context"),
                            }
                        }
                        _ => match can_has_vararg {
                            true => self.error("Expected a name or a vararg"),
                            false => self.error("Expected a name"),
                        },
                    }
                }

                Some((names, found_vararg))
            }
            Some(Token::Dots3) if can_has_vararg => {
                self.stream.next();
                Some((Vec::new(), true))
            }
            _ => None,
        }
    }

    #[track_caller]
    fn error_none<T>(&self, desc: &str) -> Option<T> {
        match self.stream.last_token_info() {
            Some(token_info) => {
                let token_begin_loc = token_info.begin_location();
                let token_end_loc = token_info.end_location();
                let pointer_len = match token_end_loc.column() > token_begin_loc.column() {
                    true => token_end_loc.column() - token_begin_loc.column(),
                    false => 1,
                };
                let pointer = " ".repeat(token_begin_loc.column() - 1) + &"^".repeat(pointer_len);
                panic!(format!("{}:{}: Parser error: {}\n{}\n{}", token_begin_loc.source_name(), token_begin_loc, desc, token_begin_loc.content(), pointer))
            }
            None => {
                panic!(format!("Parser error: {}", desc))
            }
        }
    }

    #[track_caller]
    fn error_bool(&self, desc: &str) -> bool {
        self.error_none::<()>(desc);
        false
    }

    
    #[track_caller]
    fn error(&self, desc: &str) {
        self.error_bool(desc);
    }
}