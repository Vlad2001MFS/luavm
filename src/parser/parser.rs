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
                match result.return_statement {
                    None => result.return_statement = Some(return_statement),
                    Some(_) => self.error("Expected an end of block after first return"),
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
                self.error(&format!("Expected the ending token '{}'", ending_token));
            }
        }

        result
    }

    fn try_parse_statement(&mut self) -> Option<Statement> {
        while self.eat(Token::SemiColon) {}

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
                self.expect(Token::Assign);
                
                match self.try_parse_expression_list() {
                    Some(expr_list) => Some(Statement::Assignment {
                        var_list,
                        expr_list,
                    }),
                    None => self.error_none("Expected an expression list"),
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
                Some(Token::Comma) => {
                    match vars.is_empty() {
                        true => self.error_bool("Expected a variable"),
                        false => self.stream.next()
                    }
                },
                _ => {
                    match vars.is_empty() {
                        true => true,
                        false => break,
                    }
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
                _ => {
                    match vars.is_empty() {
                        true => {
                            self.stream.set_position(saved_stream_pos);
                            break;
                        },
                        false => self.error("Expected a variable"),
                    }
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
            Some(Expression::Suffixed{expr, suffixes}) if matches!(suffixes.last().unwrap(), Suffix::CallFree(_) | Suffix::CallMethod{name: _, args: _}) => {
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
        match self.eat(Token::DoubleColon) {
            true => {
                let name = self.expect_name();
                self.expect(Token::DoubleColon);
                Some(Statement::Label(name))
            }
            false => None,
        }
    }

    fn try_parse_break_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Break) {
            true => Some(Statement::Break),
            false => None,
        }
    }

    fn try_parse_goto_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Goto) {
            true => Some(Statement::Goto(self.expect_name())),
            false => None,
        }
    }

    fn try_parse_return_statement(&mut self) -> Option<ReturnStatement> {
        match self.eat(Token::Return) {
            true => {
                let expr_list = self.try_parse_expression_list();
                self.eat(Token::SemiColon);

                Some(ReturnStatement {
                    expression_list: expr_list.unwrap_or_else(|| Vec::new()),
                })
            }
            false => None,
        }
    }

    fn try_parse_expression_list(&mut self) -> Option<Vec<Expression>> {
        match self.try_parse_expression() {
            Some(expr) => {
                let mut result = vec![expr];

                while self.eat(Token::Comma) {
                    match self.try_parse_expression() {
                        Some(expr) => result.push(expr),
                        None => self.error("Expected an expression"),
                    };
                }

                Some(result)
            }
            _ => None,
        }
    }

    fn try_parse_expression(&mut self) -> Option<Expression> {
        match self.try_parse_expression_and() {
            Some(left_expr) => {
                match self.eat(Token::Or) {
                    true => {
                        match self.try_parse_expression() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: Token::Or,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
                        }
                    },
                    false => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_and(&mut self) -> Option<Expression> {
        match self.try_parse_expression_cmp() {
            Some(left_expr) => {
                match self.eat(Token::And) {
                    true => {
                        match self.try_parse_expression_and() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: Token::And,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
                        }
                    }
                    false => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_cmp(&mut self) -> Option<Expression> {
        match self.try_parse_expression_bit_or() {
            Some(left_expr) => {
                match self.eat_any_of(&[Token::LessThan, Token::GreaterThan, Token::LessEqual, Token::GreaterEqual, Token::NotEqual, Token::Equal]) {
                    Some(token) => {
                        match self.try_parse_expression_cmp() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
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
                match self.eat(Token::BitOr) {
                    true => {
                        match self.try_parse_expression_bit_or() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: Token::BitOr,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
                        }
                    }
                    false => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_bit_xor(&mut self) -> Option<Expression> {
        match self.try_parse_expression_bit_and() {
            Some(left_expr) => {
                match self.eat(Token::BitNotXor) {
                    true => {
                        match self.try_parse_expression_bit_xor() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: Token::BitNotXor,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
                        }
                    }
                    false => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_bit_and(&mut self) -> Option<Expression> {
        match self.try_parse_expression_shift() {
            Some(left_expr) => {
                match self.eat(Token::BitAnd) {
                    true => {
                        match self.try_parse_expression_bit_and() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: Token::BitAnd,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
                        }
                    }
                    false => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_shift(&mut self) -> Option<Expression> {
        match self.try_parse_expression_concat() {
            Some(left_expr) => {
                match self.eat_any_of(&[Token::ShiftLeft, Token::ShiftRight]) {
                    Some(token) => {
                        match self.try_parse_expression_shift() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
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
                match self.eat(Token::Dots2) {
                    true => {
                        match self.try_parse_expression_concat() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: Token::Dots2,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected an expression"),
                        }
                    }
                    false => Some(left_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_expression_arithm(&mut self) -> Option<Expression> {
        match self.try_parse_expression_arithm_term() {
            Some(left_expr) => {
                match self.eat_any_of(&[Token::Add, Token::Sub]) {
                    Some(token) => {
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
                match self.eat_any_of(&[Token::Mul, Token::Div, Token::IDiv, Token::Mod]) {
                    Some(token) => {
                        match self.try_parse_expression_arithm_term() {
                            Some(right_expr) => Some(Expression::BinaryOp {
                                op: token,
                                left_expr: Box::new(left_expr),
                                right_expr: Box::new(right_expr),
                            }),
                            None => self.error_none("Expected a right term"),
                        }
                    }
                    None => Some(left_expr),
                }
            }
            _ => None,
        }
    }

    fn try_parse_expression_arithm_factor(&mut self) -> Option<Expression> {
        let factor = if self.eat(Token::Nil) {
            Some(Expression::Nil)
        }
        else if self.eat(Token::True) {
            Some(Expression::Bool(true))
        }
        else if self.eat(Token::False) {
            Some(Expression::Bool(false))
        }
        else if let Some(number) = self.eat_number() {
            Some(Expression::Number(number))
        }
        else if let Some(string) = self.eat_string() {
            Some(Expression::String(string.clone()))
        }
        else if self.eat(Token::Dots3) {
            Some(Expression::VarArg)
        }
        else if self.eat(Token::Function) {
            match self.try_parse_function_body() {
                Some(body_expr) => Some(Expression::FunctionDef(body_expr)),
                None => self.error_none("Expected a function body"),
            }
        }
        else if let Some(token) = self.eat_any_of(&[Token::Not, Token::Len, Token::Sub, Token::BitNotXor]) {
            match self.try_parse_expression_arithm_factor() {
                Some(expr) => Some(Expression::UnaryOp {
                    op: token,
                    expr: Box::new(expr),
                }),
                None => self.error_none("Expected a factor"),
            }
        }
        else if self.look_for(Token::LeftBrace) {
            match self.try_parse_table_constructor() {
                Some(table_constructor) => Some(table_constructor),
                None => self.error_none("Expected a table constructor"),
            }
        }
        else {
            self.try_parse_suffixed_expression()
        };

        match factor {
            Some(factor) => {
                match self.eat(Token::Pow) {
                    true => {
                        match self.try_parse_expression() {
                            Some(pow) => Some(Expression::BinaryOp {
                                op: Token::Pow,
                                left_expr: Box::new(factor),
                                right_expr: Box::new(pow),
                            }),
                            None => self.error_none("Expected an expression")
                        }
                    }
                    false => Some(factor),
                }
            }
            None => None,
        }
    }

    fn try_parse_suffixed_expression(&mut self) -> Option<Expression> {
        let main_expr = if let Some(name) = self.eat_name() {
            Some(Expression::Named(name))
        }
        else if self.eat(Token::LeftParen) {
            match self.try_parse_expression() {
                Some(expr) => {
                    self.expect(Token::RightParen);
                    Some(expr)
                }
                None => self.error_none("Expected an expression"),
            }
        }
        else {
            None
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
            if self.eat(Token::Dot) {
                let name = self.expect_name();
                suffixes.push(Suffix::Index(Box::new(Expression::Named(name))));
            }
            else if self.eat(Token::LeftBracket) {
                match self.try_parse_expression() {
                    Some(expr) => {
                        self.expect(Token::RightBracket);
                        suffixes.push(Suffix::Index(Box::new(expr)));
                    },
                    None => self.error("Expected a suffixed expression"),
                }
            }
            else if let Some(args) = self.try_parse_call_arguments() {
                suffixes.push(Suffix::CallFree(args))
            }
            else if self.eat(Token::Colon) {
                let name = self.expect_name();
                match self.try_parse_call_arguments() {
                    Some(args) => suffixes.push(Suffix::CallMethod {
                        name,
                        args,
                    }),
                    None => self.error("Expected arguments or ()"),
                }
            }
            else {
                break;
            }
        }

        match suffixes.is_empty() {
            true => None,
            false => Some(suffixes),
        }
    }

    fn try_parse_call_arguments(&mut self) -> Option<CallArgs> {
        if self.eat(Token::LeftParen) {
            let expr_list = self.try_parse_expression_list();
            self.expect(Token::RightParen);

            match expr_list {
                Some(expr_list) => Some(CallArgs::ExpressionList(expr_list)),
                None => Some(CallArgs::ExpressionList(Vec::new())),
            }
        }
        else if self.look_for(Token::LeftBrace) {
            match self.try_parse_table_constructor() {
                Some(table_constructor) => Some(CallArgs::Table(Box::new(table_constructor))),
                None => self.error_none("Expected a table constructor"),
            }
        }
        else if let Some(string) = self.eat_string() {
            Some(CallArgs::String(string.clone()))
        }
        else {
            None
        }
    }

    fn try_parse_table_constructor(&mut self) -> Option<Expression> {
        match self.eat(Token::LeftBrace) {
            true => {
                let mut fields = Vec::new();
                loop {
                    if self.eat(Token::LeftBracket) {
                        match self.try_parse_expression() {
                            Some(key_expr) => {
                                self.expect(Token::RightBracket);
                                self.expect(Token::Assign);

                                match self.try_parse_expression() {
                                    Some(value_expr) => fields.push(TableField {
                                        key: Some(key_expr),
                                        value: value_expr,
                                    }),
                                    None => self.error("Expected an expression"),
                                }
                            },
                            None => self.error("Expected an expression"),
                        }
                    }
                    else if let Some(name) = self.eat_name() {
                        self.expect(Token::Assign);

                        match self.try_parse_expression() {
                            Some(value_expr) => fields.push(TableField {
                                key: Some(Expression::Named(name)),
                                value: value_expr,
                            }),
                            None => self.error("Expected an expression"),
                        }
                    }
                    else if let Some(expr) = self.try_parse_expression() {
                        fields.push(TableField {
                            key: None,
                            value: expr,
                        });
                    }
                    else if self.eat(Token::RightBrace) {
                        break;
                    }
                    else {
                        self.error("Unexpected token");
                    }

                    if !fields.is_empty() && self.eat_any_of(&[Token::Comma, Token::SemiColon]).is_none() && !self.look_for(Token::RightBrace) {
                        self.error("Expected a separator ',' or ';'");
                    }
                }

                Some(Expression::Table(fields))
            }
            false => None,
        }
    }

    fn try_parse_function_body(&mut self) -> Option<FunctionBody> {
        match self.eat(Token::LeftParen) {
            true => {
                let param_list = self.try_parse_name_list(true);
                self.expect(Token::RightParen);

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
            false => None,
        }
    }

    fn try_parse_name_list(&mut self, can_has_vararg: bool) -> Option<(Vec<String>, bool)> {
        if let Some(name) = self.eat_name() {
            let mut names = vec![name];
            let mut found_vararg = false;

            while self.eat(Token::Comma) {
                if let Some(name) = self.eat_name() {
                    names.push(name.clone());
                }
                else if self.eat(Token::Dots3) {
                    match can_has_vararg {
                        true => {
                            found_vararg = true;
                            break;
                        },
                        false => self.error("Variadic arguments is not allowed in this context"),
                    }
                }
                else {
                    match can_has_vararg {
                        true => self.error("Expected a name or a vararg"),
                        false => self.error("Expected a name"),
                    }
                }
            }

            Some((names, found_vararg))
        }
        else if can_has_vararg && self.eat(Token::Dots3) {
            Some((Vec::new(), true))
        }
        else {
            None
        }
    }

    #[track_caller]
    fn expect(&mut self, expected_token: Token) {
        match self.stream.look_token(0) {
            Some(token) if token == &expected_token => self.stream.next(),
            Some(token) => self.error_bool(&format!("Expected '{}' instead '{}'", expected_token, token)),
            _ => self.error_bool("Unexpected end of token stream"),
        };
    }

    #[track_caller]
    fn expect_name(&mut self) -> String {
        match self.stream.look_token(0).cloned() {
            Some(Token::Identifier(name)) => {
                self.stream.next();
                name
            },
            Some(_) => {
                self.error("Expected an identifier");
                unreachable!();
            },
            _ => {
                self.error("Unexpected end of token stream");
                unreachable!();
            },
        }
    }

    #[track_caller]
    fn eat(&mut self, expected_token: Token) -> bool {
        match self.look_for(expected_token) {
            true => {
                self.stream.next();
                true
            },
            _ => false,
        }
    }

    #[track_caller]
    fn eat_any_of(&mut self, expected_tokens: &[Token]) -> Option<Token> {
        for token in expected_tokens.iter() {
            if self.eat(token.clone()) {
                return Some(token.clone());
            }
        }
        None
    }

    #[track_caller]
    fn eat_number(&mut self) -> Option<f64> {
        match self.stream.look_token(0).cloned() {
            Some(Token::Number(number)) => {
                self.stream.next();
                Some(number)
            },
            _ => None,
        }
    }

    #[track_caller]
    fn eat_string(&mut self) -> Option<String> {
        match self.stream.look_token(0).cloned() {
            Some(Token::String(string)) => {
                self.stream.next();
                Some(string)
            },
            _ => None,
        }
    }

    #[track_caller]
    fn eat_name(&mut self) -> Option<String> {
        match self.stream.look_token(0).cloned() {
            Some(Token::Identifier(name)) => {
                self.stream.next();
                Some(name)
            },
            _ => None,
        }
    }

    #[track_caller]
    fn look_for(&mut self, expected_token: Token) -> bool {
        self.stream.look_token(0) == Some(&expected_token)
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