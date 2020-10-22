/* The list of implemented the Lua grammar at the moment

$$$ - fully implemented relational to original Lua grammar

chunk               ::= block                                                                       $$$
block               ::= {stat} [retstat]                                                            $$$
stat                ::= ‘;’ |
                        varlist ‘=’ explist |
                        functioncall |
                        label |
                        break |
                        goto Name |
                        do block end |
                        while exp do block end |
                        repeat block until exp |
                        if exp then block {elseif exp then block} [else block] end |
                        for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
                        for namelist in explist do block end |
                        function funcname funcbody |
                        local function Name funcbody
retstat             ::= return [explist] [‘;’]                                                      $$$
label               ::= ‘::’ Name ‘::’                                                              $$$
funcname            ::= Name {‘.’ Name} [‘:’ Name]                                                  $$$
varlist             ::= var {‘,’ var}                                                               $$$
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
            ConditionalBlock,
        },
    },
};

macro_rules! try_parse_expression {
    ($low_prior_expr:ident, $expr_tokens:expr, $high_prior_expr:ident) => {
        fn $low_prior_expr(&mut self) -> Option<Expression> {
            match self.$high_prior_expr() {
                Some(left_expr) => {
                    match self.eat_any_of(&$expr_tokens) {
                        Some(token) => {
                            match self.$low_prior_expr() {
                                Some(right_expr) => Some(Expression::BinaryOp {
                                    op: token,
                                    left_expr: Box::new(left_expr),
                                    right_expr: Box::new(right_expr),
                                }),
                                None => self.error_none("Expected an expression"),
                            }
                        },
                        _ => Some(left_expr),
                    }
                }
                None => None,
            }
        }
    };
}

pub struct Parser {
    stream: TokenStream,
}

impl Parser {
    pub fn parse(tokens: Vec<TokenInfo>) -> Chunk {
        let mut parser = Parser {
            stream: TokenStream::new(tokens),
        };

        Chunk {
            block: parser.parse_block(None).0,
        }
    }

    fn parse_block(&mut self, ending_token: Option<&[Token]>) -> (Block, Option<Token>) {
        let mut result = Block {
            statements: Vec::new(),
            return_statement: None,
        };

        let mut found_ending_token = None;
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
            else if let Some(Some(ending_token)) = ending_token.map(|tokens| self.eat_any_of(tokens)) {
                found_ending_token = Some(ending_token);
                break;
            }
            else {
                self.error("Unknown syntax construction");
            }
        }

        if let Some(_) = ending_token {
            if found_ending_token.is_none() {
                self.error(&format!("Expected the block ending token"));
            }
        }

        (result, found_ending_token)
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
        else if let Some(break_) = self.try_parse_break_statement() {
            Some(break_)
        }
        else if let Some(goto) = self.try_parse_goto_statement() {
            Some(goto)
        }
        else if let Some(block) = self.try_parse_block_statement() {
            Some(block)
        }
        else if let Some(while_) = self.try_parse_while_statement() {
            Some(while_)
        }
        else if let Some(repeat_until) = self.try_parse_repeat_until_statement() {
            Some(repeat_until)
        }
        else if let Some(if_else) = self.try_parse_if_else_statement() {
            Some(if_else)
        }
        else if let Some(for_) = self.try_parse_for_statement() {
            Some(for_)
        }
        else if let Some(function_definition) = self.try_parse_function_definition_statement() {
            Some(function_definition)
        }
        else if let Some(local_function_definition) = self.try_parse_local_function_definition_statement() {
            Some(local_function_definition)
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

    fn try_parse_block_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Do) {
            true => Some(Statement::Block(self.parse_block(Some(&[Token::End])).0)),
            false => None,
        }
    }

    fn try_parse_while_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::While) {
            true => {
                match self.try_parse_expression() {
                    Some(cond) => {
                        match self.try_parse_block_statement() {
                            Some(Statement::Block(block)) => Some(Statement::While {
                                cond,
                                block,
                            }),
                            _ => self.error_none("Expected a block"),
                        }
                    }
                    None => self.error_none("Expected an expression")
                }
            }
            false => None,
        }
    }

    fn try_parse_repeat_until_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Repeat) {
            true => {
                let block = self.parse_block(Some(&[Token::Until])).0;
                match self.try_parse_expression() {
                    Some(cond) => Some(Statement::RepeatUntil {
                        cond,
                        block,
                    }),
                    None => self.error_none("Expected an expression"),
                }
            }
            false => None,
        }
    }

    fn try_parse_if_else_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::If) {
            true => {
                let if_cond = match self.try_parse_expression() {
                    Some(expr) => expr,
                    None => self.error_type("Expected a conditional expression"),
                };
                self.expect(Token::Then);
                
                let (if_block, mut ending_token) = self.parse_block(Some(&[Token::ElseIf, Token::Else, Token::End]));
                let mut elseif_parts = Vec::new();

                while let Some(Token::ElseIf) = ending_token {
                    let elseif_cond = match self.try_parse_expression() {
                        Some(expr) => expr,
                        None => self.error_type("Expected a conditional expression"),
                    };
                    self.expect(Token::Then);

                    let (elseif_block, elseif_ending_token) = self.parse_block(Some(&[Token::ElseIf, Token::Else, Token::End]));
                    ending_token = elseif_ending_token;

                    elseif_parts.push(ConditionalBlock {
                        cond_expr: elseif_cond,
                        block: elseif_block,
                    });
                };

                let else_block = match ending_token {
                    Some(Token::Else) => Some(self.parse_block(Some(&[Token::End])).0),
                    _ => {
                        self.expect(Token::End);
                        None
                    },
                };

                Some(Statement::IfElse {
                    if_part: ConditionalBlock {
                        cond_expr: if_cond,
                        block: if_block,
                    },
                    elseif_parts,
                    else_part: else_block,
                })
            }
            false => None,
        }
    }

    fn try_parse_for_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::For) {
            true => {
                let varnames = match self.try_parse_name_list(false) {
                    Some(name_list) => name_list.0,
                    None => self.error_type("Expected a variable name"),
                };

                if self.eat(Token::Assign) {
                    if varnames.len() > 1 {
                        self.error("Initial value may be only one variable");
                    }

                    let initial_value = match self.try_parse_expression() {
                        Some(expr) => expr,
                        None => self.error_type("Expected an initial value"),
                    };
                    self.expect(Token::Comma);
    
                    let limit_value = match self.try_parse_expression() {
                        Some(expr) => expr,
                        None => self.error_type("Expected an limit value"),
                    };
    
                    let step_value = match self.eat(Token::Comma) {
                        true => {
                            match self.try_parse_expression() {
                                Some(expr) => Some(expr),
                                None => self.error_none("Expected an limit value"),
                            }
                        }
                        false => None,
                    };
    
                    match self.try_parse_block_statement() {
                        Some(Statement::Block(block)) => Some(Statement::For {
                            varname: varnames.first().unwrap().clone(),
                            initial_value,
                            limit_value,
                            step_value,
                            block,
                        }),
                        _ => self.error_none("Expected a block"),
                    }
                }
                else if self.eat(Token::In) {
                    let expr_list = match self.try_parse_expression_list() {
                        Some(expr_list) => expr_list,
                        None => self.error_type("Expected at least one expressions - iterator function"),
                    };

                    if expr_list.len() > 3 {
                        self.error("Too many expression");
                    }

                    match self.try_parse_block_statement() {
                        Some(Statement::Block(block)) => Some(Statement::GeneralFor {
                            varnames,
                            expr_list,
                            block,
                        }),
                        _ => self.error_none("Expected a block"),
                    }
                }
                else {
                    self.error_none("Unknown syntax of for loop")
                }
            }
            false => None,
        }
    }

    fn try_parse_function_definition_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Function) {
            true => {
                let name = self.expect_name();

                let mut suffixes = Vec::new();
                while self.eat(Token::Dot) {
                    suffixes.push(self.expect_name());
                }

                let method_suffix = match self.eat(Token::Colon) {
                    true => Some(self.expect_name()),
                    false => None,
                };

                let body = match self.try_parse_function_body() {
                    Some(body) => body,
                    None => self.error_type("Expected a function body"),
                };

                Some(Statement::FunctionDef {
                    name,
                    suffixes,
                    method_suffix,
                    body,
                })
            }
            false => None,
        }
    }

    fn try_parse_local_function_definition_statement(&mut self) -> Option<Statement> {
        let saved_stream_pos = self.stream.position();

        match self.eat(Token::Local) && self.eat(Token::Function) {
            true => {
                let name = self.expect_name();
                let body = match self.try_parse_function_body() {
                    Some(body) => body,
                    None => self.error_type("Expected a function body"),
                };

                Some(Statement::LocalFunctionDef {
                    name,
                    body,
                })
            }
            false => {
                self.stream.set_position(saved_stream_pos);
                None
            }
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

    try_parse_expression!(try_parse_expression,
                          [Token::Or],
                          try_parse_expression_and
    );
    try_parse_expression!(try_parse_expression_and,
                          [Token::And],
                          try_parse_expression_cmp
    );
    try_parse_expression!(try_parse_expression_cmp,
                          [Token::LessThan, Token::GreaterThan, Token::LessEqual, Token::GreaterEqual, Token::NotEqual, Token::Equal],
                          try_parse_expression_bit_or
    );
    try_parse_expression!(try_parse_expression_bit_or,
                          [Token::BitOr],
                          try_parse_expression_bit_xor
    );
    try_parse_expression!(try_parse_expression_bit_xor,
                          [Token::BitNotXor],
                          try_parse_expression_bit_and
    );
    try_parse_expression!(try_parse_expression_bit_and,
                          [Token::BitAnd],
                          try_parse_expression_shift
    );
    try_parse_expression!(try_parse_expression_shift,
                          [Token::ShiftLeft, Token::ShiftRight],
                          try_parse_expression_concat
    );
    try_parse_expression!(try_parse_expression_concat,
                          [Token::Dots2],
                          try_parse_expression_arithm
    );
    try_parse_expression!(try_parse_expression_arithm,
                          [Token::Add, Token::Sub],
                          try_parse_expression_arithm_term
    );
    try_parse_expression!(try_parse_expression_arithm_term,
                          [Token::Mul, Token::Div, Token::IDiv, Token::Mod],
                          try_parse_expression_arithm_factor
    );

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

                let block = self.parse_block(Some(&[Token::End])).0;

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
    
    #[track_caller]
    fn error_type<T>(&self, desc: &str) -> T {
        self.error_bool(desc);
        unreachable!()
    }
}