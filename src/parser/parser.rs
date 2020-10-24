/* The list of implemented the Lua grammar at the moment

$$$ - fully implemented relational to original Lua grammar

chunk               ::= block                                                                       $$$
block               ::= {stat} [retstat]                                                            $$$
stat                ::= ‘;’ |                                                                       $$$
                        varlist ‘=’ explist |                                                       $$$
                        functioncall |                                                              $$$
                        label |                                                                     $$$
                        break |                                                                     $$$
                        goto Name |                                                                 $$$
                        do block end |                                                              $$$
                        while exp do block end |                                                    $$$
                        repeat block until exp |                                                    $$$
                        if exp then block {elseif exp then block} [else block] end |                $$$
                        for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |                           $$$
                        for namelist in explist do block end |                                      $$$
                        function funcname funcbody |                                                $$$
                        local function Name funcbody |                                              $$$
                        local attnamelist [‘=’ explist]                                             $$$
attnamelist         ::= Name attrib {‘,’ Name attrib}                                               $$$
attrib              ::= [‘<’ Name ‘>’]                                                              $$$
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
        TokenInfo, Token, Location,
    },
    parser::{
        TokenStream,
        ast::{
            Chunk, Block,
            Statement, ReturnStatement,
            Expression, FunctionBody, Suffixed, Suffix, CallArgs, Table, TableField,
            ConditionalBlock, LocalVariable, LocalVariableAttrib,
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
                                None => self.error("Expected an expression"),
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
        let mut block = Block {
            statements: Vec::new(),
            return_statement: None,
        };

        let mut found_ending_token = None;

        while !self.stream.is_eof() {
            if let Some(statement) = self.try_parse_statement() {
                block.statements.push(statement);
            }
            else if let Some(return_statement) = self.try_parse_return_statement() {
                match block.return_statement {
                    None => block.return_statement = Some(return_statement),
                    Some(_) => self.error("Expected an end of block after first return"),
                }
            }
            else if let Some(ending_token) = ending_token.and_then(|tokens| self.eat_any_of(tokens)) {
                found_ending_token = Some(ending_token);
                break;
            }
            else {
                self.error("Unknown syntax construction")
            }
        }

        if let Some(_) = ending_token {
            if found_ending_token.is_none() {
                self.error(&format!("Expected the block ending token"))
            }
        }

        (block, found_ending_token)
    }

    fn try_parse_statement(&mut self) -> Option<Statement> {
        while self.eat(Token::SemiColon) {}

        self.try_parse_assignment_statement()
        .or_else(|| self.try_parse_function_call_statement())
        .or_else(|| self.try_parse_label_statement())
        .or_else(|| self.try_parse_break_statement())
        .or_else(|| self.try_parse_goto_statement())
        .or_else(|| self.try_parse_block_statement())
        .or_else(|| self.try_parse_while_statement())
        .or_else(|| self.try_parse_repeat_until_statement())
        .or_else(|| self.try_parse_if_else_statement())
        .or_else(|| self.try_parse_for_statement())
        .or_else(|| self.try_parse_function_definition_statement())
        .or_else(|| self.try_parse_local_function_definition_statement())
        .or_else(|| self.try_parse_local_variables_statement())
    }

    fn try_parse_assignment_statement(&mut self) -> Option<Statement> {
        match self.try_parse_variables_list() {
            Some(var_list) => {
                self.expect(Token::Assign);
                
                let expr_list = match self.try_parse_expression_list() {
                    Some(expr_list) => expr_list,
                    None => self.error("Expected an expression list"),
                };

                Some(Statement::Assignment {
                    var_list,
                    expr_list,
                })
            }
            None => None,
        }
    }

    fn try_parse_variables_list(&mut self) -> Option<Vec<Suffixed>> {
        let saved_stream_pos = self.stream.position();
        
        let mut vars = Vec::new();
        while vars.is_empty() || self.eat(Token::Comma) {
            match self.try_parse_suffixed() {
                Some(suffixed) if matches!(suffixed.suffixes.last(), Some(Suffix::Index(_)) | None) => vars.push(suffixed),
                Some(_) if !vars.is_empty() => self.error("Function or method call is not a variable"),
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

        match self.try_parse_suffixed() {
            Some(suffixed) if matches!(suffixed.suffixes.last(), Some(Suffix::CallFree(_)) | Some(Suffix::CallMethod{name: _, args: _})) => {
                Some(Statement::FunctionCall(suffixed))
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
            true => {
                let label_name = self.expect_name();
                Some(Statement::Goto(label_name))
            }
            false => None,
        }
    }

    fn try_parse_block_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Do) {
            true => {
                let (block, _) = self.parse_block(Some(&[Token::End]));
                Some(Statement::Block(block))
            }
            false => None,
        }
    }

    fn try_parse_while_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::While) {
            true => {
                let condition = match self.try_parse_expression() {
                    Some(expr) => expr,
                    None => self.error("Expected a conditional expression")
                };

                let block = match self.try_parse_block_statement() {
                    Some(Statement::Block(block)) => block,
                    _ => self.error("Expected a block"),
                };

                Some(Statement::While {
                    condition,
                    block,
                })
            }
            false => None,
        }
    }

    fn try_parse_repeat_until_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Repeat) {
            true => {
                let (block, _) = self.parse_block(Some(&[Token::Until]));
                let condition = match self.try_parse_expression() {
                    Some(expr) => expr,
                    None => self.error("Expected a conditional expression"),
                };

                Some(Statement::RepeatUntil {
                    condition,
                    block,
                })
            }
            false => None,
        }
    }

    fn try_parse_if_else_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::If) {
            true => {
                let if_cond = match self.try_parse_expression() {
                    Some(expr) => expr,
                    None => self.error("Expected a conditional expression"),
                };
                self.expect(Token::Then);
                
                let (if_block, mut ending_token) = self.parse_block(Some(&[Token::ElseIf, Token::Else, Token::End]));
                let mut elseif_parts = Vec::new();

                while let Some(Token::ElseIf) = ending_token {
                    let elseif_cond = match self.try_parse_expression() {
                        Some(expr) => expr,
                        None => self.error("Expected a conditional expression"),
                    };
                    self.expect(Token::Then);

                    let (elseif_block, elseif_ending_token) = self.parse_block(Some(&[Token::ElseIf, Token::Else, Token::End]));
                    ending_token = elseif_ending_token;

                    elseif_parts.push(ConditionalBlock {
                        condition: elseif_cond,
                        block: elseif_block,
                    });
                };

                let else_part = match ending_token {
                    Some(Token::Else) => {
                        let (block, _) = self.parse_block(Some(&[Token::End]));
                        Some(block)
                    }
                    _ => None,
                };

                Some(Statement::IfElse {
                    if_part: ConditionalBlock {
                        condition: if_cond,
                        block: if_block,
                    },
                    elseif_parts,
                    else_part,
                })
            }
            false => None,
        }
    }

    fn try_parse_for_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::For) {
            true => {
                let var_names = match self.try_parse_name_list(false) {
                    Some(name_list) => name_list.0,
                    None => self.error("Expected a variable name"),
                };

                if self.eat(Token::Assign) {
                    if var_names.len() > 1 {
                        self.error("Initial value may declare only one variable")
                    }

                    let initial_value = match self.try_parse_expression() {
                        Some(expr) => expr,
                        None => self.error("Expected an initial value expression"),
                    };
                    self.expect(Token::Comma);
    
                    let limit_value = match self.try_parse_expression() {
                        Some(expr) => expr,
                        None => self.error("Expected a limit value expression"),
                    };
    
                    let step_value = match self.eat(Token::Comma) {
                        true => {
                            match self.try_parse_expression() {
                                Some(expr) => Some(expr),
                                None => self.error("Expected a step value expression"),
                            }
                        }
                        false => None,
                    };
    
                    let block = match self.try_parse_block_statement() {
                        Some(Statement::Block(block)) => block,
                        _ => self.error("Expected a block"),
                    };

                    Some(Statement::For {
                        var_name: var_names.first().unwrap().clone(),
                        initial_value,
                        limit_value,
                        step_value,
                        block,
                    })
                }
                else if self.eat(Token::In) {
                    let expr_list = match self.try_parse_expression_list() {
                        Some(expr_list) => expr_list,
                        None => self.error("Expected at least one expression - iterator function"),
                    };

                    if expr_list.len() > 3 {
                        self.error("Too many expressions")
                    }

                    let block = match self.try_parse_block_statement() {
                        Some(Statement::Block(block)) => block,
                        _ => self.error("Expected a block"),
                    };

                    Some(Statement::GeneralFor {
                        var_names,
                        expr_list,
                        block,
                    })
                }
                else {
                    self.error("Unknown syntax of for-loop")
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
                    None => self.error("Expected a function body"),
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
                    None => self.error("Expected a local function body"),
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

    fn try_parse_local_variables_statement(&mut self) -> Option<Statement> {
        match self.eat(Token::Local) {
            true => {
                let mut var_list = Vec::new();
                
                while var_list.is_empty() || self.eat(Token::Comma) {
                    let name = self.expect_name();
                    let attrib = self.try_parse_local_variable_attrib();
                    var_list.push(LocalVariable {
                        name,
                        attrib,
                    });
                }

                let expr_list = match self.eat(Token::Assign) {
                    true => {
                        match self.try_parse_expression_list() {
                            Some(expr_list) => expr_list,
                            None => self.error("Expected an initial values expression list"),
                        }
                    }
                    false => Vec::new(),
                };

                Some(Statement::LocalVariables {
                    var_list,
                    expr_list,
                })
            }
            false => None,
        }
    }

    fn try_parse_local_variable_attrib(&mut self) -> Option<LocalVariableAttrib> {
        match self.eat(Token::LessThan) {
            true => {
                let name = self.expect_name();
                self.expect(Token::GreaterThan);

                let attrib = match name.as_str() {
                    "const" => LocalVariableAttrib::Const,
                    "close" => LocalVariableAttrib::Close,
                    _ => self.error("Expected 'const' or 'close' local variable attribute"),
                };
                Some(attrib)
            }
            false => None,
        }
    }

    fn try_parse_return_statement(&mut self) -> Option<ReturnStatement> {
        match self.eat(Token::Return) {
            true => {
                let expr_list = self.try_parse_expression_list().unwrap_or_else(|| Vec::new());
                self.eat(Token::SemiColon);
                Some(ReturnStatement(expr_list))
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
        let factor_expr = if self.eat(Token::Nil) {
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
        else if let Some(number) = self.eat_int_number() {
            Some(Expression::IntNumber(number))
        }
        else if let Some(string) = self.eat_string() {
            Some(Expression::String(string.clone()))
        }
        else if self.eat(Token::Dots3) {
            Some(Expression::VarArg)
        }
        else if self.eat(Token::Function) {
            let body = match self.try_parse_function_body() {
                Some(body) => body,
                None => self.error("Expected a function body"),
            };
            Some(Expression::FunctionDef(body))
        }
        else if let Some(op) = self.eat_any_of(&[Token::Not, Token::Len, Token::Sub, Token::BitNotXor]) {
            let expr = match self.try_parse_expression_arithm_factor() {
                Some(expr) => expr,
                None => self.error("Expected a factor expression"),
            };
            Some(Expression::UnaryOp {
                op,
                expr: Box::new(expr),
            })
        }
        else if self.look_for(Token::LeftBrace) {
            let table_constructor = match self.try_parse_table_constructor() {
                Some(table_constructor) => table_constructor,
                None => self.error("Expected a table constructor"),
            };
            Some(Expression::Table(table_constructor))
        }
        else {
            self.try_parse_suffixed().map(|suffixed| Expression::Suffixed(Box::new(suffixed)))
        };

        match factor_expr {
            Some(factor_expr) => {
                match self.eat(Token::Pow) {
                    true => {
                        let pow_expr = match self.try_parse_expression() {
                            Some(expr) => expr,
                            None => self.error("Expected a pow expression")
                        };
                        Some(Expression::BinaryOp {
                            op: Token::Pow,
                            left_expr: Box::new(factor_expr),
                            right_expr: Box::new(pow_expr),
                        })
                    }
                    false => Some(factor_expr),
                }
            }
            None => None,
        }
    }

    fn try_parse_suffixed(&mut self) -> Option<Suffixed> {
        let main_expr = if let Some(name) = self.eat_name() {
            Some(Expression::Named(name))
        }
        else if self.eat(Token::LeftParen) {
            let expr = match self.try_parse_expression() {
                Some(expr) => expr,
                None => self.error("Expected an expression"),
            };
            self.expect(Token::RightParen);
            Some(expr)
        }
        else {
            None
        };

        match main_expr {
            Some(main_expr) => Some(Suffixed {
                expr: main_expr,
                suffixes: self.parse_suffixes(),
            }),
            None => None,
        }
    }

    fn parse_suffixes(&mut self) -> Vec<Suffix> {
        let mut suffixes = Vec::new();
        loop {
            if self.eat(Token::Dot) {
                let name = self.expect_name();
                suffixes.push(Suffix::Index(Expression::Named(name)));
            }
            else if self.eat(Token::LeftBracket) {
                let index_expr = match self.try_parse_expression() {
                    Some(expr) => expr,
                    None => self.error("Expected an index expression to index a table"),
                };
                self.expect(Token::RightBracket);
                suffixes.push(Suffix::Index(index_expr));
            }
            else if let Some(args) = self.try_parse_call_arguments() {
                suffixes.push(Suffix::CallFree(args))
            }
            else if self.eat(Token::Colon) {
                let name = self.expect_name();
                let args = match self.try_parse_call_arguments() {
                    Some(args) => args,
                    None => self.error("Expected a list of call arguments or ()"),
                };
                suffixes.push(Suffix::CallMethod {
                    name,
                    args,
                })
            }
            else {
                break;
            }
        }

        suffixes
    }

    fn try_parse_call_arguments(&mut self) -> Option<CallArgs> {
        if self.eat(Token::LeftParen) {
            let expr_list = match self.try_parse_expression_list() {
                Some(expr_list) => expr_list,
                None => Vec::new(),
            };
            self.expect(Token::RightParen);
            Some(CallArgs::ExpressionList(expr_list))
        }
        else if self.look_for(Token::LeftBrace) {
            let table_constructor = match self.try_parse_table_constructor() {
                Some(table_constructor) => table_constructor,
                None => self.error("Expected a table constructor"),
            };
            Some(CallArgs::Table(table_constructor))
        }
        else if let Some(string) = self.eat_string() {
            Some(CallArgs::String(string))
        }
        else {
            None
        }
    }

    fn try_parse_table_constructor(&mut self) -> Option<Table> {
        match self.eat(Token::LeftBrace) {
            true => {
                let mut fields = Vec::new();
                loop {
                    if self.eat(Token::LeftBracket) {
                        let key_expr = match self.try_parse_expression() {
                            Some(expr) => expr,
                            None => self.error("Expected an expression of key"),
                        };

                        self.expect(Token::RightBracket);
                        self.expect(Token::Assign);

                        let value_expr = match self.try_parse_expression() {
                            Some(expr) => expr,
                            None => self.error("Expected an expression of value"),
                        };

                        fields.push(TableField {
                            key: Some(key_expr),
                            value: value_expr,
                        })
                    }
                    else if self.eat(Token::RightBrace) {
                        break;
                    }
                    else {
                        let has_name = match self.stream.look_token(0) {
                            Some(Token::Identifier(_)) => true,
                            _ => false,
                        };
                        let has_assign = match self.stream.look_token(1) {
                            Some(Token::Assign) => true,
                            _ => false,
                        };
                        if has_name && has_assign {
                            let name = self.expect_name();
                            self.expect(Token::Assign);

                            let value_expr = match self.try_parse_expression() {
                                Some(expr) => expr,
                                None => self.error("Expected an expression of value"),
                            };

                            fields.push(TableField {
                                key: Some(Expression::Named(name)),
                                value: value_expr,
                            })
                        }
                        else if let Some(expr) = self.try_parse_expression() {
                            fields.push(TableField {
                                key: None,
                                value: expr,
                            });
                        }
                        else {
                            self.error("Unexpected token")
                        }
                    }

                    if !fields.is_empty() && self.eat_any_of(&[Token::Comma, Token::SemiColon]).is_none() && !self.look_for(Token::RightBrace) {
                        self.error("Expected a separator ',' or ';'")
                    }
                }

                Some(Table(fields))
            }
            false => None,
        }
    }

    fn try_parse_function_body(&mut self) -> Option<FunctionBody> {
        match self.eat(Token::LeftParen) {
            true => {
                let (param_list, param_list_has_vararg) = match self.try_parse_name_list(true) {
                    Some(param_list) => param_list,
                    None => (Vec::new(), false),
                };
                self.expect(Token::RightParen);

                let (block, _) = self.parse_block(Some(&[Token::End]));

                Some(FunctionBody {
                    param_list,
                    param_list_has_vararg,
                    block,
                })
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
                    names.push(name);
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
                        true => self.error("Expected a name or vararg"),
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
            Some(token) => self.error(&format!("Expected '{}' instead '{}'", expected_token, token)),
            _ => self.error("Unexpected end of token stream"),
        };
    }

    #[track_caller]
    fn expect_name(&mut self) -> String {
        match self.stream.look_token(0).cloned() {
            Some(Token::Identifier(name)) => {
                self.stream.next();
                name
            },
            Some(_) => self.error("Expected an identifier"),
            _ => self.error("Unexpected end of token stream"),
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
    fn eat_int_number(&mut self) -> Option<i64> {
        match self.stream.look_token(0).cloned() {
            Some(Token::IntNumber(number)) => {
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
    fn error<T>(&self, desc: &str) -> T {
        fn build_pointer_str(loc: &Location, len: usize) -> String {
            match loc.column() > 0 {
                true => " ".repeat(loc.column() - 1) + &"^".repeat(len),
                false => "^".repeat(len),
            }
        }

        match self.stream.last_token_info() {
            Some(token_info) => {
                let token_begin_loc = token_info.begin_location();
                let token_end_loc = token_info.end_location();
                let source_lines = token_begin_loc.lines();
               
                let mut message = format!("\n{}:{}: Parser error: {}\n", token_begin_loc.source_name(), token_begin_loc, desc);

                if let Some(line) = source_lines.get(token_begin_loc.line() - 1) {
                    let pointer_len = match token_begin_loc.line() == token_end_loc.line() {
                        true => token_end_loc.column() - token_begin_loc.column(),
                        false => line.chars().count() - token_begin_loc.column() + 1,
                    };
                    let pointer = build_pointer_str(token_begin_loc, pointer_len);
                    message += &format!("{}\n{}\n", line, pointer);

                    for i in token_begin_loc.line() + 1..token_end_loc.line() {
                        if let Some(line) = source_lines.get(i - 1) {
                            let pointer_len = line.chars().count();
                            let pointer = "^".repeat(pointer_len);
                            message += &format!("{}\n{}\n", line, pointer);
                        }
                    }

                    if token_begin_loc.line() != token_end_loc.line() {
                        if let Some(line) = source_lines.get(token_end_loc.line() - 1) {
                            let pointer_len = token_end_loc.column();
                            let pointer = build_pointer_str(token_end_loc, pointer_len);
                            message += &format!("{}\n{}\n", line, pointer);
                        }
                    }
                }
                panic!(message);
            }
            None => {
                panic!(format!("Parser error: {}\n", desc))
            }
        }
    }
}