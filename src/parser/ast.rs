use crate::lexer::Token;

#[derive(Debug)]
pub struct Chunk {
    pub block: Block,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_statement: Option<ReturnStatement>,
}

#[derive(Debug)]
pub enum Statement {
    Empty,
    Assignment {
        var_list: Vec<Expression>,
        expr_list: Vec<Expression>,
    },
    FunctionCall(Expression),
    Label(String),
    Break,
    Goto(String),
    Block(Block),
    While {
        cond: Expression,
        block: Block,
    },
    RepeatUntil {
        cond: Expression,
        block: Block,
    },
    IfElse {
        if_part: ConditionalBlock,
        elseif_parts: Vec<ConditionalBlock>,
        else_part: Option<Block>,
    },
    For {
        varname: String,
        initial_value: Expression,
        limit_value: Expression,
        step_value: Option<Expression>,
        block: Block,
    },
    GeneralFor {
        varnames: Vec<String>,
        expr_list: Vec<Expression>,
        block: Block,
    },
    FunctionDef {
        name: String,
        suffixes: Vec<String>,
        method_suffix: Option<String>,
        body: FunctionBody,
    },
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression_list: Vec<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    VarArg,
    FunctionDef(FunctionBody),
    Named(String),
    Suffixed {
        expr: Box<Expression>,
        suffixes: Vec<Suffix>,
    },
    Table(Vec<TableField>),
    BinaryOp {
        op: Token,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
    },
    UnaryOp {
        op: Token,
        expr: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct FunctionBody {
    pub param_list: Vec<String>,
    pub param_list_has_vararg: bool,
    pub block: Block,
}

#[derive(Debug)]
pub enum Suffix {
    Index(Box<Expression>),
    CallFree(CallArgs),
    CallMethod {
        name: String,
        args: CallArgs,
    },
}

#[derive(Debug)]
pub enum CallArgs {
    ExpressionList(Vec<Expression>),
    Table(Box<Expression>),
    String(String),
}

#[derive(Debug)]
pub struct TableField {
    pub key: Option<Expression>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ConditionalBlock {
    pub cond_expr: Expression,
    pub block: Block,
}