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
        var_list: Vec<Suffixed>,
        expr_list: Vec<Expression>,
    },
    FunctionCall(Suffixed),
    Label(String),
    Break,
    Goto(String),
    Block(Block),
    While {
        condition: Expression,
        block: Block,
    },
    RepeatUntil {
        condition: Expression,
        block: Block,
    },
    IfElse {
        if_part: ConditionalBlock,
        elseif_parts: Vec<ConditionalBlock>,
        else_part: Option<Block>,
    },
    For {
        var_name: String,
        initial_value: Expression,
        limit_value: Expression,
        step_value: Option<Expression>,
        block: Block,
    },
    GeneralFor {
        var_names: Vec<String>,
        expr_list: Vec<Expression>,
        block: Block,
    },
    FunctionDef {
        name: String,
        suffixes: Vec<String>,
        method_suffix: Option<String>,
        body: FunctionBody,
    },
    LocalFunctionDef {
        name: String,
        body: FunctionBody,
    },
    LocalVariables {
        var_list: Vec<LocalVariable>,
        expr_list: Vec<Expression>,
    },
}

#[derive(Debug)]
pub struct ReturnStatement(pub Vec<Expression>);

#[derive(Debug)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(f64),
    IntNumber(i64),
    String(String),
    VarArg,
    FunctionDef(FunctionBody),
    Named(String),
    Suffixed(Box<Suffixed>),
    Table(Table),
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
pub struct Suffixed {
    pub expr: Expression,
    pub suffixes: Vec<Suffix>,
}

#[derive(Debug)]
pub enum Suffix {
    Index(Expression),
    CallFree(CallArgs),
    CallMethod {
        name: String,
        args: CallArgs,
    },
}

#[derive(Debug)]
pub enum CallArgs {
    ExpressionList(Vec<Expression>),
    Table(Table),
    String(String),
}

#[derive(Debug)]
pub struct Table(pub Vec<TableField>);

#[derive(Debug)]
pub struct TableField {
    pub key: Option<Expression>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ConditionalBlock {
    pub condition: Expression,
    pub block: Block,
}

#[derive(Debug)]
pub struct LocalVariable {
    pub name: String,
    pub attrib: Option<LocalVariableAttrib>,
}

#[derive(Debug)]
pub enum LocalVariableAttrib {
    Const,
    Close,
}