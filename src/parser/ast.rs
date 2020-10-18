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
    /*Assignment(AssignmentStatement),
    FunctionCall(FunctionCallStatement),
    Label(LabelStatement),
    Break,
    Goto(GotoStatement),
    Do(DoStatement),
    While(WhileStatement),
    Repeat(RepeatStatement),
    If(IfStatement),
    For(ForStatement),
    ForEach(ForEachStatement),
    Function(FunctionStatement),
    LocalFunction(LocalFunctionStatement),
    LocalVariable(LocalVariableStatement),*/
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
    CallMethod(CallArgs),
}

#[derive(Debug)]
pub enum CallArgs {
    ExpressionList(Vec<Expression>),
    String(String),
}