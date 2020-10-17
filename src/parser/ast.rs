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
}