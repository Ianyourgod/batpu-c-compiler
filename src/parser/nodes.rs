#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<FuncDecl>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub body: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Box<Option<Statement>>),
    Compound(Vec<BlockItem>),
    Break(String),
    Continue(String),
    While(Expression, Box<Statement>, String),
    DoWhile(Box<Statement>, Expression, String),
    For(ForInit, Option<Expression>, Option<Expression>, Box<Statement>, String),
    Empty,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Declaration(Declaration),
    Expression(Expression),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Identifier,
    pub expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    Var(String),
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(i8),
    Unop(Unop, Box<Expression>),
    Binop(Binop, Box<Expression>, Box<Expression>),
    Var(Identifier),
    Assign(Box<Expression>, Box<Expression>), // lvalues :scream:
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Increment(Box<Expression>),
    Decrement(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Unop {
    Negate,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug, Clone)]
pub enum Binop {
    Add,
    Subtract,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}