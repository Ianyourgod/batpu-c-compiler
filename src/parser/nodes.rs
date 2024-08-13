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
}

#[derive(Debug, Clone)]
pub enum Unop {
    Negate,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum Binop {
    Add,
    Subtract,
}