#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<FuncDecl>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(i8),
    Unop(Unop, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Unop {
    Negate,
    BitwiseNot,
}