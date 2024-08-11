#[derive(Debug)]
pub struct Program {
    pub statements: Vec<FuncDecl>,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral(i8),
}