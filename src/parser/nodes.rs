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
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>)
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