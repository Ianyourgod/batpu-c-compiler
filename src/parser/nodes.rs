use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<FuncDecl>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<Identifier>,
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
pub enum Declaration {
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Identifier,
    pub expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
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
    FunctionCall(String, Vec<Expression>),
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

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: HashMap<Identifier, (Type, bool)>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: Identifier, ty: (Type, bool)) {
        self.symbols.insert(id, ty);
    }

    pub fn lookup(&self, id: &Identifier) -> Option<&(Type, bool)> {
        self.symbols.get(id)
    }

    pub fn contains(&self, id: &Identifier) -> bool {
        self.symbols.contains_key(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Fn(i32)
}