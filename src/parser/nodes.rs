use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<BlockItem>,
    pub storage_class: StorageClass,
    pub ty: Type,
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
    pub name: String,
    pub expr: Option<Expression>,
    pub storage_class: StorageClass,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: ExpressionEnum,
    pub ty: Type,
}

impl Expression {
    pub fn new(expr: ExpressionEnum) -> Self {
        Expression {
            expr,
            ty: Type::Int,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionEnum {
    IntegerLiteral(i8),
    Unop(Unop, Box<Expression>),
    Binop(Binop, Box<Expression>, Box<Expression>),
    Var(String),
    Assign(Box<Expression>, Box<Expression>), // lvalues :scream:
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Increment(Box<Expression>),
    Decrement(Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    Dereference(Box<Expression>),
    AddressOf(Box<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Negate,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug, Clone, Copy)]
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
    symbols: HashMap<String, (Type, TableEntry)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TableEntry {
    FnAttr(bool, bool), // defined, global
    StaticAttr(InitialValue, bool), // initial value, global
    LocalAttr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(i32),
    NoInit,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: String, ty: (Type, TableEntry)) {
        self.symbols.insert(id, ty);
    }

    pub fn lookup(&self, id: &String) -> Option<&(Type, TableEntry)> {
        self.symbols.get(id)
    }

    pub fn contains(&self, id: &String) -> bool {
        self.symbols.contains_key(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Fn(Vec<Type>, Box<Type>),
    Pointer(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StorageClass {
    Static,
    Extern,
    Auto,
}