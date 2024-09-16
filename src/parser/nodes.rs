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
    Return(Option<Expression>),
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
    StructDecl(StructDecl),
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub tag: String,
    pub members: Vec<MemberDecl>,
}

#[derive(Debug, Clone)]
pub struct MemberDecl {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub expr: Option<Initializer>,
    pub storage_class: StorageClass,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Initializer {
    Single(Expression),
    Compound(Vec<Initializer>),
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
    CharLiteral(char),
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
    Subscript(Box<Expression>, Box<Expression>),
    Cast(Type, Box<Expression>),
    SizeOf(Box<Expression>),
    SizeOfType(Type),
    Dot(Box<Expression>, String),
    Arrow(Box<Expression>, String),
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
    Char,
    Fn(Vec<Type>, Box<Type>),
    Pointer(Box<Type>),
    Array(Box<Type>, i16),
    Struct(String),

    Void,
}

impl Type {
    pub fn size(&self) -> i16 {
        match self {
            Type::Int | Type::Pointer(_) | Type::Char => 1,
            Type::Fn(_, _) => panic!("Function types should not be used in this context"),
            Type::Array(ty, size) => ty.size() * size,
            Type::Struct(_) => panic!("Struct types should not be used in this context"),

            Type::Void => 1, // void is weird
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StorageClass {
    Static,
    Extern,
    Auto,
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    symbols: HashMap<String, (i32, Vec<MemberEntry>)>,
}

#[derive(Debug, Clone)]
pub struct MemberEntry {
    pub name: String,
    pub ty: Type,
    pub offset: i32,
}

impl TypeTable {
    pub fn new() -> Self {
        TypeTable {
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: String, ty: (i32, Vec<MemberEntry>)) {
        self.symbols.insert(id, ty);
    }

    pub fn lookup(&self, id: &String) -> Option<&(i32, Vec<MemberEntry>)> {
        self.symbols.get(id)
    }

    pub fn lookup_member_entry(&self, id: &String, member: &String) -> Option<&MemberEntry> {
        match self.symbols.get(id) {
            Some((_, members)) => members.iter().find(|m| m.name == *member),
            None => None,
        }
    }

    pub fn contains(&self, id: &String) -> bool {
        self.symbols.contains_key(id)
    }
}