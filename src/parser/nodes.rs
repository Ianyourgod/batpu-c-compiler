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
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<Expression>, usize),
    Expression(Expression, usize),
    If(Expression, Box<Statement>, Box<Option<Statement>>, usize),
    Compound(Vec<BlockItem>, usize),
    Break(String, usize),
    Continue(String, usize),
    While(Expression, Box<Statement>, String, usize),
    DoWhile(Box<Statement>, Expression, String, usize),
    For(ForInit, Option<Expression>, Option<Expression>, Box<Statement>, String, usize),
    Empty(usize),
}

impl Statement {
    pub fn line(&self) -> usize {
        match self {
            Statement::Return(_, line) => *line,
            Statement::Expression(_, line) => *line,
            Statement::If(_, _, _, line) => *line,
            Statement::Compound(_, line) => *line,
            Statement::Break(_, line) => *line,
            Statement::Continue(_, line) => *line,
            Statement::While(_, _, _, line) => *line,
            Statement::DoWhile(_, _, _, line) => *line,
            Statement::For(_, _, _, _, _, line) => *line,
            Statement::Empty(line) => *line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Declaration(Declaration, usize),
    Expression(Expression, usize),
    Empty(usize),
}

impl ForInit {
    pub fn line(&self) -> usize {
        match self {
            ForInit::Declaration(_, line) => *line,
            ForInit::Expression(_, line) => *line,
            ForInit::Empty(line) => *line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    VarDecl(VarDecl, usize),
    FuncDecl(FuncDecl, usize),
    StructDecl(StructDecl, usize),
    Empty(usize),
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub tag: String,
    pub members: Vec<MemberDecl>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct MemberDecl {
    pub name: String,
    pub ty: Type,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub expr: Option<Initializer>,
    pub storage_class: StorageClass,
    pub ty: Type,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum Initializer {
    Single(Expression, usize),
    Compound(Vec<Initializer>, usize),
}

impl Initializer {
    pub fn line(&self) -> usize {
        match self {
            Initializer::Single(_, line) => *line,
            Initializer::Compound(_, line) => *line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration, usize),
    Statement(Statement, usize),
}

impl BlockItem {
    pub fn line(&self) -> usize {
        match self {
            BlockItem::Declaration(_, line) => *line,
            BlockItem::Statement(_, line) => *line,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: ExpressionEnum,
    pub ty: Type,
    pub line: usize,
}

impl Expression {
    pub fn new(expr: ExpressionEnum, line: usize) -> Self {
        Expression {
            expr,
            ty: Type::Int,
            line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionEnum {
    IntegerLiteral(i16, usize),
    CharLiteral(char, usize),
    StringLiteral(String, usize),
    Unop(Unop, Box<Expression>, usize),
    Binop(Binop, Box<Expression>, Box<Expression>, usize),
    Var(String, usize),
    Assign(Box<Expression>, Box<Expression>, usize), // lvalues :scream:
    OpAssign(Binop, Box<Expression>, Box<Expression>, usize),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>, usize),
    Increment(Box<Expression>, usize),
    Decrement(Box<Expression>, usize),
    FunctionCall(String, Vec<Expression>, usize),
    Dereference(Box<Expression>, usize),
    AddressOf(Box<Expression>, usize),
    Subscript(Box<Expression>, Box<Expression>, usize),
    Cast(Type, Box<Expression>, usize),
    SizeOf(Box<Expression>, usize),
    SizeOfType(Type, usize),
    Dot(Box<Expression>, String, usize),
    Arrow(Box<Expression>, String, usize),
}

impl ExpressionEnum {
    pub fn line(&self) -> usize {
        match self {
            ExpressionEnum::IntegerLiteral(_, line) => *line,
            ExpressionEnum::CharLiteral(_, line) => *line,
            ExpressionEnum::StringLiteral(_, line) => *line,
            ExpressionEnum::Unop(_, _, line) => *line,
            ExpressionEnum::Binop(_, _, _, line) => *line,
            ExpressionEnum::Var(_, line) => *line,
            ExpressionEnum::Assign(_, _, line) => *line,
            ExpressionEnum::OpAssign(_, _, _, line) => *line,
            ExpressionEnum::Conditional(_, _, _, line) => *line,
            ExpressionEnum::Increment(_, line) => *line,
            ExpressionEnum::Decrement(_, line) => *line,
            ExpressionEnum::FunctionCall(_, _, line) => *line,
            ExpressionEnum::Dereference(_, line) => *line,
            ExpressionEnum::AddressOf(_, line) => *line,
            ExpressionEnum::Subscript(_, _, line) => *line,
            ExpressionEnum::Cast(_, _, line) => *line,
            ExpressionEnum::SizeOf(_, line) => *line,
            ExpressionEnum::SizeOfType(_, line) => *line,
            ExpressionEnum::Dot(_, _, line) => *line,
            ExpressionEnum::Arrow(_, _, line) => *line,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Negate,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Binop {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            Type::Array(ty, size) => ty.size() * size,
            Type::Struct(_) => unreachable!("INTERNAL ERROR. PLEASE REPORT: Struct types' size cannot be found through this method"),

            Type::Fn(_, _) => unreachable!("INTERNAL ERROR. PLEASE REPORT: Function types should not be used in this context"),

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

    pub fn type_size(&self, ty: &Type) -> i32 {
        match ty {
            Type::Struct(id) => {
                let (size, _) = self.symbols.get(id).unwrap();
                *size
            },
            _ => ty.size() as i32,
        }
    }

    pub fn contains(&self, id: &String) -> bool {
        self.symbols.contains_key(id)
    }
}