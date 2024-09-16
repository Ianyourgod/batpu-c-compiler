pub use crate::parser::nodes::Type;

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<TopLevel>,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    FuncDef(FuncDef),
    StaticVariable(String, bool, Type, i32),
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub body: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Return(Option<Val>),
    Unary(Unop, Val, Val),
    Binary(Binop, Val, Val, Val),
    Copy(Val, Val),
    Jump(String),
    JumpIfZero(Val, String),
    JumpIfNotZero(Val, String),
    Label(String),
    FunCall(String, Vec<Val>, Option<Val>, bool), // bool is for if global
    GetAddress(Val, Val),
    Load(Val, Val),
    Store(Val, Val),
    CopyToOffset(Val, Val, i16), // val, var, offset
    CopyFromOffset(Val, i16, Val), // var, offset, val
    AddPtr(Val, Val, Val, Val), // val, val, offset, dest
}

#[derive(Debug, Clone)]
pub enum Val {
    Const(i8),
    Var(String, Type),
    DereferencedPtr(Box<Val>),
    SubObject((String, Type), i16), // identifier, offset
}

#[derive(Debug, Clone)]
pub enum Unop {
    Negate,
    BitwiseNot,
    LogicalNot,
    AddImm,
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