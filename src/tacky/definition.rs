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
    Return(Val),
    Unary(Unop, Val, Val),
    Binary(Binop, Val, Val, Val),
    Copy(Val, Val),
    Jump(String),
    JumpIfZero(Val, String),
    JumpIfNotZero(Val, String),
    Label(String),
    FunCall(String, Vec<Val>, Val, bool), // bool is for if global
    GetAddress(Val, Val),
    Load(Val, Val),
    Store(Val, Val),
}

#[derive(Debug, Clone)]
pub enum Val {
    Const(i8),
    Var(String, Type),
    DereferencedPtr(Box<Val>),
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