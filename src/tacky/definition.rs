#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<FuncDef>,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Instruction>,
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
    FunCall(String, Vec<Val>, Val),
}

#[derive(Debug, Clone)]
pub enum Val {
    Const(i8),
    Var(String),
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