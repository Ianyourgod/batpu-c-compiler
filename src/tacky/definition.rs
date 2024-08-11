#[derive(Debug)]
pub struct Program {
    pub statements: Vec<FuncDecl>,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Val),
    Unary(Unop, Val, Val),
}

#[derive(Debug, Clone)]
pub enum Val {
    Const(i8),
    Var(String),
}

#[derive(Debug)]
pub enum Unop {
    Negate,
    BitwiseNot,
}