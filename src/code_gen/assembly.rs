pub use crate::parser::nodes::Type;

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<TopLevel>,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    FuncDef(FuncDecl),
    StaticVariable(String, bool, i32),
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub body: Vec<Instruction>,
    pub stack_size: i16,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub enum CondCode {
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThan,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ldi(Operand, i8),
    Adi(Operand, i8),
    Unary(Unop, Operand, Operand),
    Binary(Binop, Operand, Operand, Operand),
    Cmp(Operand, Operand),
    AllocateStack(u8),
    //DeallocateStack(u8),
    Lod(Register, i16, Operand),
    Str(Operand, i16, Register),
    Jmp(String),
    JmpCC(CondCode, String),
    Label(String),
    //Push(Operand),
    Call(String, bool),
    Return,

    // "fake" instruction
    Lea(Operand, Operand),
}

#[derive(Debug, Clone)]
pub enum Unop {
    Negate,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Immediate(i8),
    Register(Register),
    Pseudo(String, Type),
    PseudoMem(String, i16, Type),
    Memory(Register, i16),
    Data(String),
}

#[derive(Debug, Clone)]
pub enum Binop {
    Add,
    Subtract,
    Nor,
}

#[derive(Debug, Clone)]
pub struct Register {
    pub name: String,
}

impl Register {
    pub fn new(name: String) -> Register {
        Register {
            name,
        }
    }

    pub fn to_string(&self) -> String {
        match self.name.as_str() {
            "rsp" => "r14".to_string(),
            "rbp" => "r15".to_string(),
            _ => self.name.to_lowercase(),
        }
    }
}