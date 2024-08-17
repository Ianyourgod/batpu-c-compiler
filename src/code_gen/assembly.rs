#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<FuncDecl>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub body: Vec<Instruction>,
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
    Lod(Register, i16, Register),
    Str(Register, i16, Register),
    Jmp(String),
    JmpCC(CondCode, String),
    Label(String),
    Return,
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
    Pseudo(String),
    Stack(i16),
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