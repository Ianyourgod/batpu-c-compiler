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
    pub defined: bool,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondCode {
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ldi(Operand, i16),
    Adi(Operand, i16),
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

    Comment(String),

    // "fake" instruction
    Lea(Operand, Operand),

    UserAsm(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unop {
    Negate,
    BitwiseNot,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Operand {
    Immediate(i16),
    Register(Register),
    Pseudo(String, Type),
    PseudoMem(String, i16, Type),
    Memory(Register, i16),
    Data(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binop {
    Add,
    Subtract,
    Nor,
    And,
    Xor,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct FunctionTable {
    functions: std::collections::HashMap<String, Vec<Register>>,
}

impl FunctionTable {
    pub fn new() -> FunctionTable {
        FunctionTable {
            functions: std::collections::HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, params: Vec<Register>) {
        self.functions.insert(name, params);
    }

    pub fn lookup(&self, name: &String) -> Option<&Vec<Register>> {
        self.functions.get(name)
    }
}