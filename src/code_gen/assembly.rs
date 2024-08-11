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
    Ldi(Register, i8),
    Return,
}

#[derive(Debug)]
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