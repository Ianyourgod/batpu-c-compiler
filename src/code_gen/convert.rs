use crate::code_gen::assembly;
use crate::tacky::definition;

pub struct ConvertPass {
    program: definition::Program,
}

impl ConvertPass {
    pub fn new(program: definition::Program) -> ConvertPass {
        ConvertPass {
            program,
        }
    }

    pub fn generate(&self) -> assembly::Program {
        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        for stmt in &self.program.statements {
            self.generate_function(stmt, &mut assembly);
        }

        assembly
    }

    fn generate_function(&self, func: &definition::FuncDecl, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
        };

        program.statements.push(func);
    }

    fn generate_instruction(&self, stmt: &definition::Instruction, instructions: &mut Vec<assembly::Instruction>) {
        match stmt {
            definition::Instruction::Return(ref val) => {
                // move the value to r1
                instructions.push(assembly::Instruction::Mov(
                    self.convert_val(val),
                    assembly::Operand::Register(assembly::Register::new("r1".to_string()))
                ));
                instructions.push(assembly::Instruction::Return);
            },
            definition::Instruction::Unary(ref op, ref src, ref dst) => {
                instructions.push(assembly::Instruction::Unary(
                    self.convert_unop(op),
                    self.convert_val(src),
                    self.convert_val(dst)
                ));
            },
        }
    }

    fn convert_val(&self, val: &definition::Val) -> assembly::Operand {
        match val {
            definition::Val::Const(i) => assembly::Operand::Immediate(*i),
            definition::Val::Var(ref s) => assembly::Operand::Pseudo(s.clone()),
        }
    }

    fn convert_unop(&self, op: &definition::Unop) -> assembly::Unop {
        match op {
            definition::Unop::Negate => assembly::Unop::Negate,
            definition::Unop::BitwiseNot => assembly::Unop::BitwiseNot,
        }
    }
}