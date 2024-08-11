use std::collections::HashMap;

use crate::code_gen::assembly;

pub struct PsuedoReplacePass {
    program: assembly::Program,
    stack_offset: i16, // 16 so we can go negative and and still go positive above like 127
    symbol_table: HashMap<String, i16>,
}

impl PsuedoReplacePass {
    pub fn new(program: assembly::Program) -> PsuedoReplacePass {
        PsuedoReplacePass {
            program,
            stack_offset: 0,
            symbol_table: HashMap::new(),
        }
    }

    pub fn generate(&mut self) -> (assembly::Program, i16) {
        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        for stmt in self.program.statements.clone() {
            self.generate_function(&stmt, &mut assembly);
        }

        (assembly, self.stack_offset)
    }

    fn generate_function(&mut self, func: &assembly::FuncDecl, program: &mut assembly::Program) {
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

    fn generate_instruction(&mut self, stmt: &assembly::Instruction, instructions: &mut Vec<assembly::Instruction>) {
        match stmt {
            assembly::Instruction::Unary(ref op, ref src, ref dst) => {
                let src = self.emit_operand(src);
                let dst = self.emit_operand(dst);

                instructions.push(assembly::Instruction::Unary(op.clone(), src, dst));
            },
            assembly::Instruction::Mov(ref src, ref dst) => {
                let src = self.emit_operand(src);
                let dst = self.emit_operand(dst);

                instructions.push(assembly::Instruction::Mov(src, dst));
            },
            assembly::Instruction::Ldi(ref dst, ref imm) => {
                let dst = self.emit_operand(dst);

                instructions.push(assembly::Instruction::Ldi(dst, imm.clone()));
            },
            assembly::Instruction::Return |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Lod(_, _, _) |
            assembly::Instruction::Str(_, _, _) => instructions.push(stmt.clone()),
        }
    }

    fn emit_operand(&mut self, operand: &assembly::Operand) -> assembly::Operand {
        match operand {
            assembly::Operand::Pseudo(ref identifier) => {
                if self.symbol_table.contains_key(identifier) {
                    return assembly::Operand::Stack(*self.symbol_table.get(identifier).unwrap());
                }

                self.stack_offset += 1;

                self.symbol_table.insert(identifier.clone(), self.stack_offset);
                assembly::Operand::Stack(self.stack_offset)
            },
            _ => operand.clone(),
        }
    }
}