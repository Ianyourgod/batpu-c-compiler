#![allow(dead_code)]

pub mod assembly;

use crate::parser::nodes;

pub struct CodeGen {
    program: nodes::Program,
}

impl CodeGen {
    pub fn new(program: nodes::Program) -> CodeGen {
        CodeGen {
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

    fn generate_function(&self, func: &nodes::FuncDecl, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        self.generate_instruction(&func.body, &mut instrs);

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
        };

        program.statements.push(func);
    }

    fn generate_instruction(&self, stmt: &nodes::Statement, instructions: &mut Vec<assembly::Instruction>) {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                self.generate_expression(expr, instructions);
                instructions.push(assembly::Instruction::Return);
            }
        }
    }

    fn generate_expression(&self, expr: &nodes::Expression, instructions: &mut Vec<assembly::Instruction>) {
        match expr {
            nodes::Expression::IntegerLiteral(i) => {
                instructions.push(assembly::Instruction::Ldi(assembly::Register::new("r1".to_string()), *i));
            }
        }
    }
}