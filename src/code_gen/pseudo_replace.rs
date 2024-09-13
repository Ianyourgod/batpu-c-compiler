use std::collections::HashMap;

use crate::parser::nodes::{self, SymbolTable};
use crate::code_gen::assembly;

pub struct PseudoReplacePass {
    program: assembly::Program,
    symbol_table: HashMap<String, i16>,
    type_table: SymbolTable,
}

struct Context {
    pub stack_offset: i16,
}

impl PseudoReplacePass {
    pub fn new(program: assembly::Program, type_table: SymbolTable) -> PseudoReplacePass {
        PseudoReplacePass {
            program,
            symbol_table: HashMap::new(),
            type_table: type_table,
        }
    }

    pub fn generate(&mut self) -> assembly::Program {

        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        assembly.statements.reserve(self.program.statements.len());

        for stmt in self.program.statements.clone() {
            let stmt = match stmt {
                assembly::TopLevel::FuncDef(func) => func,
                assembly::TopLevel::StaticVariable(name, global, init) => {
                    assembly.statements.push(assembly::TopLevel::StaticVariable(name, global, init));
                    continue;
                },
            };

            self.generate_function(&stmt, &mut assembly);
        }

        assembly
    }

    fn generate_function(&mut self, func: &assembly::FuncDecl, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        let mut context = Context { stack_offset: 0 };

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs, &mut context);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
            stack_size: context.stack_offset,
            global: func.global,
        };

        program.statements.push(assembly::TopLevel::FuncDef(func));
    }

    fn generate_instruction(&mut self, stmt: &assembly::Instruction, instructions: &mut Vec<assembly::Instruction>, context: &mut Context) {
        match stmt {
            assembly::Instruction::Comment(_) => instructions.push(stmt.clone()),

            assembly::Instruction::Unary(ref op, ref src, ref dst) => {
                let src = self.emit_operand(src, context, instructions);
                let dst = self.emit_operand(dst, context, instructions);

                instructions.push(assembly::Instruction::Unary(op.clone(), src, dst));
            },
            assembly::Instruction::Binary(ref op, ref src1, ref src2, ref dst) => {
                let src1 = self.emit_operand(src1, context, instructions);
                let src2 = self.emit_operand(src2, context, instructions);
                let dst = self.emit_operand(dst, context, instructions);

                instructions.push(assembly::Instruction::Binary(op.clone(), src1, src2, dst));
            },
            assembly::Instruction::Mov(ref src, ref dst) => {
                let src = self.emit_operand(src, context, instructions);
                let dst = self.emit_operand(dst, context, instructions);

                instructions.push(assembly::Instruction::Mov(src, dst));
            },
            assembly::Instruction::Ldi(ref dst, ref imm) => {
                let dst = self.emit_operand(dst, context, instructions);

                instructions.push(assembly::Instruction::Ldi(dst, imm.clone()));
            },
            assembly::Instruction::Adi(ref src, ref imm) => {
                let src = self.emit_operand(src, context, instructions);

                instructions.push(assembly::Instruction::Adi(src, imm.clone()));
            },
            assembly::Instruction::Cmp(ref src1, ref src2) => {
                let src1 = self.emit_operand(src1, context, instructions);
                let src2 = self.emit_operand(src2, context, instructions);

                instructions.push(assembly::Instruction::Cmp(src1, src2));
            }
            assembly::Instruction::Lea(ref src, ref dst) => {
                let src = self.emit_operand(src, context, instructions);
                let dst = self.emit_operand(dst, context, instructions);

                instructions.push(assembly::Instruction::Lea(src, dst));
            },
            assembly::Instruction::Lod(ref src, ref offset, ref dst) => {
                let dst = self.emit_operand(dst, context, instructions);

                instructions.push(assembly::Instruction::Lod(src.clone(), offset.clone(), dst));
            },
            assembly::Instruction::Str(ref src, ref offset, ref dst) => {
                let src = self.emit_operand(src, context, instructions);

                instructions.push(assembly::Instruction::Str(src, offset.clone(), dst.clone()));
            },
            assembly::Instruction::Return |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Call(_, _) |
            assembly::Instruction::Label(_) => instructions.push(stmt.clone()),
        }
    }

    fn emit_operand(&mut self, operand: &assembly::Operand, context: &mut Context, instructions: &mut Vec<assembly::Instruction>) -> assembly::Operand {
        match operand {
            assembly::Operand::Pseudo(ref ident, ref ty) => {
                let is_static = self.type_table.contains(&ident) && matches!(self.type_table.lookup(&ident).unwrap().1, nodes::TableEntry::StaticAttr(_, _));
                if is_static {
                    return assembly::Operand::Data(ident.clone());
                }

                if self.symbol_table.contains_key(ident) {
                    instructions.push(assembly::Instruction::Comment(format!("Using var {}, at index {}", ident, *self.symbol_table.get(ident).unwrap())));

                    return assembly::Operand::Memory(
                        assembly::Register { name: "r15".to_string() },
                        *self.symbol_table.get(ident).unwrap()
                    );
                }

                let so = context.stack_offset;

                context.stack_offset += ty.size();

                instructions.push(assembly::Instruction::Comment(format!("Using var {}, at index {}", ident, so + 1)));

                self.symbol_table.insert(ident.clone(), so + 1);
                assembly::Operand::Memory(
                    assembly::Register { name: "r15".to_string() },
                    so + 1,
                )
            },
            assembly::Operand::PseudoMem(name, offset, ty) => {
                let is_static = self.type_table.contains(&name) && matches!(self.type_table.lookup(&name).unwrap().1, nodes::TableEntry::StaticAttr(_, _));
                if is_static {
                    return assembly::Operand::Data(name.clone());
                }

                if self.symbol_table.contains_key(name) {
                    instructions.push(assembly::Instruction::Comment(format!("Using var {}, at index {}", name, *self.symbol_table.get(name).unwrap())));

                    return assembly::Operand::Memory(
                        assembly::Register { name: "r15".to_string() },
                        *self.symbol_table.get(name).unwrap() + offset
                    );
                }

                let so = context.stack_offset;

                context.stack_offset += ty.size();

                instructions.push(assembly::Instruction::Comment(format!("Using var {}, at index {}", name, so + 1)));

                self.symbol_table.insert(name.clone(), so + 1);
                assembly::Operand::Memory(
                    assembly::Register { name: "r15".to_string() },
                    so + 1,
                )
            },
            _ => operand.clone(),
        }
    }
}