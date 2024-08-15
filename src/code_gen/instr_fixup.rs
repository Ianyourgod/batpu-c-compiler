use crate::code_gen::assembly;

pub struct InstructionFixupPass {
    program: assembly::Program,
    stack_offset: i16,
}

impl InstructionFixupPass {
    pub fn new(program: assembly::Program, stack_offset: i16) -> InstructionFixupPass {
        InstructionFixupPass {
            program,
            stack_offset,
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

    fn generate_function(&self, func: &assembly::FuncDecl, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        instrs.push(assembly::Instruction::AllocateStack(self.stack_offset as u8));

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
        };

        program.statements.push(func);
    }

    fn is_register(&self, op: &assembly::Operand) -> (bool, assembly::Register) {
        match op {
            assembly::Operand::Register(reg) => (true, reg.clone()),
            _ => (false, assembly::Register::new("r0".to_string())),
        }
    }

    fn to_register(&self, op: &assembly::Operand, reg: &assembly::Register, instructions: &mut Vec<assembly::Instruction>) {
        match op {
            assembly::Operand::Register(_) | assembly::Operand::Pseudo(_) => unreachable!(),
            assembly::Operand::Immediate(val) => {
                instructions.push(assembly::Instruction::Ldi(
                    assembly::Operand::Register(reg.clone()),
                    val.clone()
                ));
            },
            assembly::Operand::Stack(offset) => {
                instructions.push(assembly::Instruction::Lod(
                    assembly::Register::new("rbp".to_string()),
                    *offset,
                    reg.clone()
                ));
            },
        }
    }

    fn is_stack(&self, op: &assembly::Operand) -> (bool, i16) {
        match op {
            assembly::Operand::Stack(offset) => (true, *offset),
            _ => (false, 0),
        }
    }

    fn generate_instruction(&self, stmt: &assembly::Instruction, instructions: &mut Vec<assembly::Instruction>) {
        match stmt {
            assembly::Instruction::Mov(ref src, ref dst) => {
                let (is_src_reg, src_reg) = self.is_register(src);
                let (is_dst_reg, dst_reg) = self.is_register(dst);

                if is_src_reg && is_dst_reg {
                    instructions.push(stmt.clone())
                } else {
                    let src_reg = if is_src_reg { src_reg } else { assembly::Register::new("r10".to_string()) };
                    let dst_reg = if is_dst_reg { dst_reg } else { assembly::Register::new("r11".to_string()) }; 

                    if !is_src_reg { self.to_register(src, &src_reg, instructions) };

                    instructions.push(assembly::Instruction::Mov(
                        assembly::Operand::Register(src_reg),
                        assembly::Operand::Register(dst_reg.clone())
                    ));

                    if is_dst_reg {
                        return;
                    }

                    let (is_stack, offset) = self.is_stack(dst);

                    if is_stack {
                        instructions.push(assembly::Instruction::Str(
                            assembly::Register::new("rbp".to_string()),
                            offset,
                            dst_reg,
                        ));
                    } else {
                        panic!("Invalid destination operand: {:?}", dst);
                    }
                }
            },
            assembly::Instruction::Unary(ref op, ref src, ref dst) => {
                let (is_src_reg, src_reg) = self.is_register(src);
                let (is_dst_reg, dst_reg) = self.is_register(dst);

                if is_src_reg && is_dst_reg {
                    instructions.push(stmt.clone())
                } else {
                    let src_reg = if is_src_reg { src_reg } else { assembly::Register::new("r10".to_string()) };
                    let dst_reg = if is_dst_reg { dst_reg } else { assembly::Register::new("r11".to_string()) }; 

                    if !is_src_reg { self.to_register(src, &src_reg, instructions) };

                    instructions.push(assembly::Instruction::Unary(
                        op.clone(),
                        assembly::Operand::Register(src_reg),
                        assembly::Operand::Register(dst_reg.clone())
                    ));

                    if is_dst_reg {
                        return;
                    }

                    let (is_stack, offset) = self.is_stack(dst);

                    if is_stack {
                        instructions.push(assembly::Instruction::Str(
                            assembly::Register::new("rbp".to_string()),
                            offset,
                            dst_reg,
                        ));
                    } else {
                        panic!("Invalid destination operand: {:?}", dst);
                    }
                }
            },
            assembly::Instruction::Binary(ref op, ref src1, ref src2, ref dst) => {
                let (is_src1_reg, src1_reg) = self.is_register(src1);
                let (is_src2_reg, src2_reg) = self.is_register(src2);
                let (is_dst_reg, dst_reg) = self.is_register(dst);

                if is_src1_reg && is_src2_reg && is_dst_reg {
                    instructions.push(stmt.clone())
                } else {
                    let src1_reg = if is_src1_reg { src1_reg } else { assembly::Register::new("r10".to_string()) };
                    let src2_reg = if is_src2_reg { src2_reg } else { assembly::Register::new("r11".to_string()) };
                    let dst_reg = if is_dst_reg { dst_reg } else { assembly::Register::new("r12".to_string()) }; 

                    if !is_src1_reg { self.to_register(src1, &src1_reg, instructions) };
                    if !is_src2_reg { self.to_register(src2, &src2_reg, instructions) };

                    instructions.push(assembly::Instruction::Binary(
                        op.clone(),
                        assembly::Operand::Register(src1_reg),
                        assembly::Operand::Register(src2_reg),
                        assembly::Operand::Register(dst_reg.clone())
                    ));

                    if is_dst_reg {
                        return;
                    }

                    let (is_stack, offset) = self.is_stack(dst);

                    if is_stack {
                        instructions.push(assembly::Instruction::Str(
                            assembly::Register::new("rbp".to_string()),
                            offset,
                            dst_reg,
                        ));
                    } else {
                        panic!("Invalid destination operand: {:?}", dst);
                    }
                }
            },
            assembly::Instruction::Cmp(ref lft, ref rht) => {
                let (is_src1_reg, src1_reg) = self.is_register(lft);
                let (is_src2_reg, src2_reg) = self.is_register(rht);

                if is_src1_reg && is_src2_reg {
                    instructions.push(stmt.clone());
                    return;
                }

                let src1_reg = if is_src1_reg { src1_reg } else { assembly::Register::new("r10".to_string()) };
                let src2_reg = if is_src2_reg { src2_reg } else { assembly::Register::new("r11".to_string()) };

                if !is_src1_reg { self.to_register(lft, &src1_reg, instructions) };
                if !is_src2_reg { self.to_register(rht, &src2_reg, instructions) };

                instructions.push(assembly::Instruction::Cmp(
                    assembly::Operand::Register(src1_reg),
                    assembly::Operand::Register(src2_reg),
                ));
            }
            assembly::Instruction::Ldi(_, _) |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Lod(_, _, _) |
            assembly::Instruction::Str(_, _, _) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Return => instructions.push(stmt.clone()),
            //d_ => instructions.push(stmt.clone()),
        }
    }
}