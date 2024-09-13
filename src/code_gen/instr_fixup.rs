use std::collections::HashMap;

use crate::code_gen::assembly;

pub struct InstructionFixupPass {
    program: assembly::Program,
    static_table: HashMap<String, u8>,
    static_counter: u8,
}

impl InstructionFixupPass {
    pub fn new(program: assembly::Program) -> InstructionFixupPass {
        InstructionFixupPass {
            program,
            static_table: HashMap::new(),
            static_counter: 0,
        }
    }

    pub fn generate(&self) -> assembly::Program {

        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        for stmt in &self.program.statements {
            let stmt = match stmt {
                assembly::TopLevel::FuncDef(func) => func,
                assembly::TopLevel::StaticVariable(name, global, init) => {
                    assembly.statements.push(assembly::TopLevel::StaticVariable(name.clone(), *global, init.clone()));
                    continue;
                },
            };

            self.generate_function(stmt, &mut assembly);
        }

        assembly
    }

    fn generate_function(&self, func: &assembly::FuncDecl, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        instrs.push(assembly::Instruction::AllocateStack(func.stack_size as u8));

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        instrs.push(assembly::Instruction::AllocateStack(-func.stack_size as u8));

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
            stack_size: func.stack_size,
            global: func.global,
        };

        program.statements.push(assembly::TopLevel::FuncDef(func));
    }

    fn is_register(&self, op: &assembly::Operand) -> (bool, assembly::Register) {
        match op {
            assembly::Operand::Register(reg) => (true, reg.clone()),
            _ => (false, assembly::Register::new("r0".to_string())),
        }
    }

    fn to_register(&self, op: &assembly::Operand, reg: &assembly::Register, instructions: &mut Vec<assembly::Instruction>) {
        match op {
            assembly::Operand::Register(_) | assembly::Operand::Pseudo(_, _) | assembly::Operand::PseudoMem(_, _, _) => unreachable!(),
            assembly::Operand::Immediate(val) => {
                instructions.push(assembly::Instruction::Ldi(
                    assembly::Operand::Register(reg.clone()),
                    val.clone()
                ));
            },
            assembly::Operand::Memory(base_reg, offset) => {
                instructions.push(assembly::Instruction::Lod(
                    base_reg.clone(),
                    *offset,
                    assembly::Operand::Register(reg.clone())
                ));
            },
            assembly::Operand::Data(name) => {
                let static_offset = self.static_table.get(name).unwrap();
                instructions.push(assembly::Instruction::Lod(
                    assembly::Register::new("r0".to_string()),
                    *static_offset as i16,
                    assembly::Operand::Register(reg.clone())
                ));
            }
        }
    }

    fn is_mem(&self, op: &assembly::Operand) -> (bool, assembly::Register, i16) {
        match op {
            assembly::Operand::Memory(reg, offset) => (true, reg.clone(), *offset),
            _ => (false, assembly::Register::new("r0".to_string()), 0),
        }
    }

    fn generate_instruction(&self, stmt: &assembly::Instruction, instructions: &mut Vec<assembly::Instruction>) {
        match stmt {
            assembly::Instruction::Comment(_) => instructions.push(stmt.clone()),

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

                    let (is_mem, reg, offset) = self.is_mem(dst);

                    if is_mem {
                        instructions.push(assembly::Instruction::Str(
                            assembly::Operand::Register(dst_reg),
                            offset,
                            reg,
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

                    let (is_mem, reg, offset) = self.is_mem(dst);

                    if is_mem {
                        instructions.push(assembly::Instruction::Str(
                            assembly::Operand::Register(dst_reg),
                            offset,
                            reg,
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

                    let (is_mem, reg, offset) = self.is_mem(dst);

                    if is_mem {
                        instructions.push(assembly::Instruction::Str(
                            assembly::Operand::Register(dst_reg),
                            offset,
                            reg,
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
            assembly::Instruction::Adi(ref op, i) => {
                let (is_mem, base_reg, offset) = self.is_mem(op);

                if !is_mem {
                    instructions.push(stmt.clone());
                    return;
                }

                let small_reg = assembly::Register::new("r10".to_string());

                self.to_register(op, &small_reg, instructions);

                let reg = assembly::Operand::Register(small_reg);



                instructions.push(assembly::Instruction::Adi(
                    reg.clone(),
                    i.clone()
                ));

                instructions.push(assembly::Instruction::Str(reg, offset, base_reg));
            },
            assembly::Instruction::Lod(reg, off, dst) => {
                // if dst is not a register, we need to put it in a temporary register then store it in dst
                let (is_dst_reg, _) = self.is_register(dst);
                
                if is_dst_reg {
                    instructions.push(stmt.clone());
                    return;
                }

                let dst_reg = assembly::Register::new("r10".to_string());

                instructions.push(assembly::Instruction::Lod(reg.clone(), *off, assembly::Operand::Register(dst_reg.clone())));

                let (is_mem, base_reg, offset) = self.is_mem(dst);

                if is_mem {
                    instructions.push(assembly::Instruction::Str(assembly::Operand::Register(dst_reg), offset, base_reg));
                } else {
                    panic!("Invalid destination operand: {:?}", dst);
                }
            }
            assembly::Instruction::Str(ref src, ref off, ref dst) => {
                // if dst is not a register, we need to put it in a temporary register then store it in dst
                let (is_src_reg, _) = self.is_register(src);
                
                if is_src_reg {
                    instructions.push(stmt.clone());
                    return;
                }

                let src_reg = assembly::Register::new("r10".to_string());

                self.to_register(src, &src_reg, instructions);

                instructions.push(assembly::Instruction::Str(assembly::Operand::Register(src_reg), *off, dst.clone()));
            }
            assembly::Instruction::Lea(ref src, ref dst) => {
                // alright, this is a "fake" instruction
                // we need to get the address of src and store it in dst
                // we do this by assuming src is a memory operand, then we add the offset to the base register
                // then we store the result in dst
                // if dst is not a register, we need to put it in a temporary register then store it in dst
                // (also, we need to remove the offset when done)
                let (is_dst_reg, _) = self.is_register(dst);

                let src = match src {
                    assembly::Operand::Memory(base_reg, offset) => (base_reg.clone(), *offset),
                    _ => panic!("Invalid source operand: {:?}", src),
                };

                instructions.push(assembly::Instruction::Adi(
                    assembly::Operand::Register(src.0.clone()),
                    -src.1 as i8,
                ));

                if is_dst_reg {
                    instructions.push(assembly::Instruction::Mov(
                        assembly::Operand::Register(src.0.clone()),
                        dst.clone(),
                    ));
                    instructions.push(assembly::Instruction::Adi(
                        assembly::Operand::Register(src.0),
                        src.1 as i8,
                    ));
                    return;
                }

                let dst_reg = assembly::Register::new("r10".to_string());
                
                instructions.push(assembly::Instruction::Mov(
                    assembly::Operand::Register(src.0.clone()),
                    assembly::Operand::Register(dst_reg.clone()),
                ));

                instructions.push(assembly::Instruction::Adi(
                    assembly::Operand::Register(src.0),
                    src.1 as i8,
                ));

                let (is_mem, base_reg, offset) = self.is_mem(dst);

                if is_mem {
                    instructions.push(assembly::Instruction::Str(assembly::Operand::Register(dst_reg), offset, base_reg));
                } else {
                    panic!("Invalid destination operand: {:?}", dst);
                }
            }
            assembly::Instruction::Ldi(_, _) |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Call(_, _) |
            assembly::Instruction::Return => instructions.push(stmt.clone()),
            //d_ => instructions.push(stmt.clone()),
        }
    }
}