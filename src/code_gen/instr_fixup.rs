use std::collections::HashMap;

use crate::{code_gen::assembly, errors};

pub struct InstructionFixupPass {
    program: assembly::Program,
    static_table: HashMap<String, u8>,
    static_counter: u8,
    callee_saved: HashMap<String, Vec<assembly::Register>>,
    current_callee_saved_regs: (Vec<(assembly::Operand, i16)>, i16),
}

impl InstructionFixupPass {
    pub fn new(program: assembly::Program, callee_saved: HashMap<String, Vec<assembly::Register>>) -> InstructionFixupPass {
        InstructionFixupPass {
            program,
            static_table: HashMap::new(),
            static_counter: 0,
            callee_saved,
            current_callee_saved_regs: (vec![], 0),
        }
    }

    pub fn generate(&mut self, source: (&String, &String)) -> assembly::Program {
        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        for stmt in self.program.statements.clone() {
            let stmt = match stmt {
                assembly::TopLevel::FuncDef(func) => func,
                assembly::TopLevel::StaticVariable(name, global, init) => {
                    assembly.statements.push(assembly::TopLevel::StaticVariable(name, global, init));
                    continue;
                },
            };

            let callee_saved = if self.callee_saved.is_empty() {
                vec![]
            } else { self.callee_saved.get(&stmt.name).unwrap().clone() };

            self.generate_function(&stmt, &mut assembly, &callee_saved, source);
        }

        assembly
    }

    fn generate_function(&mut self, func: &assembly::FuncDecl, program: &mut assembly::Program, callee_saved: &Vec<assembly::Register>, source: (&String, &String)) {
        let mut instrs: Vec<assembly::Instruction> = Vec::with_capacity(func.body.len());

        if func.stack_size > 239 {
            let line = source.1.lines().nth(func.line-1).unwrap();

            // TODO: go through function body, find calls, do recursive shit to find the max stack size

            errors::inline_warn((func.line, 0), "Stack size is too large. Maximum stack size is 239 bytes.", line, source.0);
        }

        instrs.push(assembly::Instruction::AllocateStack(func.stack_size as u8));

        let mut added_to_sp = 0;
        let mut extra_added_to_sp = 0;
        let r14 = assembly::Register::new(String::from("r14"));

        let mut saved_at = vec![];

        for (i, reg) in callee_saved.iter().enumerate() {
            // push reg
            let mod_i = i as i16 % 8;
            added_to_sp += 1;
            let added_extra = mod_i == 0 && i > 0;
            if added_extra {
                extra_added_to_sp += 8;
                instrs.push(assembly::Instruction::Adi(assembly::Operand::Register(r14.clone()), -8));
            }

            saved_at.push((assembly::Operand::Register(reg.clone()), i as i16));

            instrs.push(assembly::Instruction::Str(assembly::Operand::Register(reg.clone()), mod_i, r14.clone()));
        }

        self.current_callee_saved_regs = (saved_at.clone(), func.stack_size);

        if extra_added_to_sp != added_to_sp {
            instrs.push(assembly::Instruction::Adi(assembly::Operand::Register(r14.clone()), extra_added_to_sp-added_to_sp));
        }

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
            stack_size: func.stack_size,
            global: func.global,
            defined: func.defined,
            line: func.line,
        };

        program.statements.push(assembly::TopLevel::FuncDef(func));
    }

    fn is_register(&self, op: &assembly::Operand) -> (bool, assembly::Register) {
        match op {
            assembly::Operand::Register(reg) => (true, reg.clone()),
            _ => (false, assembly::Register::new("r0".to_string())),
        }
    }

    fn to_register(&self, op: &assembly::Operand, reg: &assembly::Register, instructions: &mut Vec<assembly::Instruction>) -> assembly::Register {
        match op {
            assembly::Operand::Register(_) | assembly::Operand::Pseudo(_, _) | assembly::Operand::PseudoMem(_, _, _) => unreachable!(),
            assembly::Operand::Immediate(val) => {
                if *val == 0 {
                    return assembly::Register::new("r0".to_string());
                }

                instructions.push(assembly::Instruction::Ldi(
                    assembly::Operand::Register(reg.clone()),
                    val.clone()
                ));
                return reg.clone();
            },
            assembly::Operand::Memory(base_reg, offset) => {
                instructions.push(assembly::Instruction::Lod(
                    base_reg.clone(),
                    *offset,
                    assembly::Operand::Register(reg.clone())
                ));
                return reg.clone();
            },
            assembly::Operand::Data(name) => {
                let static_offset = self.static_table.get(name).unwrap();
                instructions.push(assembly::Instruction::Lod(
                    assembly::Register::new("r0".to_string()),
                    *static_offset as i16,
                    assembly::Operand::Register(reg.clone())
                ));
                return reg.clone();
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
                match dst {
                    assembly::Operand::Register(dst_reg) => {
                        match src {
                            assembly::Operand::Register(src_reg) => {
                                instructions.push(assembly::Instruction::Mov(
                                    assembly::Operand::Register(src_reg.clone()),
                                    assembly::Operand::Register(dst_reg.clone()),
                                ));
                            },
                            assembly::Operand::Immediate(imm) => {
                                instructions.push(assembly::Instruction::Ldi(
                                    assembly::Operand::Register(dst_reg.clone()),
                                    imm.clone(),
                                ));
                            }
                            assembly::Operand::Memory(base_reg, off) => {
                                instructions.push(assembly::Instruction::Lod(
                                    base_reg.clone(),
                                    *off,
                                    assembly::Operand::Register(dst_reg.clone()),
                                ));
                            }

                            assembly::Operand::Data(_) => unimplemented!(),
                            assembly::Operand::Pseudo(_, _) | assembly::Operand::PseudoMem(_, _, _) => unreachable!("pseudo should have been removed by now"),
                        }
                    },
                    assembly::Operand::Memory(dst_reg, offset) => {
                        match src {
                            assembly::Operand::Register(src_reg) => {
                                instructions.push(assembly::Instruction::Str(
                                    assembly::Operand::Register(src_reg.clone()),
                                    *offset,
                                    dst_reg.clone(),
                                ));
                            },
                            assembly::Operand::Immediate(imm) => {
                                instructions.push(assembly::Instruction::Ldi(
                                    assembly::Operand::Register(assembly::Register::new("r10".to_string())),
                                    imm.clone(),
                                ));
                                instructions.push(assembly::Instruction::Str(
                                    assembly::Operand::Register(assembly::Register::new("r10".to_string())),
                                    *offset,
                                    dst_reg.clone(),
                                ));
                            }
                            assembly::Operand::Memory(base_reg, off) => {
                                instructions.push(assembly::Instruction::Lod(
                                    base_reg.clone(),
                                    *off,
                                    assembly::Operand::Register(assembly::Register::new("r10".to_string())),
                                ));
                                instructions.push(assembly::Instruction::Str(
                                    assembly::Operand::Register(assembly::Register::new("r10".to_string())),
                                    *offset,
                                    dst_reg.clone(),
                                ));
                            }

                            assembly::Operand::Data(_) => unimplemented!(),
                            assembly::Operand::Pseudo(_, _) | assembly::Operand::PseudoMem(_, _, _) => unreachable!("pseudo should have been removed by now"),
                        }
                    }

                    assembly::Operand::Data(_) => unimplemented!(),
                    assembly::Operand::Immediate(_) => unreachable!("INTERNAL ERROR. PLEASE REPORT: cant mov to immediate"),
                    assembly::Operand::Pseudo(_, _) | assembly::Operand::PseudoMem(_, _, _) => unreachable!("pseudo should have been removed by now"),
                }
            },
            assembly::Instruction::Unary(ref op, ref src, ref dst) => {
                let (is_src_reg, src_reg) = self.is_register(src);
                let (is_dst_reg, dst_reg) = self.is_register(dst);

                if is_src_reg && is_dst_reg {
                    instructions.push(stmt.clone())
                } else {
                    let mut src_reg = if is_src_reg { src_reg } else { assembly::Register::new("r10".to_string()) };
                    let dst_reg = if is_dst_reg { dst_reg } else { assembly::Register::new("r11".to_string()) }; 

                    src_reg = if !is_src_reg { self.to_register(src, &src_reg, instructions) } else { src_reg };

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
                        unreachable!("INTERNAL ERROR. PLEASE REPORT: Invalid destination operand: {:?}", dst);
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
                    let is_adi_op = match op {
                        assembly::Binop::Add => (true, true),
                        assembly::Binop::Subtract => (true, false),
                        _ => (false, false)
                    };
                    let is_imm_and_reg = match (src1, src2) {
                        (assembly::Operand::Register(r), assembly::Operand::Immediate(v)) |
                        (assembly::Operand::Immediate(v), assembly::Operand::Register(r)) => (true, assembly::Operand::Register(r.clone()), *v),
                        _ => (false, assembly::Operand::Immediate(0), 0)
                    };

                    if is_adi_op.1 && is_imm_and_reg.0 && is_imm_and_reg.2 == 0 {
                        // yay we can do mov!!
                        instructions.push(assembly::Instruction::Mov(
                            is_imm_and_reg.1.clone(),
                            dst.clone(),
                        ));
                        return;
                    }

                    if is_adi_op.0 && is_imm_and_reg.0 && *dst == is_imm_and_reg.1 {
                        // yay we can do adi!!
                        let imm = if is_adi_op.1 { is_imm_and_reg.2 } else { -is_imm_and_reg.2 };

                        if imm == 0 {
                            return;
                        }

                        instructions.push(assembly::Instruction::Adi(
                            is_imm_and_reg.1.clone(),
                            imm,
                        ));
                        return;
                    }

                    let mut src1_reg = if is_src1_reg { src1_reg } else { assembly::Register::new("r10".to_string()) };
                    let mut src2_reg = if is_src2_reg { src2_reg } else { assembly::Register::new("r11".to_string()) };
                    let dst_reg = if is_dst_reg { dst_reg } else { assembly::Register::new("r11".to_string()) }; 

                    if !is_src1_reg { src1_reg = self.to_register(src1, &src1_reg, instructions) };
                    if !is_src2_reg { src2_reg = self.to_register(src2, &src2_reg, instructions) };

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
                        unreachable!("INTERNAL ERROR. PLEASE REPORT: Invalid destination operand: {:?}", dst);
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

                let mut src1_reg = if is_src1_reg { src1_reg } else { assembly::Register::new("r10".to_string()) };
                let mut src2_reg = if is_src2_reg { src2_reg } else { assembly::Register::new("r11".to_string()) };

                if !is_src1_reg { src1_reg = self.to_register(lft, &src1_reg, instructions) };
                if !is_src2_reg { src2_reg = self.to_register(rht, &src2_reg, instructions) };

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
                    unreachable!("INTERNAL ERROR. PLEASE REPORT: Invalid destination operand: {:?}", dst);
                }
            }
            assembly::Instruction::Str(ref src, ref off, ref dst) => {
                // if dst is not a register, we need to put it in a temporary register then store it in dst
                let (is_src_reg, _) = self.is_register(src);
                
                if is_src_reg {
                    instructions.push(stmt.clone());
                    return;
                }

                let mut src_reg = assembly::Register::new("r10".to_string());

                src_reg = self.to_register(src, &src_reg, instructions);

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
                    _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Invalid source operand: {:?}", src),
                };

                instructions.push(assembly::Instruction::Adi(
                    assembly::Operand::Register(src.0.clone()),
                    -src.1 as i16,
                ));

                if is_dst_reg {
                    instructions.push(assembly::Instruction::Mov(
                        assembly::Operand::Register(src.0.clone()),
                        dst.clone(),
                    ));
                    instructions.push(assembly::Instruction::Adi(
                        assembly::Operand::Register(src.0),
                        src.1 as i16,
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
                    src.1 as i16,
                ));

                let (is_mem, base_reg, offset) = self.is_mem(dst);

                if is_mem {
                    instructions.push(assembly::Instruction::Str(assembly::Operand::Register(dst_reg), offset, base_reg));
                } else {
                    unreachable!("INTERNAL ERROR. PLEASE REPORT: Invalid destination operand: {:?}", dst);
                }
            },
            assembly::Instruction::Return => {
                let mut added_to_sp = 0;
                let r14 = assembly::Register::new(String::from("r14"));

                instructions.push(assembly::Instruction::Adi(assembly::Operand::Register(r14.clone()), self.current_callee_saved_regs.0.len() as i16));

                for (i, reg) in self.current_callee_saved_regs.0.iter().enumerate() {
                    // push reg
                    let mod_i = i as i16 % 8;
                    let added_extra = mod_i == 0 && i > 0;
                    if added_extra {
                        added_to_sp += 8;
                        instructions.push(assembly::Instruction::Adi(assembly::Operand::Register(r14.clone()), -8));
                    }
        
                    instructions.push(assembly::Instruction::Lod(r14.clone(), mod_i, reg.0.clone()));
                }

                if added_to_sp != 0 {
                    instructions.push(assembly::Instruction::Adi(assembly::Operand::Register(r14.clone()), added_to_sp));
                }

                instructions.push(assembly::Instruction::Adi(assembly::Operand::Register(r14), self.current_callee_saved_regs.1));

                // mov r15 r14\n    lod r14 r15 1\n    adi r14 1\n    

                instructions.push(assembly::Instruction::Mov(
                    assembly::Operand::Register(assembly::Register::new(String::from("r15"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r14")))
                ));
                instructions.push(assembly::Instruction::Lod(
                    assembly::Register::new(String::from("r14")),
                    -1,
                    assembly::Operand::Register(assembly::Register::new(String::from("r15"))),
                ));
                instructions.push(assembly::Instruction::Adi(
                    assembly::Operand::Register(assembly::Register::new(String::from("r14"))),
                    1
                ));

                instructions.push(assembly::Instruction::Return);
            }

            assembly::Instruction::Ldi(_, _) |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Call(_, _) => instructions.push(stmt.clone()),
            //d_ => instructions.push(stmt.clone()),
        }
    }
}