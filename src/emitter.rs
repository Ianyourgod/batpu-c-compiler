#![allow(dead_code)]

use crate::code_gen::assembly;


pub struct Emitter {
    program: assembly::Program,
}

impl Emitter {
    pub fn new(program: assembly::Program) -> Emitter {
        //println!("{:#?}", program);
        Emitter {
            program,
        }
    }

    fn contains_main_function(&self) -> (bool, bool) {
        for tl in &self.program.statements {
            match tl {
                assembly::TopLevel::FuncDef(ref func) => {
                    if func.name == "main" {
                        return (true, func.global);
                    }
                },
                _ => {}
            }
        }
        (false, false)
    }

    pub fn emit(&self) -> String {
        let (contains_main, main_global) = self.contains_main_function();
        let mut output = format!("
ldi r14 239
ldi r15 239
{}{}
ldi r2 250
str r2 r1 0
hlt
.mem_read
  lod r1 r1 0
  ret
.mem_write
  str r1 r2 0
  ret
..mult
  LDI r3 0
  LDI r4 0
  LDI r5 8
  ..MULT_LOOP
    LSH r4 r4
    LSH r3 r3
    BRH NC ..MULT_NOCARRY
    ADI r4 1
      ..MULT_NOCARRY
    LSH r2 r2
    BRH NC ..MULT_NOADD
    ADD r1 r3 r3
    BRH NC ..MULT_NOADD
    ADI r4 1
      ..MULT_NOADD
    ADI r5 -1
    BRH NZ ..MULT_LOOP
  MOV r3 r1
  RET
", if contains_main { "cal .main" } else { "" }, if main_global { ":global" } else { "" });
        for tl in &self.program.statements {
            match tl {
                assembly::TopLevel::FuncDef(ref func) => {
                    if func.global {
                        output.push_str(":global\n");
                    }
                    output.push_str(&format!(".{}\n    str r14 r15 0\n    adi r14 -1\n    mov r14 r15\n", func.name));
                    for instr in &func.body {
                        output.push_str(&format!("    {}\n", self.emit_instruction(instr)));
                    }
                    output.push_str("    ret\n");
                },
                #[allow(unused_variables)]
                assembly::TopLevel::StaticVariable(ref name, global, init) => {
                    // TODO: maybe have it literally malloc the memory since we don't have a von neumann architecture?
                    todo!()
                }
            }
        }

        output
    }

    fn emit_operand(&self, op: &assembly::Operand) -> String {
        match op {
            assembly::Operand::Register(ref reg) => self.emit_register(reg),
            assembly::Operand::Immediate(i) => {
                i.to_string()
            }
            _ => unimplemented!(),
        }
    }

    fn emit_register(&self, reg: &assembly::Register) -> String {
        reg.to_string()
    }

    fn emit_as_register(&self, op: &assembly::Operand) -> String {
        match op {
            assembly::Operand::Register(ref reg) => self.emit_register(reg),
            _ => panic!("Expected register, got {:?}", op),
        }
    }

    fn emit_unop(&self, op: &assembly::Unop, src: &assembly::Operand, dst: &assembly::Operand) -> String {
        match op {
            assembly::Unop::Negate => {
                format!("sub r0 {} {}", self.emit_operand(src), self.emit_operand(dst))
            }
            assembly::Unop::BitwiseNot => {
                let src = self.emit_operand(src);
                format!("nor {} {} {}", src, src, self.emit_operand(dst))
            }
        }
    }

    fn binop(&self, op: &assembly::Binop) -> String {
        match op {
            assembly::Binop::Add => "add",
            assembly::Binop::Subtract => "sub",
            assembly::Binop::Nor => "nor",
        }.to_string()
    }

    fn cond(&self, cond: &assembly::CondCode) -> String {
        match cond {
            assembly::CondCode::Equal => "EQ",
            assembly::CondCode::NotEqual => "NE",
            assembly::CondCode::GreaterThanEqual => "GE",
            assembly::CondCode::LessThan => "LT",
        }.to_string()
    }

    fn emit_instruction(&self, instr: &assembly::Instruction) -> String {
        match instr {
            assembly::Instruction::Mov(ref src, ref dst) => {
                format!("mov {} {}", self.emit_operand(src), self.emit_operand(dst))
            }
            assembly::Instruction::Ldi(ref reg, i) => {
                format!("ldi {} {}", self.emit_operand(reg), i)
            }
            assembly::Instruction::Return => {
                "mov r15 r14\n    lod r14 r15 1\n    adi r14 1\n    ret".to_string()
            }
            assembly::Instruction::Unary(ref op, ref src, ref dst) => {
                self.emit_unop(op, src, dst)
            }
            assembly::Instruction::Binary(ref op, ref src1, ref src2, ref dst) => {
                
                format!("{} {} {} {}",
                        self.binop(op),
                        self.emit_operand(src1),
                        self.emit_operand(src2),
                        self.emit_operand(dst))
            }
            assembly::Instruction::AllocateStack(i) => {
                format!("adi r14 -{}", i)
            }
            assembly::Instruction::Adi(ref reg, i) => {
                format!("adi {} {}", self.emit_operand(reg), i)
            }
            assembly::Instruction::Lod(ref src, i, ref dst) => {
                let ofb_1 = 8 < *i;
                let out_of_bounds = ofb_1 || *i < -7;
                let diff = if ofb_1 { 8 - i } else { 7 + i };
                let mut res = String::new();
                let src = self.emit_register(src);
                if out_of_bounds {
                    // we add the difference to the offset
                    res.push_str(&format!("adi {} {}\n    ", src, diff));
                }
                res.push_str(&format!("lod {} {} {}", src, self.emit_as_register(dst), -i - diff*out_of_bounds as i16));
                if out_of_bounds { res.push_str(&format!("\n    adi {} {}", src, -diff)) }
                res
            }
            assembly::Instruction::Str(ref src, i, ref dst) => {
                let ofb_1 = 8 < *i;
                let out_of_bounds = ofb_1 || *i < -7;
                let diff = if ofb_1 { 8 - i } else { 7 + i };
                let mut res = String::new();
                let dst = self.emit_register(dst);
                if out_of_bounds {
                    // we add the difference to the offset
                    res.push_str(&format!("adi {} {}\n    ", dst, diff));
                }
                res.push_str(&format!("str {} {} {}", dst, self.emit_as_register(src), -i - diff*out_of_bounds as i16));
                if out_of_bounds { res.push_str(&format!("\n    adi {} {}", dst, -diff)) }
                res
            }
            assembly::Instruction::Jmp(lbl) => {
                format!("jmp .{}", lbl)
            }
            assembly::Instruction::JmpCC(cond, lbl) => {
                format!("brh {} .{}", self.cond(cond), lbl)
            }
            assembly::Instruction::Cmp(ref src1, ref src2) => {
                format!("cmp {} {}",
                    self.emit_operand(src1),
                    self.emit_operand(src2))
            }
            assembly::Instruction::Label(ref lbl) => {
                format!(".{}", lbl)
            }
            assembly::Instruction::Call(ref lbl, global) => {
                format!("cal .{}{}", lbl, if *global { ":global" } else { "" })
            }

            assembly::Instruction::Lea(_, _) => unreachable!(),
        }
    }
}