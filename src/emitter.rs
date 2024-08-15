#![allow(dead_code)]

use crate::code_gen::assembly;


pub struct Emitter {
    program: assembly::Program,
}

impl Emitter {
    pub fn new(program: assembly::Program) -> Emitter {
        Emitter {
            program,
        }
    }

    pub fn emit(&self) -> String {
        let mut output = "
ldi r14 239
ldi r15 239
cal .user_func_main
ldi r2 250
str r2 r1 0
hlt
.user_func_mem_read
  lod r1 r1 0
  ret
.user_func_mem_write
  str r1 r2 0
  ret
".to_string();
        for func in &self.program.statements {
            output.push_str(&format!(".user_func_{}\n    str r14 r15 0\n    adi r14 -1\n    mov r14 r15\n", func.name));
            for instr in &func.body {
                output.push_str(&format!("    {}\n", self.emit_instruction(instr)));
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
                "mov r15 r14\n    lod r14 r15 0\n    adi r14 1\n    ret".to_string()
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
            assembly::Instruction::Lod(ref src, i, ref dst) => {
                format!("lod {} {} -{}", self.emit_register(src), self.emit_register(dst), i)
            }
            assembly::Instruction::Str(ref src, i, ref dst) => {
                format!("str {} {} -{}", self.emit_register(src), self.emit_register(dst), i)
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
        }
    }
}