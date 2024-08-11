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
hlt
.user_func_mem_read
  lod r1 r1 0
  ret
.user_func_mem_write
  str r1 r2 0
  ret
".to_string();
        for func in &self.program.statements {
            output.push_str(&format!(".user_func_{}\n", func.name));
            for instr in &func.body {
                output.push_str(&format!("    {}\n", self.emit_instruction(instr)));
            }
        }

        output
    }

    fn emit_instruction(&self, instr: &assembly::Instruction) -> String {
        match instr {
            assembly::Instruction::Ldi(ref reg, i) => {
                format!("ldi {} {}", reg.name, i)
            }
            assembly::Instruction::Return => {
                "ret".to_string()
            }
        }
    }
}