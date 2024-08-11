#![allow(dead_code)]

pub mod assembly;

mod convert;
mod pseudo_replace;
mod instr_fixup;

use crate::tacky::definition;

pub fn convert(program: definition::Program) -> assembly::Program {
    let convert_pass = convert::ConvertPass::new(program);
    let mut pseudo_pass = pseudo_replace::PsuedoReplacePass::new(convert_pass.generate());
    let (pseudo_program, stack_offset) = pseudo_pass.generate();
    let instr_fixup_pass = instr_fixup::InstructionFixupPass::new(pseudo_program, stack_offset);
    instr_fixup_pass.generate()
}