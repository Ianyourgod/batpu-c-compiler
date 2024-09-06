#![allow(dead_code)]

pub mod assembly;

mod convert;
mod pseudo_replace;
mod instr_fixup;

use crate::parser::nodes::SymbolTable;
use crate::tacky::definition;

pub fn convert(program: definition::Program, type_table: SymbolTable) -> assembly::Program {
    let mut convert_pass = convert::ConvertPass::new(program);
    let mut pseudo_pass = pseudo_replace::PsuedoReplacePass::new(convert_pass.generate(), type_table);
    let pseudo_program = pseudo_pass.generate();
    let instr_fixup_pass = instr_fixup::InstructionFixupPass::new(pseudo_program);
    instr_fixup_pass.generate()
}