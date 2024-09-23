#![allow(dead_code)]

pub mod assembly;

mod convert;
mod pseudo_replace;
mod instr_fixup;

use crate::parser::nodes::{SymbolTable, TypeTable};
use crate::tacky::definition;

pub fn convert(program: definition::Program, type_table: SymbolTable, struct_table: TypeTable) -> assembly::Program {
    let mut convert_pass = convert::ConvertPass::new(program);

    #[allow(unused_variables)]
    let (program, function_table) = convert_pass.generate();

    //println!("{:#?}", program);

    let mut pseudo_pass = pseudo_replace::PseudoReplacePass::new(program, type_table, struct_table);

    let pseudo_program = pseudo_pass.generate();

    //println!("{:#?}", pseudo_program);

    let instr_fixup_pass = instr_fixup::InstructionFixupPass::new(pseudo_program);
    let fixup_program = instr_fixup_pass.generate();

    //println!("{:#?}", fixup_program);

    fixup_program
}