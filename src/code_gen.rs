#![allow(dead_code)]

pub mod assembly;

mod convert;
mod register_allocation;
mod pseudo_replace;
mod instr_fixup;

use std::collections::HashMap;

use crate::parser::nodes::{SymbolTable, TypeTable};
use crate::tacky::definition;

pub fn convert(program: definition::Program, type_table: SymbolTable, struct_table: TypeTable, aliased: HashMap<String, Vec<String>>, optimize: bool) -> assembly::Program {
    let mut convert_pass = convert::ConvertPass::new(program);

    #[allow(unused_variables)]
    let (program, function_table) = convert_pass.generate();

    //println!("{:#?}", program.statements.get(7));

    let (program, callee_saved) = if optimize {
        register_allocation::register_allocation(program, function_table, aliased)
    } else { (program, HashMap::new()) };

    let mut pseudo_pass = pseudo_replace::PseudoReplacePass::new(program, type_table, struct_table);

    let pseudo_program = pseudo_pass.generate();

    //println!("{:#?}", pseudo_program);

    let mut instr_fixup_pass = instr_fixup::InstructionFixupPass::new(pseudo_program, callee_saved);
    let fixup_program = instr_fixup_pass.generate();

    //println!("{:#?}", fixup_program);

    fixup_program
}