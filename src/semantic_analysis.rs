#![allow(dead_code)]

mod variable_resolution;
mod loop_labeling;
mod type_checking;

use crate::parser::nodes;
use super::errors;

pub fn resolve(program: nodes::Program) -> Result<(nodes::Program, nodes::SymbolTable, nodes::TypeTable), errors::Error> {
    let mut var_resolver = variable_resolution::VariableResolution::new(program);
    let program = var_resolver.resolve()?;

    let mut loop_labeler = loop_labeling::LoopLabeling::new(program);
    let program = loop_labeler.resolve()?;

    //println!("{:#?}", program.statements.get(15));

    let mut type_checker = type_checking::TypeChecker::new(program);
    type_checker.resolve()
}