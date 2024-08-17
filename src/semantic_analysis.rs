#![allow(dead_code)]

mod variable_resolution;
mod loop_labeling;

use crate::parser::nodes;

pub fn resolve(program: nodes::Program) -> nodes::Program {
    let mut var_resolver = variable_resolution::VariableResolution::new(program);
    let mut loop_labeler = loop_labeling::LoopLabeling::new(var_resolver.resolve());
    loop_labeler.resolve()
}