#![allow(dead_code)]

mod variable_resolution;

use crate::parser::nodes;

pub fn resolve(program: nodes::Program) -> nodes::Program {
    let mut resolver = variable_resolution::VariableResolution::new(program);
    resolver.resolve()
}