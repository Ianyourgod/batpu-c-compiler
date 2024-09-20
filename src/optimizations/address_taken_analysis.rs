use crate::tacky::definition;

pub fn address_taken_analysis(function_body: &Vec<definition::Instruction>) -> Vec<definition::Val> {
    let mut aliased_vars = Vec::new();

    for instruction in function_body {
        match instruction {
            definition::Instruction::GetAddress(src, _) => {
                aliased_vars.push(src.clone());
            },
            _ => {},
        }
    }

    aliased_vars
}

fn _is_static_variable(_val: &definition::Val) -> bool {
    todo!()
}