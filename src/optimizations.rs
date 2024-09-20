use crate::tacky::definition;

pub mod cfg;

mod constant_folding;
mod unreachable_code_elimination;
mod reaching_copies_analysis;
mod address_taken_analysis;
mod dead_store_elimination;

pub fn optimize(program: definition::Program) -> definition::Program {
    let mut optimized_program = definition::Program {
        statements: Vec::new(),
    };

    for func in program.statements {
        match func {
            definition::TopLevel::FuncDef(func) => {
                let optimized_func_body = optimize_function(func.body);
                optimized_program.statements.push(definition::TopLevel::FuncDef(definition::FuncDef {
                    name: func.name,
                    body: optimized_func_body,
                    params: func.params,
                    global: func.global,
                }));
            },
            definition::TopLevel::StaticVariable(name, global, ty, init) => {
                optimized_program.statements.push(definition::TopLevel::StaticVariable(name, global, ty, init));
            },
        }
    }

    optimized_program
}

pub fn optimize_function(func: Vec<definition::Instruction>) -> Vec<definition::Instruction> {
    if func.len() == 0 {
        return func;
    }

    let mut function_body = func.clone();

    loop {
        let aliased_vars = address_taken_analysis::address_taken_analysis(&function_body);

        let constant_folding = constant_folding::ConstantFolding::new(function_body.clone());
        let folded_body = constant_folding.fold();

        let cfg = cfg::create_cfg(&folded_body);

        let unreachable_code_elimination = unreachable_code_elimination::UnreachableCodeElimination::new(cfg);
        let cfg = unreachable_code_elimination.eliminate();

        let mut reaching_copies_analysis = reaching_copies_analysis::ReachingCopiesAnalysis::new(cfg, aliased_vars.clone());
        let cfg = reaching_copies_analysis.eliminate();

        let mut dead_store_elimination = dead_store_elimination::DeadStoreElimination::new(cfg, aliased_vars);
        let cfg = dead_store_elimination.eliminate();

        let optimized_function_body = cfg::cfg_to_func_def(cfg);

        if function_body == optimized_function_body || optimized_function_body.len() == 0 {
            break;
        }
        function_body = optimized_function_body;
    }

    function_body
}