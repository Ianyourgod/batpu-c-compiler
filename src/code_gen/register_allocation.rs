mod cfg;
mod liveness;
mod color_graph;

use std::collections::HashMap;

use super::assembly;

pub fn register_allocation(functions: assembly::Program, function_table: assembly::FunctionTable, aliased: HashMap<String, Vec<String>>) -> assembly::Program {
    let mut transformed_functions = Vec::new();
    for tl in functions.statements {
        match tl {
            assembly::TopLevel::FuncDef(func) => {
                let aliased = aliased.get(&func.name).unwrap();

                let transformed_instructions = allocate_registers(func, &function_table, aliased);
                transformed_functions.push(assembly::TopLevel::FuncDef(transformed_instructions));
            }
            assembly::TopLevel::StaticVariable(_, _, _) => transformed_functions.push(tl),
        }
    }
    assembly::Program {
        statements: transformed_functions,
    }
}

pub fn allocate_registers(instructions: assembly::FuncDecl, function_table: &assembly::FunctionTable, aliased: &Vec<String>) -> assembly::FuncDecl {
    if !instructions.defined {
        return instructions;
    }

    let (mut interference_graph, liveness_analysis) = build_graph(&instructions, function_table, aliased);
    add_spill_costs(&mut interference_graph, &instructions, liveness_analysis);
    
    color_graph::color_graph(&mut interference_graph);

    #[allow(unused_variables)]
    let (register_map, callee_saved) = create_register_map(&interference_graph);

    let transformed_instructions = replace_pseudoregs(&instructions.body, register_map);
    
    assembly::FuncDecl {
        name: instructions.name,
        body: transformed_instructions,
        stack_size: instructions.stack_size,
        global: instructions.global,
        defined: true
    }
}

fn add_spill_costs(interference_graph: &mut Graph, instructions: &assembly::FuncDecl, liveness: liveness::LivenessAnalysis) {
    for instruction in &instructions.body {
        let (used, updated) = liveness.find_used_and_updated(instruction);

        for u in used {
            let node = interference_graph.nodes.get_mut(&u).unwrap();
            node.spill_cost += 1.0;
        }

        for u in updated {
            let node = interference_graph.nodes.get_mut(&u).unwrap();
            node.spill_cost += 1.0;
        }
    }
}

#[derive(Clone, Debug)]
struct Node {
    id: assembly::Operand,
    neighbors: Vec<assembly::Operand>,
    spill_cost: f32,
    color: Option<i32>,
    pruned: bool,
}

impl Node {
    fn new(id: assembly::Operand) -> Node {
        Node {
            id,
            neighbors: Vec::new(),
            spill_cost: 0.0,
            color: None,
            pruned: false,
        }
    }
}

#[derive(Clone, Debug)]
struct Graph {
    nodes: HashMap<assembly::Operand, Node>,
}

fn build_graph(instructions: &assembly::FuncDecl, function_table: &assembly::FunctionTable, aliased: &Vec<String>) -> (Graph, liveness::LivenessAnalysis) {
    let mut interference_graph = generate_base_graph();
    add_pseudo_registers(&mut interference_graph, &instructions, aliased);

    let cfg = cfg::create_cfg(&instructions.body);

    let mut liveness_analysis = liveness::LivenessAnalysis::new(cfg.clone(), function_table.clone());
    liveness_analysis.analyze_and_add_edges(&mut interference_graph);
    
    (interference_graph, liveness_analysis)
}

fn generate_base_graph() -> Graph {
    let mut operands = Vec::new();

    for i in 1..10 {
        let reg = assembly::Register::new(format!("r{}", i));
        operands.push(assembly::Operand::Register(reg));
    }

    // push r12 and 13
    operands.push(assembly::Operand::Register(assembly::Register::new(String::from("r12"))));
    operands.push(assembly::Operand::Register(assembly::Register::new(String::from("r13"))));

    // add connections between registers
    let mut nodes = HashMap::new();
    for operand in &operands {
        let mut node = Node::new(operand.clone());
        node.spill_cost = 999999999999999999.0;
        for neighbor in &operands {
            if &node.id != neighbor {
                node.neighbors.push(neighbor.clone());
            }
        }
        nodes.insert(operand.clone(), node);
    }

    Graph { nodes }
}

fn is_pseudo_register(operand: &assembly::Operand, aliased: &Vec<String>) -> bool {
    match operand {
        assembly::Operand::Pseudo(name, _) => {
            !aliased.contains(name)
        }
        _ => false,
    }
}

fn add_pseudo_registers(graph: &mut Graph, instructions: &assembly::FuncDecl, aliased: &Vec<String>) {
    for instruction in &instructions.body {
        match instruction {
            assembly::Instruction::Adi(src, _) => {
                if is_pseudo_register(&src, aliased) {
                    graph.nodes.insert(src.clone(), Node::new(src.clone()));
                }
            }
            assembly::Instruction::Binary(_, src1, src2, dst) => {
                if is_pseudo_register(&src1, aliased) {
                    graph.nodes.insert(src1.clone(), Node::new(src1.clone()));
                }
                if is_pseudo_register(&src2, aliased) {
                    graph.nodes.insert(src2.clone(), Node::new(src2.clone()));
                }
                if is_pseudo_register(&dst, aliased) {
                    graph.nodes.insert(dst.clone(), Node::new(dst.clone()));
                }
            }
            assembly::Instruction::Cmp(src1, src2) => {
                if is_pseudo_register(&src1, aliased) {
                    graph.nodes.insert(src1.clone(), Node::new(src1.clone()));
                }
                if is_pseudo_register(&src2, aliased) {
                    graph.nodes.insert(src2.clone(), Node::new(src2.clone()));
                }
            }
            assembly::Instruction::Ldi(src, _) => {
                if is_pseudo_register(&src, aliased) {
                    graph.nodes.insert(src.clone(), Node::new(src.clone()));
                }
            }
            assembly::Instruction::Lea(src, dst) => {
                if is_pseudo_register(&src, aliased) {
                    graph.nodes.insert(src.clone(), Node::new(src.clone()));
                }
                if is_pseudo_register(&dst, aliased) {
                    graph.nodes.insert(dst.clone(), Node::new(dst.clone()));
                }
            }
            assembly::Instruction::Lod(_, _, val) => {
                if is_pseudo_register(&val, aliased) {
                    graph.nodes.insert(val.clone(), Node::new(val.clone()));
                }
            }
            assembly::Instruction::Mov(src, dst) => {
                if is_pseudo_register(&src, aliased) {
                    graph.nodes.insert(src.clone(), Node::new(src.clone()));
                }
                if is_pseudo_register(&dst, aliased) {
                    graph.nodes.insert(dst.clone(), Node::new(dst.clone()));
                }
            }
            assembly::Instruction::Unary(_, src, dst) => {
                if is_pseudo_register(&src, aliased) {
                    graph.nodes.insert(src.clone(), Node::new(src.clone()));
                }
                if is_pseudo_register(&dst, aliased) {
                    graph.nodes.insert(dst.clone(), Node::new(dst.clone()));
                }
            }
            assembly::Instruction::Str(val, _, _) => {
                if is_pseudo_register(&val, aliased) {
                    graph.nodes.insert(val.clone(), Node::new(val.clone()));
                }
            }

            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Comment(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Call(_, _) |
            assembly::Instruction::Return => ()
        }
    }
}

fn create_register_map(graph: &Graph) -> (HashMap<String, assembly::Register>, Vec<assembly::Register>) {
    let mut color_map = HashMap::new();

    for node in &graph.nodes {
        match node.0 {
            assembly::Operand::Register(r) => {
                color_map.insert(node.1.color.unwrap(), r);
            }
            _ => continue,
        }
    }

    let mut register_map = HashMap::new();
    let mut callee_saved_regs = Vec::new();
    for node in &graph.nodes {
        match node.0 {
            assembly::Operand::Pseudo(name, _) => {
                if node.1.color.is_some() {
                    let hardreg = color_map.get(&node.1.color.unwrap()).unwrap();
                    register_map.insert(name.clone(), (*hardreg).clone());
                    if is_callee_saved(hardreg) {
                        callee_saved_regs.push((*hardreg).clone());
                    }
                }
            }
            _ => continue,
        }
    }

    (register_map, callee_saved_regs)
}

fn is_callee_saved(reg: &assembly::Register) -> bool {
    match reg.name.as_str() {
        "r12" |
        "r13" |
        "r14" |
        "r15" => true,
        _ => false
    }
}

fn replace_operand(operand: &assembly::Operand, register_map: &HashMap<String, assembly::Register>) -> assembly::Operand {
    match operand {
        assembly::Operand::Pseudo(name, _) => {
            if register_map.contains_key(name) {
                let reg = register_map.get(name).unwrap().clone();
                //println!("Replacing {} with {:?}", name, reg);
                assembly::Operand::Register(reg)
            } else {
                operand.clone()
            }
        }
        _ => operand.clone()
    }
}

fn replace_pseudoregs(instructions: &Vec<assembly::Instruction>, register_map: HashMap<String, assembly::Register>) -> Vec<assembly::Instruction> {
    let mut new_instructions = Vec::with_capacity(instructions.len());
    
    for instruction in instructions {
        new_instructions.push(match instruction {
            assembly::Instruction::Adi(op, imm) => {
                assembly::Instruction::Adi(replace_operand(op, &register_map), *imm)
            }
            assembly::Instruction::Binary(bin, op1, op2, dest) => {
                let op1 = replace_operand(op1, &register_map);
                let op2 = replace_operand(op2, &register_map);
                let dst = replace_operand(dest, &register_map);

                assembly::Instruction::Binary(bin.clone(), op1, op2, dst)
            }
            assembly::Instruction::Cmp(op1, op2) => {
                let op1 = replace_operand(op1, &register_map);
                let op2 = replace_operand(op2, &register_map);

                assembly::Instruction::Cmp(op1, op2)
            }
            assembly::Instruction::Ldi(op, imm) => {
                assembly::Instruction::Ldi(replace_operand(op, &register_map), *imm)
            }
            assembly::Instruction::Lea(src, dst) => {
                // we dont replace src
                let dst = replace_operand(dst, &register_map);

                assembly::Instruction::Lea(src.clone(), dst)
            }
            assembly::Instruction::Mov(src, dst) => {
                let src = replace_operand(src, &register_map);
                let dst = replace_operand(dst, &register_map);

                assembly::Instruction::Mov(src, dst)
            }
            assembly::Instruction::Lod(reg, off, dst) => {
                let dst = replace_operand(dst, &register_map);

                assembly::Instruction::Lod(reg.clone(), off.clone(), dst.clone())
            }
            assembly::Instruction::Str(val, off, reg) => {
                let val = replace_operand(val, &register_map);

                assembly::Instruction::Str(val, off.clone(), reg.clone())
            }
            assembly::Instruction::Unary(unop, op, dst) => {
                let op = replace_operand(op, &register_map);
                let dst = replace_operand(dst, &register_map);

                assembly::Instruction::Unary(unop.clone(), op, dst)
            }

            assembly::Instruction::Comment(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Call(_, _) |
            assembly::Instruction::Return => instruction.clone(),
        });
    }

    new_instructions
}