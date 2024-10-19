mod cfg;
mod liveness;
mod color_graph;

use std::collections::HashMap;

use super::assembly;

const K: i32 = 11;

pub fn register_allocation(functions: assembly::Program, function_table: assembly::FunctionTable, aliased: HashMap<String, Vec<String>>) -> (assembly::Program, HashMap<String, Vec<assembly::Register>>) {
    let mut transformed_functions = Vec::new();
    let mut callee_saved = HashMap::new();

    for tl in functions.statements {
        match tl {
            assembly::TopLevel::FuncDef(func) => {
                let aliased = aliased.get(&func.name).unwrap();
                let name = func.name.clone();

                let (transformed_instructions, cles) = allocate_registers(func, &function_table, aliased);

                callee_saved.insert(name, cles);

                transformed_functions.push(assembly::TopLevel::FuncDef(transformed_instructions));
            }
            assembly::TopLevel::StaticVariable(_, _, _) => transformed_functions.push(tl),
        }
    }
    
    (assembly::Program {
        statements: transformed_functions,
    }, callee_saved)
}

pub fn allocate_registers(mut instructions: assembly::FuncDecl, function_table: &assembly::FunctionTable, aliased: &Vec<String>) -> (assembly::FuncDecl, Vec<assembly::Register>) {
    if !instructions.defined {
        return (instructions, vec![]);
    }

    let (mut interference_graph, mut liveness_analysis): (Graph, liveness::LivenessAnalysis);
    loop {
        (interference_graph, liveness_analysis) = build_graph(&instructions, function_table, aliased);
        let coalesced_regs = coalesce(&mut interference_graph, &instructions.body);
        if coalesced_regs.same_set() {
            break;
        }
        instructions.body = rewrite_coalesced(&instructions.body, &coalesced_regs);
    }
    
    add_spill_costs(&mut interference_graph, &instructions, liveness_analysis, aliased);
    
    color_graph::color_graph(&mut interference_graph);

    let (register_map, callee_saved) = create_register_map(&interference_graph);

    let transformed_instructions = replace_pseudoregs(&instructions.body, register_map);
    
    (assembly::FuncDecl {
        name: instructions.name,
        body: transformed_instructions,
        stack_size: instructions.stack_size,
        global: instructions.global,
        defined: true,
        line: instructions.line,
    }, callee_saved)
}

fn coalesce(graph: &mut Graph, instructions: &Vec<assembly::Instruction>) -> DisjointSets {
    let mut coalesced_regs = DisjointSets::new();

    for instruction in instructions {
        match instruction {
            assembly::Instruction::Mov(src, dst) => {
                let src = coalesced_regs.find(src);
                let dst = coalesced_regs.find(dst);

                let src_node = graph.nodes.get(&src);

                if
                    graph.nodes.contains_key(&src) &&
                    graph.nodes.contains_key(&dst) &&
                    src != dst &&
                    !src_node.unwrap().neighbors.contains(&dst) &&
                    conservitive_coalesceable(graph, &src, &dst)
                {
                    let (to_keep, to_merge) = if let assembly::Operand::Register(_) = src_node.unwrap().id {
                        (src, dst)
                    } else {
                        (dst, src)
                    };

                    //println!("Merging {:?} into {:?}", to_merge, to_keep);

                    coalesced_regs.union(to_merge.clone(), to_keep.clone());
                    update_graph(graph, &to_merge, &to_keep);
                }
            }
            _ => continue,
        }
    }

    coalesced_regs
}

fn conservitive_coalesceable(graph: &Graph, src: &assembly::Operand, dst: &assembly::Operand) -> bool {
    if briggs_test(graph, src, dst) {
        return true;
    }
    if let assembly::Operand::Register(_) = src {
        return george_test(graph, src, dst);
    }
    if let assembly::Operand::Register(_) = dst {
        return george_test(graph, dst, src);
    }
    return false;
}

fn briggs_test(graph: &Graph, x: &assembly::Operand, y: &assembly::Operand) -> bool {
    let mut significant_neighbors = 0;
    let x_node = graph.nodes.get(x).unwrap();
    let y_node = graph.nodes.get(y).unwrap();
    // x_node neighbors combined with y_node neighbors
    let combined_neighbors = x_node.neighbors.iter().chain(y_node.neighbors.iter());
    for n in combined_neighbors {
        let neighbor_node = graph.nodes.get(n).unwrap();
        let mut degree = neighbor_node.neighbors.len();
        if neighbor_node.neighbors.contains(x) &&
           neighbor_node.neighbors.contains(y)
        {
            degree -= 1;
        }
        if degree >= K as usize {
            significant_neighbors += 1;
        }
    }
    return significant_neighbors < K;
}

fn update_graph(graph: &mut Graph, x: &assembly::Operand, y: &assembly::Operand) {
    let node_to_remove = graph.nodes.get(x).unwrap();
    for neighbor in node_to_remove.neighbors.clone() {
        // add edge between y and neighbor
        let y_node = graph.nodes.get_mut(y).unwrap();
        y_node.neighbors.push(neighbor.clone());
        let neighbor_node = graph.nodes.get_mut(&neighbor).unwrap();
        neighbor_node.neighbors.push(y.clone());

        // remove edge between x and neighbor
        neighbor_node.neighbors.retain(|n| n != x);
    }
    // remove_node_by_id(graph, x);
    graph.nodes.remove(x);
}

fn george_test(graph: &Graph, hardreg: &assembly::Operand, pseudoreg: &assembly::Operand) -> bool {
    let pseudo_node = graph.nodes.get(pseudoreg).unwrap();
    for n in &pseudo_node.neighbors {
        if graph.nodes.get(n).unwrap().neighbors.contains(hardreg) {
            continue;
        }
        let neighbor_node = graph.nodes.get(n).unwrap();
        if neighbor_node.neighbors.len() < K as usize {
            return false;
        }
    }
    return true;
}

fn rewrite_coalesced(instructions: &Vec<assembly::Instruction>, coalesced_regs: &DisjointSets) -> Vec<assembly::Instruction> {
    let mut new_instructions = Vec::with_capacity(instructions.len());

    for instruction in instructions {
        match instruction {
            assembly::Instruction::Mov(src, dst) => {
                let src = coalesced_regs.find(src);
                let dst = coalesced_regs.find(dst);

                if src != dst {
                    new_instructions.push(assembly::Instruction::Mov(src, dst));
                }
            }
            assembly::Instruction::Binary(op, src1, src2, dst) => {
                let src1 = coalesced_regs.find(src1);
                let src2 = coalesced_regs.find(src2);
                let dst = coalesced_regs.find(dst);

                new_instructions.push(assembly::Instruction::Binary(op.clone(), src1, src2, dst));
            }
            assembly::Instruction::Unary(op, src, dst) => {
                let src = coalesced_regs.find(src);
                let dst = coalesced_regs.find(dst);

                new_instructions.push(assembly::Instruction::Unary(op.clone(), src, dst));
            }
            assembly::Instruction::Adi(src, imm) => {
                let src = coalesced_regs.find(src);

                new_instructions.push(assembly::Instruction::Adi(src, *imm));
            }
            assembly::Instruction::Cmp(src1, src2) => {
                let src1 = coalesced_regs.find(src1);
                let src2 = coalesced_regs.find(src2);

                new_instructions.push(assembly::Instruction::Cmp(src1, src2));
            }
            assembly::Instruction::Ldi(src, imm) => {
                let src = coalesced_regs.find(src);

                new_instructions.push(assembly::Instruction::Ldi(src, *imm));
            }
            assembly::Instruction::Lea(src, dst) => {
                let src = coalesced_regs.find(src);
                let dst = coalesced_regs.find(dst);

                new_instructions.push(assembly::Instruction::Lea(src, dst));
            }
            assembly::Instruction::Lod(src, off, dst) => {
                let src = coalesced_regs.find(&assembly::Operand::Register(src.clone()));
                let dst = coalesced_regs.find(dst);

                let src = match src {
                    assembly::Operand::Register(r) => r,
                    _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected register"),
                };

                new_instructions.push(assembly::Instruction::Lod(src, *off, dst));
            }
            assembly::Instruction::Str(src, off, dst) => {
                let src = coalesced_regs.find(src);
                let dst = coalesced_regs.find(&assembly::Operand::Register(dst.clone()));

                let dst = match dst {
                    assembly::Operand::Register(r) => r,
                    _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected register"),
                };

                new_instructions.push(assembly::Instruction::Str(src, *off, dst));
            }

            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Comment(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Call(_, _) |
            assembly::Instruction::Return => {
                new_instructions.push(instruction.clone());
            }
        }
    }

    new_instructions
}


fn add_spill_costs(interference_graph: &mut Graph, instructions: &assembly::FuncDecl, liveness: liveness::LivenessAnalysis, aliased: &Vec<String>) {
    for instruction in &instructions.body {
        let (used, updated) = liveness.find_used_and_updated(instruction, aliased);

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

    let mut liveness_analysis = liveness::LivenessAnalysis::new(cfg.clone(), function_table.clone(), aliased.clone());
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

struct DisjointSets {
    set: HashMap<assembly::Operand, assembly::Operand>,
}

impl DisjointSets {
    pub fn new() -> DisjointSets {
        DisjointSets {
            set: HashMap::new(),
        }
    }

    pub fn union(&mut self, a: assembly::Operand, b: assembly::Operand) {
        self.set.insert(a, b);
    }

    pub fn contains(&self, r: &assembly::Operand) -> bool {
        self.set.contains_key(r)
    }

    pub fn find(&self, r: &assembly::Operand) -> assembly::Operand {
        match self.set.get(r) {
            Some(res) => self.find(res),
            None => r.clone(),
        }
    }

    pub fn same_set(&self) -> bool {
        return self.set.is_empty()
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
                    if is_callee_saved(hardreg) && !callee_saved_regs.contains(*hardreg) {
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

fn replace_operand(operand: &assembly::Operand, register_map: &HashMap<String, assembly::Register>, instructions: &mut Vec<assembly::Instruction>) -> assembly::Operand {
    match operand {
        assembly::Operand::Pseudo(name, _) => {
            if register_map.contains_key(name) {
                let reg = register_map.get(name).unwrap().clone();
                //println!("Replacing {} with {:?}", name, reg);
                instructions.push(assembly::Instruction::Comment(format!("Replacing {} with {}", name, reg.name)));
                assembly::Operand::Register(reg)
            } else {
                operand.clone()
            }
        }
        _ => operand.clone()
    }
}

fn replace_pseudoregs(instructions: &Vec<assembly::Instruction>, register_map: HashMap<String, assembly::Register>) -> Vec<assembly::Instruction> {
    let mut new_instructions = Vec::with_capacity(instructions.len()*2);
    
    for instruction in instructions {
        new_instructions.push(assembly::Instruction::Comment(format!("Original: {:?}", instruction)));
        let instr = match instruction {
            assembly::Instruction::Adi(op, imm) => {
                assembly::Instruction::Adi(replace_operand(op, &register_map, &mut new_instructions), *imm)
            }
            assembly::Instruction::Binary(bin, op1, op2, dest) => {
                let op1 = replace_operand(op1, &register_map, &mut new_instructions);
                let op2 = replace_operand(op2, &register_map, &mut new_instructions);
                let dst = replace_operand(dest, &register_map, &mut new_instructions);

                assembly::Instruction::Binary(bin.clone(), op1, op2, dst)
            }
            assembly::Instruction::Cmp(op1, op2) => {
                let op1 = replace_operand(op1, &register_map, &mut new_instructions);
                let op2 = replace_operand(op2, &register_map, &mut new_instructions);

                assembly::Instruction::Cmp(op1, op2)
            }
            assembly::Instruction::Ldi(op, imm) => {
                assembly::Instruction::Ldi(replace_operand(op, &register_map, &mut new_instructions), *imm)
            }
            assembly::Instruction::Lea(src, dst) => {
                // we dont replace src
                let dst = replace_operand(dst, &register_map, &mut new_instructions);

                assembly::Instruction::Lea(src.clone(), dst)
            }
            assembly::Instruction::Mov(src, dst) => {
                let src = replace_operand(src, &register_map, &mut new_instructions);
                let dst = replace_operand(dst, &register_map, &mut new_instructions);

                assembly::Instruction::Mov(src, dst)
            }
            assembly::Instruction::Lod(reg, off, dst) => {
                let dst = replace_operand(dst, &register_map, &mut new_instructions);

                assembly::Instruction::Lod(reg.clone(), off.clone(), dst.clone())
            }
            assembly::Instruction::Str(val, off, reg) => {
                let val = replace_operand(val, &register_map, &mut new_instructions);

                assembly::Instruction::Str(val, off.clone(), reg.clone())
            }
            assembly::Instruction::Unary(unop, op, dst) => {
                let op = replace_operand(op, &register_map, &mut new_instructions);
                let dst = replace_operand(dst, &register_map, &mut new_instructions);

                assembly::Instruction::Unary(unop.clone(), op, dst)
            }

            assembly::Instruction::Comment(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Call(_, _) |
            assembly::Instruction::Return => instruction.clone(),
        };

        new_instructions.push(instr);
    }

    new_instructions
}