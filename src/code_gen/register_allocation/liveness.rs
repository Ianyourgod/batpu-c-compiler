use std::collections::{HashMap, HashSet};

use crate::code_gen::assembly;

use super::cfg;

#[derive(Clone, Debug)]
pub struct Annotations {
    pub block_annotations: HashMap<cfg::NodeID, Annotation>,
    pub instruction_annotations: HashMap<(cfg::NodeID, usize), Annotation>,
}

impl Annotations {
    pub fn new() -> Annotations {
        return Annotations {
            block_annotations: HashMap::new(),
            instruction_annotations: HashMap::new()
        }
    }

    pub fn get_block_annotation(&self, id: &cfg::NodeID) -> Option<&Annotation> {
        self.block_annotations.get(id)
    }

    pub fn get_instruction_annotation(&self, id: &(cfg::NodeID, usize)) -> Option<&Annotation> {
        self.instruction_annotations.get(id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    pub live: HashSet<assembly::Operand>,
}

pub struct LivenessAnalysis {
    cfg: cfg::CFG,
    annotations: Annotations,
    function_table: assembly::FunctionTable,
    aliased: Vec<String>,
}

impl LivenessAnalysis {
    pub fn new(cfg: cfg::CFG, function_table: assembly::FunctionTable, aliased: Vec<String>) -> LivenessAnalysis {
        return LivenessAnalysis {
            cfg,
            annotations: Annotations::new(),
            function_table,
            aliased
        }
    }

    pub fn analyze_and_add_edges(&mut self, interference_graph: &mut super::Graph) {
        let aliased = self.aliased.clone();

        self.find_deadness(&aliased);

        self.add_edges(interference_graph, &aliased);
    }

    pub fn find_deadness(&mut self, aliased: &Vec<String>) {
        let graph = &self.cfg;

        let mut worklist = Vec::new();

        for node in &graph.nodes {
            match node {
                cfg::Node::Entry(_, _) => continue,
                cfg::Node::Exit(_, _) => continue,
                cfg::Node::BasicBlock(id, _, _, _) => {
                    worklist.push(id.clone());
                    self.annotations.block_annotations.insert(id.clone(), Annotation {
                        live: HashSet::new()
                    });
                }
            }
        }
        
        while worklist.len() > 0 {
            let block_id = worklist.pop().unwrap();

            let block = self.get_node_from_id(&block_id);

            let old_annotation = self.annotations.get_block_annotation(&block_id).unwrap().clone();
            let incoming_copies = self.meet(&block);

            self.transfer(&block, &incoming_copies, aliased);

            if old_annotation != *self.annotations.get_block_annotation(&block_id).unwrap() {
                for pred_id in block.get_predecessors() {
                    match pred_id {
                        cfg::NodeID::ENTRY => continue,
                        cfg::NodeID::EXIT => unreachable!("INTERNAL ERROR. PLEASE REPORT: Mal formed CFG"),
                        cfg::NodeID::BlockID(_) => {
                            if !worklist.contains(&pred_id) {
                                worklist.push(pred_id.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_node_from_id(&self, node_id: &cfg::NodeID) -> cfg::Node {
        for node in &self.cfg.nodes {
            if &node.get_id() == node_id {
                return node.clone();
            }
        }

        unreachable!("INTERNAL ERROR. PLEASE REPORT: Block not found");
    }

    fn meet(&self, block: &cfg::Node) -> HashSet<assembly::Operand> {
        let mut live_registers = HashSet::new();
        for succ_id in block.get_successors() {
            match succ_id {
                cfg::NodeID::ENTRY => unreachable!("INTERNAL ERROR. PLEASE REPORT: Malformed control-flow graph"),
                cfg::NodeID::EXIT => { live_registers.insert(assembly::Operand::Register(assembly::Register::new(String::from("r1")))); },
                cfg::NodeID::BlockID(_) => {
                    let succ_live_registers = self.annotations.get_block_annotation(succ_id).unwrap().live.clone();
                    live_registers = live_registers.union(&succ_live_registers).cloned().collect();
                }
            }
        }
        return live_registers;
    }

    fn is_stack_reg(&self, op: &assembly::Operand) -> bool {
        match op {
            assembly::Operand::Register(r) => {
                match r.name.as_str() {
                    "r14" |
                    "rsp" |
                    "r15" |
                    "rbp" => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn handle_ops(&self, read: &Vec<assembly::Operand>, updated: &Vec<assembly::Operand>, aliased: &Vec<String>) -> (Vec<assembly::Operand>, Vec<assembly::Operand>) {
        let mut new_read = Vec::new();
        let mut new_updated = Vec::new();

        for r in read {
            if self.is_stack_reg(r) {
                continue;
            }

            match r {
                assembly::Operand::Memory(reg, _) => {
                    match reg.name.as_str() {
                        "r14" |
                        "rsp" |
                        "r15" |
                        "rbp" => continue,
                        _ => ()
                    }
                    new_read.push(assembly::Operand::Register(reg.clone()));
                }
                assembly::Operand::Register(reg) => {
                    match reg.name.as_str() {
                        "r14" |
                        "rsp" |
                        "r15" |
                        "rbp" => continue,
                        _ => new_read.push(r.clone())
                    }
                }

                assembly::Operand::Pseudo(name, ty) => {
                    if aliased.contains(name) {
                        continue;
                    }

                    match ty {
                        assembly::Type::Struct(_) => continue,
                        _ => new_read.push(r.clone())
                    }
                }

                assembly::Operand::Immediate(_) |
                assembly::Operand::PseudoMem(_, _, _) => continue,

                assembly::Operand::Data(_) => unreachable!("erm what the skibma")
            }
        }

        for u in updated {
            if self.is_stack_reg(u) {
                continue;
            }

            match u {
                assembly::Operand::Memory(reg, _) => {
                    match reg.name.as_str() {
                        "r14" |
                        "rsp" |
                        "r15" |
                        "rbp" => continue,
                        _ => ()
                    }
                    new_read.push(assembly::Operand::Register(reg.clone()));
                }
                assembly::Operand::Register(reg) => {
                    match reg.name.as_str() {
                        "r14" |
                        "rsp" |
                        "r15" |
                        "rbp" => continue,
                        _ => new_updated.push(u.clone())
                    }
                }

                assembly::Operand::Pseudo(name, ty) => {
                    if aliased.contains(name) {
                        continue;
                    }

                    match ty {
                        assembly::Type::Struct(_) => continue,
                        _ => new_updated.push(u.clone())
                    }
                }

                assembly::Operand::Immediate(_) |
                assembly::Operand::PseudoMem(_, _, _) => continue,

                assembly::Operand::Data(_) => unreachable!("erm what the skibma")
            }
        }

        return (new_read, new_updated);
    }

    pub fn find_used_and_updated(&self, instruction: &assembly::Instruction, aliased: &Vec<String>) -> (Vec<assembly::Operand>, Vec<assembly::Operand>) {
        let (used, updated) = match instruction {
            assembly::Instruction::Adi(src, _) => {
                (vec![src.clone()], vec![src.clone()])
            }
            assembly::Instruction::Binary(_, src1, src2, dst) => {
                (vec![src1.clone(), src2.clone()], vec![dst.clone()])
            }
            assembly::Instruction::Cmp(src1, src2) => {
                (vec![src1.clone(), src2.clone()], vec![])
            }
            assembly::Instruction::Ldi(src, _) => {
                (vec![], vec![src.clone()])
            }
            assembly::Instruction::Lea(src, dst) => {
                (vec![src.clone()], vec![dst.clone()])
            }
            assembly::Instruction::Lod(src_reg, _, dst) => {
                (vec![assembly::Operand::Register(src_reg.clone())], vec![dst.clone()])
            }
            assembly::Instruction::Mov(src, dst) => {
                (vec![src.clone()], vec![dst.clone()])
            }
            assembly::Instruction::Unary(_, src, dst) => {
                (vec![src.clone()], vec![dst.clone()])
            }
            assembly::Instruction::Str(val, _, dst_reg) => {
                (vec![val.clone(), assembly::Operand::Register(dst_reg.clone())], vec![])
            }
            assembly::Instruction::Call(name, _) => {
                // lookup function and get all registers used
                let used_registers = match name.as_str() {
                    "__mult" => vec![assembly::Operand::Register(assembly::Register::new(String::from("r1"))), assembly::Operand::Register(assembly::Register::new(String::from("r2")))],
                    "__div" => vec![assembly::Operand::Register(assembly::Register::new(String::from("r1"))), assembly::Operand::Register(assembly::Register::new(String::from("r2")))],
                    _ => self.function_table.lookup(name).unwrap().iter().map(|x| assembly::Operand::Register(x.clone())).collect()
                };

                // r1-r11 are caller-saved registers
                let updated = vec![
                    assembly::Operand::Register(assembly::Register::new(String::from("r1"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r2"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r3"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r4"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r5"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r6"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r7"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r8"))),
                    assembly::Operand::Register(assembly::Register::new(String::from("r9"))),
                    // dont mark these as updated since they are purely for temporary vals
                    //assembly::Operand::Register(assembly::Register::new(String::from("r10"))),
                    //assembly::Operand::Register(assembly::Register::new(String::from("r11"))),
                ];

                (used_registers, updated)
            }

            assembly::Instruction::AllocateStack(_) |
            assembly::Instruction::Comment(_) |
            assembly::Instruction::Jmp(_) |
            assembly::Instruction::JmpCC(_, _) |
            assembly::Instruction::Label(_) |
            assembly::Instruction::Return => (vec![], vec![])
        };

        return self.handle_ops(&used, &updated, aliased);
    }

    fn is_register(&self, operand: &assembly::Operand) -> bool {
        match operand {
            assembly::Operand::Register(_) |
            assembly::Operand::Pseudo(_, _)  => true,
            _ => false
        }
    }

    fn transfer(&mut self, block: &cfg::Node, end_live_registers: &HashSet<assembly::Operand>, aliased: &Vec<String>) {
        let mut current_live_registers = end_live_registers.clone();

        let instructions = block.get_instructions();

        let block_id = block.get_id();

        for (i, instruction) in instructions.iter().rev().enumerate() {
            self.annotations.instruction_annotations.insert((block_id.clone(), instructions.len()-i-1), Annotation {
                live: current_live_registers.clone()
            });

            let (used, updated) = self.find_used_and_updated(instruction, aliased);

            for u in updated {
                if self.is_register(&u) {
                    current_live_registers.remove(&u);
                }
            }

            for u in used {
                if self.is_register(&u) {
                    current_live_registers.insert(u);
                }
            }
        }

        self.annotations.block_annotations.insert(block_id.clone(), Annotation {
            live: current_live_registers.clone()
        });
    }

    fn is_mov(&self, instr: &assembly::Instruction) -> (bool, assembly::Operand) {
        match instr {
            assembly::Instruction::Mov(src, _) => (true, src.clone()),
            _ => (false, assembly::Operand::Immediate(0))
        }
    }

    fn graph_contains_node(&self, graph: &super::Graph, node: &assembly::Operand) -> bool {
        graph.nodes.contains_key(node)
    }

    fn add_edges(&mut self, interference_graph: &mut super::Graph, aliased: &Vec<String>) {
        for node in &self.cfg.nodes {
            match node {
                cfg::Node::Entry(_, _) |
                cfg::Node::Exit(_, _) => continue,

                cfg::Node::BasicBlock(_, instructions, _, _) => {
                    for (i, instr) in instructions.iter().enumerate() {
                        let (_, updated) = self.find_used_and_updated(instr, aliased);
                        let live_registers = self.annotations.get_instruction_annotation(&(node.get_id(), i)).unwrap().live.clone();

                        for l in &live_registers {
                            let (is_mov, src) = self.is_mov(instr);
                            if is_mov && src == *l {
                                continue;
                            }

                            for u in &updated {
                                if self.graph_contains_node(interference_graph, u) &&
                                   self.graph_contains_node(interference_graph, l) &&
                                   l != u {
                                    // add edge l <-> u
                                    let l_node = interference_graph.nodes.get_mut(&l).unwrap();
                                    l_node.neighbors.push(u.clone());

                                    let u_node = interference_graph.nodes.get_mut(&u).unwrap();
                                    u_node.neighbors.push(l.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}