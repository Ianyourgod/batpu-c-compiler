use std::collections::{HashMap, HashSet};

use crate::optimizations::cfg;
use crate::tacky::definition;

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
    pub live: HashSet<definition::Val>,
}

pub struct DeadStoreElimination {
    cfg: cfg::CFG,
    annotations: Annotations,
    aliased_vars: Vec<definition::Val>,
}

impl DeadStoreElimination {
    pub fn new(cfg: cfg::CFG, aliased_vars: Vec<definition::Val>) -> DeadStoreElimination {
        DeadStoreElimination {
            cfg,
            annotations: Annotations::new(),
            aliased_vars,
        }
    }

    pub fn eliminate(&mut self) -> cfg::CFG {
        let cfg = self.cfg.clone();

        self.find_deadness(&cfg);

        let mut new_cfg = cfg::CFG {
            nodes: Vec::new(),
        };
        for node in &cfg.nodes {
            match node {
                cfg::Node::BasicBlock(id, instructions, preds, succs) => {
                    let mut new_instructions = Vec::new();
                    for (i, instruction) in instructions.iter().rev().enumerate() {
                        if self.is_dead_store(instruction, &(id.clone(), i)) {
                            continue;
                        }
                        new_instructions.push(instruction.clone());
                    }
                    new_instructions.reverse();
                    new_cfg.nodes.push(cfg::Node::BasicBlock(id.clone(), new_instructions, preds.clone(), succs.clone()));
                }
                _ => new_cfg.nodes.push(node.clone()),
            }
        }

        new_cfg
    }

    fn transfer(&mut self, block: &cfg::Node, end_live_variables: &Annotation) {
        let mut current_live_variables = end_live_variables.clone();

        let (id, instructions) = match block {
            cfg::Node::Entry(_, _) |
            cfg::Node::Exit(_, _) => {
                return;
            }
            cfg::Node::BasicBlock(id, instructions, _, _) => {
                (id, instructions)
            }
        };

        for (i, instruction) in instructions.iter().rev().enumerate() {
            self.annotations.instruction_annotations.insert((id.clone(), i), current_live_variables.clone());
            
            match instruction {
                definition::Instruction::Binary(_, src1, src2, dst) => {
                    current_live_variables.live.remove(dst);
                    if self.is_var(src1) {
                        current_live_variables.live.insert(src1.clone());
                    }
                    if self.is_var(src2) {
                        current_live_variables.live.insert(src2.clone());
                    }
                }
                definition::Instruction::JumpIfZero(cond, _) => {
                    if self.is_var(cond) {
                        current_live_variables.live.insert(cond.clone());
                    }
                }
                definition::Instruction::JumpIfNotZero(cond, _) => {
                    if self.is_var(cond) {
                        current_live_variables.live.insert(cond.clone());
                    }
                }
                definition::Instruction::FunCall(_, args, dst, _) => {
                    if dst.is_some() { current_live_variables.live.remove(dst.as_ref().unwrap()); }
                    for arg in args {
                        if self.is_var(arg) {
                            current_live_variables.live.insert(arg.clone());
                        }
                    }
                }
                definition::Instruction::AddPtr(val1, val2, offset, dst) => {
                    current_live_variables.live.remove(dst);
                    if self.is_var(val1) {
                        current_live_variables.live.insert(val1.clone());
                    }
                    if self.is_var(val2) {
                        current_live_variables.live.insert(val2.clone());
                    }
                    if self.is_var(offset) {
                        current_live_variables.live.insert(offset.clone());
                    }
                }
                definition::Instruction::Copy(dst, src) => {
                    current_live_variables.live.remove(dst);
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                }
                definition::Instruction::Return(val) => {
                    if val.is_some() {
                        if self.is_var(val.as_ref().unwrap()) {
                            current_live_variables.live.insert(val.as_ref().unwrap().clone());
                        }
                    }
                }
                definition::Instruction::Unary(_, src, dst) => {
                    current_live_variables.live.remove(dst);
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                }
                definition::Instruction::Load(src, dst) => {
                    current_live_variables.live.remove(dst);
                    for var in &self.aliased_vars {
                        current_live_variables.live.insert(var.clone());
                    }
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                }
                definition::Instruction::Store(src, val) => {
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                    if self.is_var(val) {
                        current_live_variables.live.insert(val.clone());
                    }
                }
                definition::Instruction::CopyFromOffset(src, _, dst) => {
                    current_live_variables.live.remove(dst);
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                }
                definition::Instruction::GetAddress(src, dst) => {
                    current_live_variables.live.remove(dst);
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                }
                definition::Instruction::CopyToOffset(src, _, _) => {
                    if self.is_var(src) {
                        current_live_variables.live.insert(src.clone());
                    }
                }

                definition::Instruction::Label(_) => (),
                definition::Instruction::Jump(_) => (),
            }
        }

        self.annotations.block_annotations.insert(id.clone(), current_live_variables);
    }

    fn meet(&self, block: &cfg::Node) -> Annotation {
        let mut live_variables = HashSet::new();

        for succ_id in block.get_successors() {
            match succ_id {
                cfg::NodeID::EXIT => continue,
                cfg::NodeID::ENTRY => panic!("Malformed CFG"),
                cfg::NodeID::BlockID(_) => {
                    let annotation = self.annotations.get_block_annotation(succ_id).unwrap();
                    for var in annotation.live.iter() {
                        live_variables.insert(var.clone());
                    }
                }
            }
        }

        return Annotation {
            live: live_variables
        };
    }

    fn find_deadness(&mut self, graph: &cfg::CFG) {
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

            self.transfer(&block, &incoming_copies);

            if old_annotation != *self.annotations.get_block_annotation(&block_id).unwrap() {
                for pred_id in block.get_predecessors() {
                    match pred_id {
                        cfg::NodeID::ENTRY => continue,
                        cfg::NodeID::EXIT => panic!("Mal formed CFG"),
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

        panic!("Block not found");
    }

    fn is_var(&self, src: &definition::Val) -> bool {
        match src {
            definition::Val::Var(_, _) => true,
            _ => false
        }
    }

    fn is_dead_store(&self, instr: &definition::Instruction, id: &(cfg::NodeID, usize)) -> bool {
        match instr {
            definition::Instruction::FunCall(_, _, _, _) => false,

            definition::Instruction::Binary(_, _, _, dst) |
            definition::Instruction::Unary(_, _, dst) |
            definition::Instruction::AddPtr(_, _, _, dst) |
            definition::Instruction::Load(_, dst) |
            definition::Instruction::CopyFromOffset(_, _, dst) |
            definition::Instruction::GetAddress(_, dst) |
            definition::Instruction::Copy(dst, _) => {
                if self.is_var(dst) {
                    return !self.annotations.get_instruction_annotation(id).unwrap().live.contains(dst);
                }
                false
            }

            definition::Instruction::JumpIfZero(_, _) |
            definition::Instruction::JumpIfNotZero(_, _) |
            definition::Instruction::Jump(_) |
            definition::Instruction::Label(_) |
            definition::Instruction::Return(_) |
            definition::Instruction::Store(_, _) |
            definition::Instruction::CopyToOffset(_, _, _) => false,


        }
    }
}
