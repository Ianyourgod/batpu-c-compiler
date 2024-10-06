use std::collections::HashMap;

use crate::{optimizations::cfg, tacky::definition};

pub struct ReachingCopiesAnalysis {
    cfg: cfg::CFG,
    annotations: Annotations,
    aliased_vars: Vec<definition::Val>,
}

#[derive(Clone, Debug)]
pub struct Annotations {
    pub block_annotations: HashMap<cfg::NodeID, Annotation>,
    pub instruction_annotations: HashMap<(cfg::NodeID, usize), Annotation>,
}

impl Annotations {
    pub fn get_block_annotation(&self, id: &cfg::NodeID) -> Option<&Annotation> {
        self.block_annotations.get(id)
    }

    pub fn get_instruction_annotation(&self, id: &(cfg::NodeID, usize)) -> Option<&Annotation> {
        self.instruction_annotations.get(id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    pub copies: Vec<(definition::Val, definition::Val)>,
}

impl ReachingCopiesAnalysis {
    pub fn new(cfg: cfg::CFG, aliased_vars: Vec<definition::Val>) -> ReachingCopiesAnalysis {
        ReachingCopiesAnalysis {
            cfg,
            annotations: Annotations {
                block_annotations: HashMap::new(),
                instruction_annotations: HashMap::new(),
            },
            aliased_vars,
        }
    }

    pub fn eliminate(&mut self) -> cfg::CFG {
        let cfg = self.cfg.clone();
        self.find_reaching_copies(&cfg);

        let mut new_cfg = cfg::CFG {
            nodes: Vec::new(),
        };
        for node in &cfg.nodes {
            match node {
                cfg::Node::BasicBlock(id, instructions, preds, succs) => {
                    let new_instructions = self.rewrite_instructions(id, instructions);
                    new_cfg.nodes.push(cfg::Node::BasicBlock(id.clone(), new_instructions, preds.clone(), succs.clone()));
                }
                _ => new_cfg.nodes.push(node.clone()),
            }
        }

        new_cfg
    }

    fn get_node_from_id(&self, node_id: &cfg::NodeID) -> cfg::Node {
        for node in &self.cfg.nodes {
            if &node.get_id() == node_id {
                return node.clone();
            }
        }

        unreachable!("INTERNAL ERROR. PLEASE REPORT: Block not found");
    }

    fn transfer(&mut self, node: &cfg::Node, initial_reaching_copies: &Annotation) {
        let mut current_reaching_copies = initial_reaching_copies.clone();

        let (id, instructions) = match node {
            cfg::Node::Entry(id, _) |
            cfg::Node::Exit(id, _) => {
                self.annotations.block_annotations.insert(id.clone(), current_reaching_copies.clone());
                return;
            }
            cfg::Node::BasicBlock(id, instructions, _, _) => {
                (id, instructions)
            }
        };

        for (i, instruction) in instructions.iter().enumerate() {
            self.annotations.instruction_annotations.insert((id.clone(), i), current_reaching_copies.clone());

            match instruction {
                definition::Instruction::Copy(dst, src) => {
                    if current_reaching_copies.copies.contains(&(dst.clone(), src.clone())) {
                        continue;
                    }

                    let mut removed = 0;
                    for (i, (copy_src, copy_dst)) in current_reaching_copies.copies.clone().iter().enumerate() {
                        if copy_src == dst || copy_dst == dst {
                            current_reaching_copies.copies.remove(i-removed);
                            removed += 1;
                        }
                    }

                    current_reaching_copies.copies.push((src.clone(), dst.clone()));
                }
                definition::Instruction::FunCall(_, _, dst, _) => {
                    for (i, copy) in current_reaching_copies.copies.clone().iter().enumerate() {
                        if  self.aliased_vars.contains(&copy.0) ||
                            self.aliased_vars.contains(&copy.1) ||
                            (dst.is_some() && (&copy.0 == dst.as_ref().unwrap() || &copy.1 == dst.as_ref().unwrap())) {

                            current_reaching_copies.copies.remove(i);
                        }
                    }
                }
                definition::Instruction::Unary(_, _, dst) |
                definition::Instruction::Binary(_, _, _, dst) => {
                    for (i, (copy_src, copy_dst)) in current_reaching_copies.clone().copies.iter().enumerate() {
                        if copy_src == dst || copy_dst == dst {
                            current_reaching_copies.copies.remove(i);
                        }
                    }
                }
                _ => continue,
            }
        }

        self.annotations.block_annotations.insert(id.clone(), current_reaching_copies.clone());
    }

    fn meet(&mut self, block: &cfg::Node, all_copies: &Vec<(definition::Val, definition::Val)>) -> Vec<(definition::Val, definition::Val)> {
        let mut incoming_copies = all_copies.clone();
        for pred_id in block.get_predecessors() {
            match pred_id {
                cfg::NodeID::ENTRY => return Vec::new(),
                cfg::NodeID::EXIT => unreachable!("INTERNAL ERROR. PLEASE REPORT: Mal formed CFG"),
                cfg::NodeID::BlockID(_) => {
                    let pred_out_copies = self.annotations.get_block_annotation(&pred_id).unwrap().copies.clone();
                    incoming_copies = incoming_copies.into_iter().filter(|item| !pred_out_copies.contains(item)).collect();
                }
            }
        }

        return incoming_copies;
    }

    fn find_all_copies(&mut self, graph: &cfg::CFG) -> Annotation {
        let mut all_copies = Vec::new();

        for node in &graph.nodes {
            match node {
                cfg::Node::BasicBlock(_, instructions, _, _) => {
                    for instruction in instructions {
                        match instruction {
                            definition::Instruction::Copy(dst, src) => {
                                all_copies.push((src.clone(), dst.clone()));
                            }
                            _ => continue,
                        }
                    }
                }
                _ => continue,
            }
        }

        Annotation {
            copies: all_copies,
        }
    }

    fn find_reaching_copies(&mut self, graph: &cfg::CFG) {
        let all_copies = self.find_all_copies(graph);
        let mut worklist = Vec::new();

        for node in &graph.nodes {
            match node {
                cfg::Node::Entry(_, _) => continue,
                cfg::Node::Exit(_, _) => continue,
                cfg::Node::BasicBlock(id, _, _, _) => {
                    worklist.push(id.clone());
                    self.annotations.block_annotations.insert(id.clone(), all_copies.clone());
                }
            }
        }

        while worklist.len() > 0 {
            let block_id = worklist.first().unwrap();

            let block = self.get_node_from_id(block_id);

            let old_annotation = self.annotations.get_block_annotation(block_id).unwrap().clone();
            let incoming_copies = self.meet(&block, &all_copies.copies);
            self.transfer(&block, &Annotation { copies: incoming_copies });

            if old_annotation != *self.annotations.get_block_annotation(block_id).unwrap() {
                for succ_id in block.get_successors() {
                    match succ_id {
                        cfg::NodeID::ENTRY => unreachable!("INTERNAL ERROR. PLEASE REPORT: Mal formed CFG"),
                        cfg::NodeID::EXIT => continue,
                        cfg::NodeID::BlockID(_) => {
                            if !worklist.contains(&succ_id) {
                                worklist.push(succ_id.clone());
                            }
                        }
                    }
                }
            }

            worklist.remove(0);
        }
    }

    fn replace_operand(&self, op: &definition::Val, reaching_copies: &Annotation) -> definition::Val {
        if let definition::Val::Const(_) = op {
            return op.clone();
        } 

        for copy in &reaching_copies.copies {
            if copy.1 == *op {
                return copy.0.clone();
            }
        }

        return op.clone();
    }

    fn rewrite_instruction(&self, block_id: &cfg::NodeID, idx: usize, instruction: &definition::Instruction) -> Option<definition::Instruction> {
        let reaching_copies = self.annotations.get_instruction_annotation(&(*block_id, idx)).unwrap();

        match instruction {
            definition::Instruction::Copy(dst, src) => {
                for copy in &reaching_copies.copies {
                    if (copy.0 == *src && copy.1 == *dst) || (copy.0 == *dst && copy.1 == *src) {
                        return None;
                    }
                }
                let new_src = self.replace_operand(src, reaching_copies);
                return Some(definition::Instruction::Copy(dst.clone(), new_src));
            }
            definition::Instruction::Unary(op, src, dst) => {
                let new_src = self.replace_operand(src, reaching_copies);
                return Some(definition::Instruction::Unary(op.clone(), new_src, dst.clone()));
            }
            definition::Instruction::Binary(op, src1, src2, dst) => {
                let new_src1 = self.replace_operand(src1, reaching_copies);
                let new_src2 = self.replace_operand(src2, reaching_copies);
                return Some(definition::Instruction::Binary(op.clone(), new_src1, new_src2, dst.clone()));
            }
            definition::Instruction::FunCall(name, args, dst, globle) => {
                let new_args = args.iter().map(|arg| self.replace_operand(arg, reaching_copies)).collect();
                return Some(definition::Instruction::FunCall(name.clone(), new_args, dst.clone(), *globle));
            }
            definition::Instruction::AddPtr(src1, src2, offset, dst) => {
                let new_src1 = self.replace_operand(src1, reaching_copies);
                let new_src2 = self.replace_operand(src2, reaching_copies);
                return Some(definition::Instruction::AddPtr(new_src1, new_src2, *offset, dst.clone()));
            } 
            definition::Instruction::CopyFromOffset(var, offset, val) => {
                let new_val = self.replace_operand(val, reaching_copies);
                return Some(definition::Instruction::CopyFromOffset(var.clone(), *offset, new_val));
            }
            definition::Instruction::CopyToOffset(val, var, offset) => {
                let new_val = self.replace_operand(val, reaching_copies);
                return Some(definition::Instruction::CopyToOffset(new_val, var.clone(), *offset));
            }
            definition::Instruction::JumpIfNotZero(val, lbl) => {
                let new_val = self.replace_operand(val, reaching_copies);
                return Some(definition::Instruction::JumpIfNotZero(new_val, lbl.clone()));
            }
            definition::Instruction::JumpIfZero(val, lbl) => {
                let new_val = self.replace_operand(val, reaching_copies);
                return Some(definition::Instruction::JumpIfZero(new_val, lbl.clone()));
            }
            definition::Instruction::Return(val) => {
                match val {
                    Some(val) => {
                        let new_val = self.replace_operand(val, reaching_copies);
                        return Some(definition::Instruction::Return(Some(new_val)));
                    }
                    None => return Some(definition::Instruction::Return(None)),
                }
            }
            definition::Instruction::Load(val, dst) => {
                let new_val = self.replace_operand(val, reaching_copies);
                return Some(definition::Instruction::Load(new_val, dst.clone()));
            }
            definition::Instruction::Store(val, dst) => {
                let new_val = self.replace_operand(val, reaching_copies);
                return Some(definition::Instruction::Store(new_val, dst.clone()));
            }
            definition::Instruction::Jump(_) => return Some(instruction.clone()),
            definition::Instruction::Label(_) => return Some(instruction.clone()),
            definition::Instruction::GetAddress(_, _) => return Some(instruction.clone()),
        }
    }

    fn rewrite_instructions(&self, block_id: &cfg::NodeID, instructions: &Vec<definition::Instruction>) -> Vec<definition::Instruction> {
        let mut new_instructions = Vec::new();

        for (i, instruction) in instructions.iter().enumerate() {
            match self.rewrite_instruction(block_id, i, instruction) {
                Some(new_instruction) => new_instructions.push(new_instruction),
                None => continue,
            }
        }

        new_instructions
    }
}