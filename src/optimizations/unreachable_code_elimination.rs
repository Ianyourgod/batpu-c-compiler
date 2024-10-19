use crate::{optimizations::cfg, tacky::definition};

pub struct UnreachableCodeElimination {
    cfg: cfg::CFG,
}

impl UnreachableCodeElimination {
    pub fn new(cfg: cfg::CFG) -> UnreachableCodeElimination {
        UnreachableCodeElimination {
            cfg,
        }
    }

    pub fn eliminate(&self) -> cfg::CFG {
        let cfg = self.eliminate_unused_blocks(self.cfg.clone());
        let cfg = self.remove_redundant_jumps(cfg);
        let cfg = self.remove_redundant_labels(cfg);
        cfg
    }

    fn eliminate_unused_blocks(&self, cfg: cfg::CFG) -> cfg::CFG {
        let starting_block = cfg::NodeID::ENTRY;

        let visited = self.get_possible_visited_blocks(&starting_block, &vec![starting_block.clone()]);

        let mut new_nodes = Vec::new();

        for node in &cfg.nodes {
            if visited.contains(&node.get_id()) {
                match node.get_id() {
                    cfg::NodeID::ENTRY => new_nodes.push(node.clone()),
                    cfg::NodeID::EXIT => {
                        let preds = node.get_predecessors();

                        let preds: Vec<cfg::NodeID> = preds.iter().filter(|id| visited.contains(id)).cloned().collect();

                        new_nodes.push(cfg::Node::Exit(cfg::NodeID::EXIT, preds));
                    }
                    cfg::NodeID::BlockID(id) => {
                        let preds = node.get_predecessors();

                        let preds: Vec<cfg::NodeID> = preds.iter().filter(|id| visited.contains(id)).cloned().collect();

                        new_nodes.push(cfg::Node::BasicBlock(
                            cfg::NodeID::BlockID(id),
                            node.get_instructions(),
                            preds,
                            node.get_successors().clone(),
                        ));

                    }
                }
            }
        }

        cfg::CFG {
            nodes: new_nodes,
        }
    }

    fn get_possible_visited_blocks(&self, block: &cfg::NodeID, found: &Vec<cfg::NodeID>) -> Vec<cfg::NodeID> {
        let mut possible_blocks = found.clone();

        let successors = cfg::get_block_by_id(block, &self.cfg.nodes).get_successors().clone();
        for successor in &successors {
            if successor == &cfg::NodeID::EXIT {
                continue;
            }
            if !possible_blocks.contains(successor) {
                possible_blocks.push(successor.clone());
                possible_blocks = self.get_possible_visited_blocks(successor, &possible_blocks);
            }
        }

        possible_blocks
    }

    fn remove_redundant_jumps(&self, mut cfg: cfg::CFG) -> cfg::CFG {
        cfg.sort();

        let mut new_cfg = cfg::CFG {
            nodes: vec![]
        };

        for (i, block) in cfg.nodes.iter().enumerate() {
            new_cfg.nodes.push(match block {
                cfg::Node::BasicBlock(id, instructions, preds, successors) => {
                    if self.is_jump(instructions.last().unwrap()) {
                        let mut keep_jump = false;
                        let default_succ = cfg.nodes[i].get_id();

                        for succ_id in successors {
                            if succ_id != &default_succ {
                                keep_jump = true;
                                break;
                            }
                        }
                        if !keep_jump {
                            let mut new_instructions = instructions.clone();
                            new_instructions.remove(instructions.len()-1);
                            cfg::Node::BasicBlock(id.clone(), new_instructions, preds.clone(), successors.clone())
                        } else {
                            block.clone()
                        }
                    } else {
                        block.clone()
                    }
                }
                _ => block.clone(),
            });
        }

        cfg
    }

    fn remove_redundant_labels(&self, mut cfg: cfg::CFG) -> cfg::CFG {
        cfg.sort();

        let mut new_cfg = cfg::CFG {
            nodes: vec![]
        };

        for (i, block) in cfg.nodes.iter().enumerate() {
            new_cfg.nodes.push(match block {
                cfg::Node::BasicBlock(_, instructions, preds, _) => {
                    if let definition::Instruction::Label(_) = instructions.first().unwrap() {
                        if preds.len() == 1 && preds[0] == cfg::NodeID::BlockID(i-1) {
                            continue;
                        } else {
                            block.clone()
                        }
                    } else {
                        block.clone()
                    }
                }
                _ => block.clone(),
            });
        }

        cfg
    }

    fn is_jump(&self, instruction: &definition::Instruction) -> bool {
        match instruction {
            definition::Instruction::Jump(_) |
            definition::Instruction::JumpIfNotZero(_, _) |
            definition::Instruction::JumpIfZero(_, _) => true,
            _ => false
        }
    }
}