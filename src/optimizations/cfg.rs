#![allow(dead_code)]

use std::collections::HashMap;

use crate::tacky::definition;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NodeID {
    ENTRY,
    EXIT,
    BlockID(usize),
}

#[derive(Clone, Debug)]
pub enum Node {
    BasicBlock(NodeID, Vec<definition::Instruction>, Vec<NodeID>, Vec<NodeID>),
    Entry(NodeID, Vec<NodeID>),
    Exit(NodeID, Vec<NodeID>),
}

impl Node {
    pub fn add_successor(&mut self, succ: &NodeID) {
        match self {
            Node::BasicBlock(_, _, _, ref mut succs) |
            Node::Entry(_, ref mut succs)  => succs.push(succ.clone()),
            Node::Exit(_, _) => panic!("Cannot add a successor to the exit node"),
        }
    }

    pub fn add_predecessor(&mut self, pred: NodeID) {
        match self {
            Node::BasicBlock(_, _, ref mut preds, _) |
            Node::Exit(_, ref mut preds) => preds.push(pred),
            Node::Entry(_, _) => panic!("Cannot add a predecessor to the entry node"),
        }
    }

    pub fn get_successors(&self) -> &Vec<NodeID> {
        match self {
            Node::BasicBlock(_, _, _, succs) |
            Node::Entry(_, succs) => succs,
            Node::Exit(_, _) => panic!("Cannot get successors of the exit node"),
        }
    }

    pub fn get_predecessors(&self) -> &Vec<NodeID> {
        match self {
            Node::BasicBlock(_, _, preds, _) |
            Node::Exit(_, preds) => preds,
            Node::Entry(_, _) => panic!("Cannot get predecessors of the entry node"),
        }
    }

    pub fn get_id(&self) -> NodeID {
        match self {
            Node::BasicBlock(id, _, _, _) |
            Node::Entry(id, _) |
            Node::Exit(id, _) => id.clone(),
        }
    }

    pub fn get_instructions(&self) -> Vec<definition::Instruction> {
        match self {
            Node::BasicBlock(_, instr, _, _) => instr.clone(),
            _ => panic!("Expected basic block found {:?}", self),
        }
    }
}

pub fn get_block_by_id(id: &NodeID, nodes: &Vec<Node>) -> Node {
    for node in nodes {
        if &node.get_id() == id {
            return node.clone();
        }
    }

    panic!("Block not found: {:?}", id);
}

#[derive(Clone, Debug)]
pub struct CFG {
    pub nodes: Vec<Node>,
}

impl CFG {
    pub fn sort(&mut self) {
        self.nodes.sort_by(|node_a, node_b| {
            let a_id = node_a.get_id();
            let b_id = node_b.get_id();
            if a_id == NodeID::ENTRY || b_id == NodeID::EXIT {
                return std::cmp::Ordering::Less;
            }
            if b_id == NodeID::ENTRY || a_id == NodeID::EXIT {
                return std::cmp::Ordering::Greater;
            }
            let a_id = match a_id {
                NodeID::BlockID(id) => id,
                _ => unreachable!(),
            };
            let b_id = match b_id {
                NodeID::BlockID(id) => id,
                _ => unreachable!(),
            };
            a_id.cmp(&b_id)
        });
    }
}

#[allow(unused_variables)]
pub fn create_cfg(func: &Vec<definition::Instruction>) -> CFG {
    let basic_blocks = partition_into_basic_blocks(func);
    let nodes = convert_to_basic_blocks(basic_blocks);

    let nodes = add_all_edges(&nodes);

    CFG {
        nodes,
    }
}

fn convert_to_map(nodes: &Vec<Node>) -> HashMap<NodeID, Node> {
    let mut map = HashMap::new();

    for node in nodes {
        map.insert(node.get_id(), node.clone());
    }

    map
}

fn partition_into_basic_blocks(instructions: &Vec<definition::Instruction>) -> Vec<Vec<definition::Instruction>> {
    let mut finished_blocks = Vec::new();
    let mut current_block  = Vec::new();
    for instr in instructions {
        match instr {
            definition::Instruction::Label(_) => {
                if current_block.len() > 0 {
                    finished_blocks.push(current_block);
                }
                current_block = vec![instr.clone()];
            },
            definition::Instruction::Jump(_) |
            definition::Instruction::JumpIfZero(_, _) |
            definition::Instruction::JumpIfNotZero(_, _) |
            definition::Instruction::Return(_) => {
                current_block.push(instr.clone());
                finished_blocks.push(current_block);
                current_block = vec![];
            }
            other => current_block.push(other.clone()),
        }
    }

    finished_blocks
}

fn convert_to_basic_blocks(blocks: Vec<Vec<definition::Instruction>>) -> Vec<Node> {
    let mut nodes = Vec::with_capacity(blocks.len() + 2); // +2 for entry and exit

    nodes.push(Node::Entry(NodeID::ENTRY, Vec::new()));

    for block in blocks {
        let block_id = NodeID::BlockID(nodes.len());
        
        nodes.push(Node::BasicBlock(block_id, block, Vec::new(), Vec::new()));
    }

    nodes.push(Node::Exit(NodeID::EXIT, Vec::new()));

    nodes
}

fn get_block_id_by_label(label: &str, nodes: &Vec<Node>) -> NodeID {
    for node in nodes {
        match node {
            Node::BasicBlock(id, instructions, _, _) => {
                match instructions.first().unwrap() {
                    definition::Instruction::Label(lbl) => {
                        if lbl == label {
                            return id.clone();
                        }
                    }
                    _ => (),
                }
            },
            _ => (),
        }
    }

    panic!("Label not found");
}

fn add_all_edges(nodes_vec: &Vec<Node>) -> Vec<Node> {
    let mut nodes = convert_to_map(nodes_vec);

    nodes.get_mut(&NodeID::ENTRY).unwrap().add_successor(&NodeID::BlockID(1));
    nodes.get_mut(&NodeID::BlockID(1)).unwrap().add_predecessor(NodeID::ENTRY);

    let nodes_len = nodes.len();

    let mut new_nodes = nodes.clone();

    for (id, node) in nodes {
        let instructions = match node {
            Node::BasicBlock(_, instructions, _, _) => instructions,
            Node::Entry(_, _) | Node::Exit(_, _) => continue,
        };
        let node_id = id;
        let id = match id {
            NodeID::BlockID(id) => id,
            _ => unreachable!(),
        };

        let next_id = if id == nodes_len - 2 {
            NodeID::EXIT
        } else {
            NodeID::BlockID(id + 1)
        };

        let instr = instructions.last().unwrap();

        let node = new_nodes.get_mut(&node_id).unwrap();
        match instr {
            definition::Instruction::Return(_) => {
                node.add_successor(&NodeID::EXIT);
                // add pred to exit
                new_nodes.get_mut(&NodeID::EXIT).unwrap().add_predecessor(node_id);
            },
            definition::Instruction::Jump(lbl) => {
                let target = get_block_id_by_label(lbl, nodes_vec);

                node.add_successor(&target);
                new_nodes.get_mut(&target).unwrap().add_predecessor(node_id);
            },
            definition::Instruction::JumpIfZero(_, lbl) |
            definition::Instruction::JumpIfNotZero(_, lbl) => {
                let target = get_block_id_by_label(lbl, nodes_vec);

                // Add the next block as a predecessor to the target block
                let next_id = if id == nodes_len - 2 {
                    NodeID::EXIT
                } else {
                    NodeID::BlockID(id + 1)
                };

                node.add_successor(&target);
                node.add_successor(&next_id);

                new_nodes.get_mut(&target).unwrap().add_predecessor(node_id);
                new_nodes.get_mut(&next_id).unwrap().add_predecessor(node_id);
            },
            _ => {
                node.add_successor(&next_id);
                new_nodes.get_mut(&next_id).unwrap().add_predecessor(node_id);
            }
        }
    }

    new_nodes.values().cloned().collect()
}

pub fn cfg_to_func_def(cfg: CFG) -> Vec<definition::Instruction> {
    let mut body = Vec::new();

    for node in cfg.nodes {
        match node {
            Node::BasicBlock(_, instructions, _, _) => {
                body.extend(instructions);
            },
            _ => (),
        }
    }

    body
}