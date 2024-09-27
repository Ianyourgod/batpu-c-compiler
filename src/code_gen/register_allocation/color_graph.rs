use std::collections::HashSet;

use super::{assembly, is_callee_saved, Graph, Node};

const K: i32 = 11;

fn get_unpruned(graph: &Graph, neighbors: &Vec<assembly::Operand>) -> Vec<(assembly::Operand, Node)> { // (unpruned, pruned)
    let mut unpruned = Vec::new();
    for operand in neighbors {
        let node = graph.nodes.get(operand).unwrap();

        if !node.pruned {
            unpruned.push((operand.clone(), node.clone()));
        }
    }

    unpruned
}
 
// this introduces some randomness. it's not *that* bad, and its unavoidable unless we swap the graph hashmap to a vec
fn graph_to_vec_of_ops(graph: &Graph) -> Vec<assembly::Operand> {
    graph.nodes.keys().cloned().collect()
}

pub fn color_graph(graph: &mut Graph) {
    let unpruned = get_unpruned(&graph, &graph_to_vec_of_ops(graph));

    if unpruned.is_empty() {
        return;
    }

    let mut chosen_node: Option<assembly::Operand> = None;

    for node in &unpruned {
        let degree = get_unpruned(graph, &node.1.neighbors).len();

        if (degree as i32) < K {
            chosen_node = Some(node.0.clone());
            break;
        }
    }

    if chosen_node.is_none() {
        let mut best_spill_metric = f32::INFINITY;

        for node in unpruned {
            let degree = get_unpruned(graph, &node.1.neighbors).len();
            let spill_metric = node.1.spill_cost / degree as f32;
            if spill_metric < best_spill_metric { // || best_spill_metric == f32::INFINITY { // extra clause is so hard registers can still be picked
                chosen_node = Some(node.0);
                best_spill_metric = spill_metric;
            }
        }
    }

    let chosen_node = chosen_node.unwrap();

    graph.nodes.get_mut(&chosen_node).unwrap().pruned = true;

    color_graph(graph);

    let mut colors: HashSet<i32> = (1..K+1).collect();
    for neighbor_id in &graph.nodes.get(&chosen_node).unwrap().neighbors {
        let neighbor = graph.nodes.get(neighbor_id).unwrap();

        if neighbor.color.is_some() {
            colors.remove(&neighbor.color.unwrap());
        }
    }

    if !colors.is_empty() {
        let chosen_node = graph.nodes.get_mut(&chosen_node).unwrap();

        let mut c_vec = colors.into_iter().collect::<Vec<i32>>();
        c_vec.sort();
        chosen_node.color = Some(match chosen_node.id {
            assembly::Operand::Register(ref r) => {
                if is_callee_saved(r) {
                    *c_vec.last().unwrap()
                } else {
                    *c_vec.first().unwrap()
                }
            }
            _ => {
                *c_vec.first().unwrap()
            }
        });

        chosen_node.pruned = false;
    }
}