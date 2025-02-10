use itertools::Itertools;
use petgraph::algo::floyd_warshall;
use petgraph::algo::NegativeCycle;
use petgraph::Directed;
use petgraph::Graph;

use crate::rcpsp_instance::Precedence;
use crate::rcpsp_instance::RcpspInstance;

pub(crate) struct PrecedenceClosure {
    precedences: Vec<Precedence>,
    processing_times: Vec<u32>,
}

impl PrecedenceClosure {
    pub(crate) fn new(rcpsp_instance: &RcpspInstance) -> Option<Self> {
        let precedence_closure = match distance_closure(
            rcpsp_instance.processing_times.len(),
            &rcpsp_instance
                .dependencies
                .iter()
                .flat_map(|(_, v)| v.iter())
                .cloned()
                .collect_vec(),
        ) {
            Ok(closure) => Some(closure),
            Err(NegativeCycle(_)) => {
                println!("Unsatisfiable");
                None
            }
        }?;
        Some(Self {
            precedences: precedence_closure,
            processing_times: rcpsp_instance.processing_times.to_vec(),
        })
    }

    pub(crate) fn num_edges(&self) -> usize {
        self.precedences
            .iter()
            .filter(|precedence| {
                // Simply check whether the precedence relation is large enough to be disjoint
                precedence.gap >= self.processing_times[precedence.successor] as i32
            })
            .count()
    }

    pub(crate) fn contains_edge(&self, node1: usize, node2: usize) -> bool {
        self.precedences.iter().any(|precedence| {
            // Check whether the precedence contains either of the two edges and the distance is
            // larger than the processing times of the predecessor
            ((precedence.predecessor == node1 && precedence.successor == node2)
                || (precedence.predecessor == node2 && precedence.successor == node1))
                && precedence.gap >= self.processing_times[precedence.predecessor] as i32
        })
    }

    pub(crate) fn get_incoming_edges(&self, node: usize) -> impl Iterator<Item = &Precedence> {
        self.precedences
            .iter()
            .filter(move |precedence| precedence.successor == node && precedence.gap.is_positive())
    }

    pub(crate) fn precedences(&self) -> impl Iterator<Item = &Precedence> {
        self.precedences.iter()
    }
}

fn distance_closure(
    n_tasks: usize,
    dependencies: &[Precedence],
) -> Result<Vec<Precedence>, NegativeCycle> {
    let mut graph = Graph::<usize, i32, Directed>::new();
    let nodes = (0..n_tasks).map(|ix| graph.add_node(ix)).collect_vec();
    for &d in dependencies {
        let _ = graph.add_edge(nodes[d.predecessor], nodes[d.successor], -d.gap);
    }
    let closure = floyd_warshall(&graph, |edge| *edge.weight())?;
    let mut result = vec![];
    for ((from, to), weight) in closure {
        let predecessor = graph
            .node_weight(from)
            .cloned()
            .expect("cannot find the graph node index");
        let successor = graph
            .node_weight(to)
            .cloned()
            .expect("cannot find the graph node index");
        result.push(Precedence {
            predecessor,
            successor,
            gap: -weight,
        });
    }
    Ok(result)
}
