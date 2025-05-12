mod options;

use crate::basic_types::Inconsistency::Conflict;
use crate::basic_types::{HashMap, HashSet};
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::{
    EnqueueDecision, LocalId, PropagationContext, PropagationContextMut, Propagator, ReadDomains,
};
use crate::engine::{DomainEvents, EmptyDomain};
use crate::predicates::PropositionalConjunction;
use crate::variables::IntegerVariable;
use fnv::{FnvBuildHasher, FnvHashMap, FnvHashSet};
use log::warn;
use mdd_compile::mdd::{MddEdge, MddGraph, MddNode};
pub use options::*;
use std::cmp::PartialEq;
use std::collections::hash_set::Iter;
use std::collections::VecDeque;

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum EdgeStatus {
    Alive,
    Dom,
    Above,
    Below,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) enum EdgeWatchFlag {
    Value,
    Begin,
    End,
}
pub(crate) struct MddPropagator<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> {
    mdd: MddGraph<Var>,

    /// The value at index `i` is the domain for `x[i]`
    current_domains: Vec<HashSet<i32>>,
    domain_changes: Vec<(Var, i32)>,
    trail: Vec<MddEdge>,
    limit: HashMap<(Var, i32), i32>,
    edge_status: HashMap<MddEdge, EdgeStatus>,
    watched: HashMap<MddEdge, HashSet<EdgeWatchFlag>>,
    var_value_to_edges: HashMap<(Var, i32), HashSet<MddEdge>>,
    var_value_to_watched_edge: HashMap<(Var, i32), MddEdge>,
    node_to_watched_in_edge: HashMap<MddNode, MddEdge>,
    node_to_watched_out_edge: HashMap<MddNode, MddEdge>,
    node_to_in_edges: HashMap<MddNode, HashSet<MddEdge>>,
    node_to_out_edges: HashMap<MddNode, HashSet<MddEdge>>,
}

impl<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> MddPropagator<Var>
where
    //todo check this
    Var: IntegerVariable,
{
    pub(crate) fn new(mdd: MddGraph<Var>) -> Self {
        Self {
            mdd,
            current_domains: Vec::new(),
            domain_changes: Vec::new(),
            trail: Vec::new(),
            limit: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            edge_status: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            watched: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            var_value_to_edges: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            var_value_to_watched_edge: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            node_to_watched_in_edge: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            node_to_watched_out_edge: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            node_to_in_edges: FnvHashMap::with_hasher(FnvBuildHasher::default()),
            node_to_out_edges: FnvHashMap::with_hasher(FnvBuildHasher::default()),
        }
    }

    fn downward_pass(&mut self, kfa: HashSet<MddNode>) -> HashSet<(Var, i32)> {
        let mut pinf = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut node_queue = VecDeque::from_iter(kfa.iter());

        while !node_queue.is_empty() {
            let node = node_queue.pop_front().unwrap();

            // Find support to replace watcher edger for the node
            for edge in self.node_to_in_edges.get(node).unwrap() {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Alive {
                    let _ = self
                        .watched
                        .get_mut(self.node_to_watched_in_edge.get(node).unwrap())
                        .unwrap()
                        .remove(&EdgeWatchFlag::End);
                    let _ = self
                        .watched
                        .get_mut(edge)
                        .unwrap()
                        .insert(EdgeWatchFlag::End);
                    let _ = self.node_to_watched_in_edge.insert(*node, *edge);
                    break;
                }
            }

            // No support found meaning it is dead, i.e., all of its incoming edges are killed
            // If the node is dead, kill all of its outgoing edges
            if self.is_node_dead(self.node_to_in_edges.get(node).unwrap().iter()) {
                for edge in self.node_to_out_edges.get(node).unwrap() {
                    if self.edge_status.get(edge).unwrap() != &EdgeStatus::Alive {
                        continue;
                    }
                    let _ = self.edge_status.insert(*edge, EdgeStatus::Above);
                    let _ = self.trail.push(*edge);
                    // Queue node that watching the current edge as its incoming edge to process it
                    if self
                        .watched
                        .get(edge)
                        .unwrap()
                        .contains(&EdgeWatchFlag::End)
                    {
                        let _ = node_queue.push_back(&edge.to);
                    }

                    // If a (var, val) watches the edge, add it to be processed by collect fn
                    if self
                        .watched
                        .get(edge)
                        .unwrap()
                        .contains(&EdgeWatchFlag::Value)
                    {
                        let _ = pinf.insert((self.mdd.layers[edge.from.layer].clone(), edge.value));
                    }
                }
            }
        }
        pinf
    }

    fn upward_pass(&mut self, kfb: HashSet<MddNode>) -> HashSet<(Var, i32)> {
        let mut pinf = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut node_queue = VecDeque::from_iter(kfb.iter());

        while !node_queue.is_empty() {
            let node = node_queue.pop_front().unwrap();

            // Find support to replace watcher edger for the node
            for edge in self.node_to_out_edges.get(node).unwrap() {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Alive {
                    let _ = self
                        .watched
                        .get_mut(self.node_to_watched_in_edge.get(node).unwrap())
                        .unwrap()
                        .remove(&EdgeWatchFlag::Begin);
                    let _ = self
                        .watched
                        .get_mut(edge)
                        .unwrap()
                        .insert(EdgeWatchFlag::Begin);
                    let _ = self.node_to_watched_out_edge.insert(*node, *edge);
                    break;
                }
            }

            // No support found meaning it is dead, i.e., all of its outgoing edges are killed
            // If the node is dead, kill all of its incoming edges
            if self.is_node_dead(self.node_to_out_edges.get(node).unwrap().iter()) {
                for edge in self.node_to_in_edges.get(node).unwrap() {
                    if self.edge_status.get(edge).unwrap() != &EdgeStatus::Alive {
                        continue;
                    }
                    let _ = self.edge_status.insert(*edge, EdgeStatus::Below);
                    let _ = self.trail.push(*edge);
                    // Queue node that watching the current edge as its incoming edge to process it
                    if self
                        .watched
                        .get(edge)
                        .unwrap()
                        .contains(&EdgeWatchFlag::End)
                    {
                        let _ = node_queue.push_back(&edge.from);
                    }

                    // If a (var, val) watches the edge, add it to be processed by collect fn
                    if self
                        .watched
                        .get(edge)
                        .unwrap()
                        .contains(&EdgeWatchFlag::Value)
                    {
                        let _ = pinf.insert((self.mdd.layers[edge.from.layer].clone(), edge.value));
                    }
                }
            }
        }
        pinf
    }

    fn is_node_dead(&self, edges: Iter<MddEdge>) -> bool {
        for edge in edges {
            if *self.edge_status.get(&edge).unwrap() == EdgeStatus::Alive {
                return false;
            }
        }
        true
    }

    fn collect_and_propagate(
        &mut self,
        pinf: HashSet<(Var, i32)>,
        count: i32,
        mut context: PropagationContextMut,
    ) -> Result<(), EmptyDomain> {
        let mut inf: HashSet<(Var, i32)> = FnvHashSet::with_hasher(FnvBuildHasher::default());

        for (var, val) in pinf {
            let mut edge = self
                .var_value_to_watched_edge
                .get(&(var.clone(), val))
                .unwrap();
            // Find new support edge for the value to watch
            for e in self.var_value_to_edges.get(&(var.clone(), val)).unwrap() {
                if self.edge_status.get(e).unwrap() == &EdgeStatus::Alive {
                    let _ = self
                        .watched
                        .get_mut(edge)
                        .unwrap()
                        .remove(&EdgeWatchFlag::Value);
                    let _ = self
                        .watched
                        .get_mut(e)
                        .unwrap()
                        .insert(EdgeWatchFlag::Value);
                    let _ = self
                        .var_value_to_watched_edge
                        .insert((var.clone(), val), *e);
                    edge = e;
                    break;
                }
            }
            // No support found meaning the (var, val) can be propagated
            if self.edge_status.get(edge).unwrap() != &EdgeStatus::Alive {
                let _ = inf.insert((var.clone(), val));
                let _ = self.limit.insert((var.clone(), val), count);
            }
        }

        // Propagate
        for (var, val) in inf {
            //TODO: explanation
            context.remove(&var, val, PropositionalConjunction::new(vec![]))?;
        }
        Ok(())
    }

    #[allow(dead_code)] // TODO function might be useful
    fn restore_to(&mut self, var: Var, val: i32) {
        let limit = self.limit.get(&(var, val)).unwrap();
        while self.trail.len() > *limit as usize {
            let edge = self.trail.pop().unwrap();
            let _ = self.edge_status.insert(edge, EdgeStatus::Alive);
        }
    }

    fn check_path_from_source_to_sink(&self) -> Result<(), PropositionalConjunction> {
        let mut node_queue = VecDeque::new();
        node_queue.push_back(MddNode::source());
        while !node_queue.is_empty() {
            let node = node_queue.pop_front().unwrap();
            if node == self.mdd.sink {
                return Ok(());
            }
            for edge in self.node_to_out_edges.get(&node).unwrap() {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Alive {
                    node_queue.push_back(edge.to);
                }
            }
        }
        // TODO explanation
        Err(PropositionalConjunction::new(vec![]))
    }
}

impl<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> Propagator
    for MddPropagator<Var>
where
    //todo check this
    Var: IntegerVariable,
{
    fn name(&self) -> &str {
        "DecisionDiagram"
    }

    fn debug_propagate_from_scratch(
        &self,
        _context: PropagationContextMut,
    ) -> crate::basic_types::PropagationStatusCP {
        todo!()
    }

    fn propagate(
        &mut self,
        _context: PropagationContextMut,
    ) -> crate::basic_types::PropagationStatusCP {
        let mut kfb: HashSet<MddNode> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut kfa: HashSet<MddNode> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut pinf: HashSet<(Var, i32)> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let count: i32 = self.trail.len() as i32;

        for (x_i, value) in self.domain_changes.iter() {
            // Mark restoration point
            let _ = self.limit.insert((x_i.clone(), *value), count);

            // Kill remaining edges for current (x_i, val) as val is no longer in the domain of x_i
            for edge in self.var_value_to_edges.get(&(x_i.clone(), *value)).unwrap() {
                if *self.edge_status.get(edge).unwrap() != EdgeStatus::Alive {
                    continue;
                }
                let _ = self.edge_status.insert(*edge, EdgeStatus::Dom);
                self.trail.push(*edge);
                if (*self.watched.get(edge).unwrap()).contains(&EdgeWatchFlag::Begin) {
                    let _ = kfb.insert(edge.from);
                }

                if (*self.watched.get(edge).unwrap()).contains(&EdgeWatchFlag::End) {
                    let _ = kfa.insert(edge.to);
                }
            }
        }
        let _ = pinf.extend(self.downward_pass(kfa));

        if *self
            .edge_status
            .get(self.node_to_watched_in_edge.get(&self.mdd.sink).unwrap())
            .unwrap()
            != EdgeStatus::Alive
        {
            //TODO explanation
            return Err(Conflict(PropositionalConjunction::new(vec![])));
        }

        let _ = pinf.extend(self.upward_pass(kfb));
        self.domain_changes = Vec::new();
        self.collect_and_propagate(pinf, count, _context)?;
        Ok(())
    }

    fn notify(
        &mut self,
        _context: PropagationContextWithTrailedValues,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let index = _local_id.unpack() as usize;
        let x_i = &self.mdd.layers[index];

        let old_domain_set = self.current_domains[index].clone();
        let new_domain_set = _context.iterate_domain(x_i).collect::<HashSet<i32>>();

        let values_removed = old_domain_set.difference(&new_domain_set);
        values_removed.into_iter().for_each(|x| {
            self.domain_changes.push((x_i.clone(), *x));
        });
        EnqueueDecision::Enqueue
    }
    fn notify_backtrack(
        &mut self,
        _context: PropagationContext,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) {
        let index = _local_id.unpack() as usize;
        let x_i = self.mdd.layers[index].clone();

        let old_domain_set = self.current_domains[index].clone();
        let new_domain_set = _context.iterate_domain(&x_i).collect::<HashSet<i32>>();

        let values_returned = old_domain_set.difference(&new_domain_set);
        values_returned
            .into_iter()
            .for_each(|val| self.restore_to(x_i.clone(), *val));
    }

    fn initialise_at_root(
        &mut self,
        _context: &mut crate::engine::propagation::PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        warn!(
            "MDD with variables {:?} and {} transitions; not implemented yet",
            self.mdd.layers,
            self.mdd.transitions.len()
        );

        // self.mdd.layers.iter().enumerate().for_each(|(i, x_i)| {
        //     let _ = _context.register(x_i.clone(), DomainEvents::ANY_INT, LocalId::from(i as u32));
        //     let domain_set = _context.iterate_domain(x_i).collect::<HashSet<i32>>();
        //     self.current_domains.push(domain_set)
        // });
        // 
        // self.mdd.transitions.iter().for_each(|edge| {
        //     let _ = self
        //         .node_to_in_edges
        //         .entry(edge.to)
        //         .or_insert(FnvHashSet::with_hasher(FnvBuildHasher::default()))
        //         .insert(edge.clone());
        //     let _ = self
        //         .node_to_out_edges
        //         .entry(edge.from)
        //         .or_insert(FnvHashSet::with_hasher(FnvBuildHasher::default()))
        //         .insert(edge.clone());
        //     let _ = self
        //         .var_value_to_edges
        //         .entry((self.mdd.layers[edge.from.layer].clone(), edge.value))
        //         .or_insert(FnvHashSet::with_hasher(FnvBuildHasher::default()))
        //         .insert(edge.clone());
        //     let _ = self.edge_status.insert(edge.clone(), EdgeStatus::Alive);
        //     let _ = self.watched.insert(
        //         edge.clone(),
        //         FnvHashSet::with_hasher(FnvBuildHasher::default()),
        //     );
        // });
        // 
        // self.node_to_in_edges.keys().for_each(|node| {
        //     let incoming_edge = *self
        //         .node_to_in_edges
        //         .get(node)
        //         .unwrap()
        //         .iter()
        //         .nth(0)
        //         .unwrap();
        //     let _ = self.node_to_watched_in_edge.insert(*node, incoming_edge);
        //     let _ = self
        //         .watched
        //         .get_mut(&incoming_edge)
        //         .unwrap()
        //         .insert(EdgeWatchFlag::End);
        // });
        // 
        // self.node_to_out_edges.keys().for_each(|node| {
        //     let outgoing_edge = *self
        //         .node_to_out_edges
        //         .get(node)
        //         .unwrap()
        //         .iter()
        //         .nth(0)
        //         .unwrap();
        //     let _ = self.node_to_watched_out_edge.insert(*node, outgoing_edge);
        //     let _ = self
        //         .watched
        //         .get_mut(&outgoing_edge)
        //         .unwrap()
        //         .insert(EdgeWatchFlag::Begin);
        // });
        // 
        // self.var_value_to_edges.keys().for_each(|var_value| {
        //     let edge = *self
        //         .var_value_to_edges
        //         .get(var_value)
        //         .unwrap()
        //         .iter()
        //         .nth(0)
        //         .unwrap();
        //     let _ = self
        //         .var_value_to_watched_edge
        //         .insert(var_value.clone(), edge);
        //     let _ = self
        //         .watched
        //         .get_mut(&edge)
        //         .unwrap()
        //         .insert(EdgeWatchFlag::Value);
        // });
        // 
        // self.check_path_from_source_to_sink()?;
        Ok(())
    }
}
