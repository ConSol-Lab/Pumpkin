mod options;
use crate::basic_types::Inconsistency::Conflict;
use crate::basic_types::{HashMap, HashSet};
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::{
    EnqueueDecision, LocalId, PropagationContext, PropagationContextMut, Propagator, ReadDomains,
};
use crate::engine::{DomainEvents, EmptyDomain};
use crate::predicate;
use crate::predicates::{Predicate, PropositionalConjunction};
use crate::variables::IntegerVariable;
use fnv::{FnvBuildHasher, FnvHashMap, FnvHashSet};
use mdd_compile::mdd::{MddEdge, MddGraph, MddNode};
pub use options::*;
use std::cmp::PartialEq;
use std::collections::hash_set::Iter;
use std::collections::VecDeque;

/// Enum to represent the status of an edge in the MDD
#[derive(Debug, Eq, PartialEq)]
pub(crate) enum EdgeStatus {
    /// Edge is alive
    Alive,
    /// Edge is killed due to a domain change
    Dom,
    /// Edge is killed from above (due to downward pass)
    Above,
    /// Edge is killed from below (due to upward pass)
    Below,
}

/// Enum to represent the edge watch flags for the edge watching scheme proposed by \[1\].
///
/// \[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) enum EdgeWatchFlag {
    /// Edge is being watched by a value, i.e. (var, val)
    Value,
    /// Edge is being watched as an outgoing edge for a node
    Begin,
    /// Edge is being watched as an incoming edge for a node
    End,
}

/// ['MddPropagator'] is a propagator that uses provided multi-valued decision diagram (MDD) to propagate
/// the constraint represented by the MDD (see ['mdd_compile::mdd']).
///
/// The propagator uses incremental propagation and explanations algorithms from \[1\].
///
/// \[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
pub(crate) struct MddPropagator<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> {
    mdd: MddGraph<Var>,

    /// The current state of the MDD, represented by domains of the variables that it involves
    ///
    /// The value at index `i` is the domain for `x[i]`
    current_domains: Vec<HashSet<i32>>,
    /// The domain changes that have been made since the last propagation, to be accounted for
    /// the next propagation.
    domain_changes: Vec<(Var, i32)>,
    /// The trail of edges that have been killed during the propagation
    trail: Vec<MddEdge>,
    /// For each (var, val) pair that removed due to inference, the limit keeps track the trail position
    /// before the propagation where the (var, val) was removed.
    ///
    /// This is used to restore the state of the MDD when backtracking occurs.
    limit: HashMap<(Var, i32), i32>,
    /// The status of each edge in the MDD at the current state.
    edge_status: HashMap<MddEdge, EdgeStatus>,
    /// Tracks each edge and their watch flags at the current state of the MDD.
    watched: HashMap<MddEdge, HashSet<EdgeWatchFlag>>,
    /// Maps between each (var, val) pair and the edges that are related to it.
    var_value_to_edges: HashMap<(Var, i32), HashSet<MddEdge>>,
    /// Tracks between each (var, val) pair and the edge that it is watching at the current state of the MDD.
    var_value_to_watched_edge: HashMap<(Var, i32), MddEdge>,
    /// Tracks between each node and the incoming edge it is watching at the current state of the MDD.
    node_to_watched_in_edge: HashMap<MddNode, MddEdge>,
    /// Tracks between each node and the outgoing edge it is watching at the current state of the MDD.
    node_to_watched_out_edge: HashMap<MddNode, MddEdge>,
    /// Maps between each node and its incoming edges.
    node_to_in_edges: HashMap<MddNode, HashSet<MddEdge>>,
    /// Maps between each node and its outgoing edges.
    node_to_out_edges: HashMap<MddNode, HashSet<MddEdge>>,
    /// Maps between each variable and its index in the MDD layers.
    var_to_index: HashMap<Var, usize>,
}

impl<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> MddPropagator<Var>
where
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
            var_to_index: FnvHashMap::with_hasher(FnvBuildHasher::default()),
        }
    }

    /// The downward pass of the MDD incremental propagation algorithm from \[1\].
    /// For each node *potentially* killed from above, check if it is actually killed/dead by finding the support edge to replace the watcher edge for the node.
    /// Also, process children of the node to check if they are also killed from below.
    ///
    /// Returns a set of (var, val) pairs that should potentially be propagated due to the corresponding watched edge being killed.
    ///
    /// \[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
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
            if self.is_node_dead(self.node_to_in_edges.get(node).unwrap().iter())
                && node.layer < self.mdd.layers.len()
            {
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

    /// The upward pass of the MDD incremental propagation algorithm from \[1\].
    /// For each node *potentially* killed from below, check if it is actually killed/dead by finding the support edge to replace the watcher edge for the node.
    /// Also, process parent of the node to check if they are also killed from below.
    ///
    /// Returns a set of (var, val) pairs that should potentially be propagated due to the corresponding watched edge being killed.
    ///
    ///[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
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
                        .get_mut(self.node_to_watched_out_edge.get(node).unwrap())
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
            if self.is_node_dead(self.node_to_out_edges.get(node).unwrap().iter()) && node.layer > 0
            {
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

    /// Check if a node is dead by checking if all of its given (incoming or outgoing) edges are dead.
    fn is_node_dead(&self, edges: Iter<MddEdge>) -> bool {
        for edge in edges {
            if *self.edge_status.get(&edge).unwrap() == EdgeStatus::Alive {
                return false;
            }
        }
        true
    }

    /// Given (var, val) pairs that should be potentially propagated due to their corresponding watched edge being killed from above or below,
    /// check if all the edges corresponding to the (var, val) pairs are dead via the watching scheme of \[1\].
    ///
    /// Propagates the removal of (var, val) that are determined to be "dead".
    ///
    ///[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
    fn collect_and_propagate(
        &mut self,
        pinf: HashSet<(Var, i32)>,
        count: i32,
        mut context: PropagationContextMut,
    ) -> Result<(), EmptyDomain> {
        let mut inf: HashSet<(Var, i32)> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        // TODO optimize further memoization so that it is memoized for the entirety of the solver and capable of restoring upon backtrack
        let mut killed_above_memo: HashMap<MddNode, bool> =
            FnvHashMap::with_hasher(FnvBuildHasher::default());
        let mut killed_below_memo: HashMap<MddNode, bool> =
            FnvHashMap::with_hasher(FnvBuildHasher::default());

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
            let reason = self.explain(
                var.clone(),
                val,
                context.as_readonly(),
                &mut killed_below_memo,
                &mut killed_above_memo,
            );
            context.remove(&var, val, reason)?;
        }
        Ok(())
    }

    /// Function to restore the state of the MDD when backtracking occurs.
    fn restore_to(&mut self, var: Var, val: i32) {
        let limit = self.limit.get(&(var, val)).unwrap();
        while self.trail.len() > *limit as usize {
            let edge = self.trail.pop().unwrap();
            let _ = self.edge_status.insert(edge, EdgeStatus::Alive);
        }
    }

    /// Performs BFS to check if there is a path from the source to the sink in the MDD following
    /// edges that are alive. Used to check if the MDD is valid during initialization.
    fn check_path_from_source_to_sink(
        &self,
        _context: &mut crate::engine::propagation::PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        let mut node_queue = VecDeque::new();
        node_queue.push_back(MddNode::source());
        while !node_queue.is_empty() {
            let node = node_queue.pop_front().unwrap();
            if node == self.mdd.sink {
                return Ok(());
            }
            if let None = self.node_to_out_edges.get(&node) {
                continue;
            }
            for edge in self.node_to_out_edges.get(&node).unwrap() {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Alive {
                    node_queue.push_back(edge.to);
                }
            }
        }
        // Explain with domain
        Err(PropositionalConjunction::new(self.mdd.layers.iter().fold(
            Vec::new(),
            |mut acc, x| {
                acc.extend(vec![
                    predicate![x >= _context.lower_bound(x)],
                    predicate![x <= _context.upper_bound(x)],
                ]);
                acc
            },
        )))
    }

    /// The incremental explanation algorithm from \[1\].
    /// Produces a sufficiently small reason without exploring the whole MDD graph.
    ///
    /// \[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
    fn explain(
        &mut self,
        var: Var,
        val: i32,
        _context: PropagationContext,
        killed_below_memo: &mut HashMap<MddNode, bool>,
        killed_above_memo: &mut HashMap<MddNode, bool>,
    ) -> PropositionalConjunction {
        let mut kfa: HashSet<MddEdge> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut kfb: HashSet<MddEdge> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        for edge in self.var_value_to_edges.get(&(var.clone(), val)).unwrap() {
            if *self.edge_status.get(edge).unwrap() == EdgeStatus::Above {
                let _ = kfa.insert(*edge);
            } else if *self.edge_status.get(edge).unwrap() == EdgeStatus::Below {
                let _ = kfb.insert(*edge);
            }
        }
        let mut predicates = Vec::new();
        predicates.extend(self.explain_down(kfb, killed_above_memo));
        predicates.extend(self.explain_up(kfa, killed_below_memo));
        PropositionalConjunction::new(predicates)
    }

    /// Produces explanations for edges related to the propagated (var, val) removal that are killed from bellow.
    fn explain_down(
        &mut self,
        kfb: HashSet<MddEdge>,
        killed_below_memo: &mut HashMap<MddNode, bool>,
    ) -> Vec<Predicate> {
        let mut reason_predicates = Vec::new();
        let mut reason: HashSet<(Var, i32)> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut current_kfb = kfb.clone();
        while !current_kfb.is_empty() {
            let mut pending: HashSet<MddEdge> = FnvHashSet::with_hasher(FnvBuildHasher::default());
            for edge in current_kfb.iter() {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Dom
                    && !self.killed_below(edge.to, killed_below_memo)
                {
                    let var = self.mdd.layers[edge.from.layer].clone();
                    // TODO: check if this relates to minimal reason (value exclusion) or we need to provide the entire domain
                    reason_predicates.push(predicate![var != edge.value]);
                    let _ = reason.insert((var, edge.value));
                } else {
                    let _ = pending.insert(*edge);
                }
            }

            let mut next_kfb = FnvHashSet::with_hasher(FnvBuildHasher::default());
            for edge in pending.iter() {
                if !reason.contains(&(self.mdd.layers[edge.from.layer].clone(), edge.value)) {
                    if let Some(out_edges) = self.node_to_out_edges.get(&edge.to) {
                        next_kfb.extend(out_edges);
                    }
                }
            }
            current_kfb = next_kfb;
        }
        reason_predicates
    }

    /// Checks if a node is killed from below or above
    ///
    /// Results are memoized for each propagation call
    fn killed_below(
        &mut self,
        node: MddNode,
        killed_below_memo: &mut HashMap<MddNode, bool>,
    ) -> bool {
        if let Some(result) = killed_below_memo.get(&node) {
            return *result;
        }
        if let Some(out_edges) = self.node_to_out_edges.get(&node) {
            for edge in out_edges {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Alive
                    || *self.edge_status.get(edge).unwrap() == EdgeStatus::Above
                {
                    let _ = killed_below_memo.insert(node, false);
                    return false;
                }
            }
        }
        let _ = killed_below_memo.insert(node, true);
        true
    }

    /// Produces explanations for edges related to the propagated (var, val) removal that are killed from above.
    fn explain_up(
        &mut self,
        kfa: HashSet<MddEdge>,
        killed_above_memo: &mut HashMap<MddNode, bool>,
    ) -> Vec<Predicate> {
        let mut reason_predicates = Vec::new();
        let mut reason: HashSet<(Var, i32)> = FnvHashSet::with_hasher(FnvBuildHasher::default());
        let mut current_kfa = kfa.clone();
        while !current_kfa.is_empty() {
            let mut pending: HashSet<MddEdge> = FnvHashSet::with_hasher(FnvBuildHasher::default());
            for edge in current_kfa.iter() {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Dom
                    && !self.killed_above(edge.from, killed_above_memo)
                {
                    let var = self.mdd.layers[edge.from.layer].clone();
                    // TODO: check if this relates to minimal reason (value exclusion) or we need to provide the entire domain
                    reason_predicates.push(predicate![var != edge.value]);
                    let _ = reason.insert((var, edge.value));
                } else {
                    let _ = pending.insert(*edge);
                }
            }

            let mut next_kfa = FnvHashSet::with_hasher(FnvBuildHasher::default());
            for edge in pending.iter() {
                if !reason.contains(&(self.mdd.layers[edge.from.layer].clone(), edge.value)) {
                    if let Some(in_edges) = self.node_to_in_edges.get(&edge.from) {
                        next_kfa.extend(in_edges);
                    }
                }
            }
            current_kfa = next_kfa;
        }
        reason_predicates
    }

    /// Checks if a node is killed from above
    ///
    /// Results are memoized for each propagation call
    fn killed_above(
        &mut self,
        node: MddNode,
        killed_above_memo: &mut HashMap<MddNode, bool>,
    ) -> bool {
        if let Some(result) = killed_above_memo.get(&node) {
            return *result;
        }
        if let Some(in_edges) = self.node_to_in_edges.get(&node) {
            for edge in in_edges {
                if *self.edge_status.get(edge).unwrap() == EdgeStatus::Alive
                    || *self.edge_status.get(edge).unwrap() == EdgeStatus::Below
                {
                    let _ = killed_above_memo.insert(node, false);
                    return false;
                }
            }
        }
        let _ = killed_above_memo.insert(node, true);
        true
    }
}

impl<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> Propagator
    for MddPropagator<Var>
where
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

            // Remove the value from the domain of x_i (our state) after processing it
            let _ = self
                .current_domains
                .get_mut(*self.var_to_index.get(x_i).unwrap())
                .unwrap()
                .remove(value);
        }
        let _ = pinf.extend(self.downward_pass(kfa));

        if *self
            .edge_status
            .get(self.node_to_watched_in_edge.get(&self.mdd.sink).unwrap())
            .unwrap()
            != EdgeStatus::Alive
        {
            // If the sink is dead, it is due to watched incoming edge being killed
            let edge_involved = self.node_to_watched_in_edge.get(&self.mdd.sink).unwrap();
            let var_val = (
                self.mdd.layers[edge_involved.from.layer].clone(),
                edge_involved.value,
            );
            return Err(Conflict(self.explain(
                var_val.0.clone(),
                var_val.1,
                _context.as_readonly(),
                &mut FnvHashMap::with_hasher(FnvBuildHasher::default()),
                &mut FnvHashMap::with_hasher(FnvBuildHasher::default()),
            )));
        }

        let _ = pinf.extend(self.upward_pass(kfb));
        self.collect_and_propagate(pinf, count, _context)?;
        // Reset the following at the end of the propagation
        self.domain_changes.clear();
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
        // Assert if the new domain is a subset of the old domain
        assert!(new_domain_set.is_subset(&old_domain_set));
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
        // Assert if the old domain is a subset of the new domain, to backtrack to
        assert!(old_domain_set.is_subset(&new_domain_set));
        let values_returned = new_domain_set.difference(&old_domain_set);
        values_returned
            .into_iter()
            .for_each(|val| self.restore_to(x_i.clone(), *val));
        self.current_domains[index] = new_domain_set;
        self.domain_changes.clear(); // Reset previously recorded domain changes
    }

    fn initialise_at_root(
        &mut self,
        _context: &mut crate::engine::propagation::PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        self.mdd.layers.iter().enumerate().for_each(|(i, x_i)| {
            let _ = _context.register(x_i.clone(), DomainEvents::ANY_INT, LocalId::from(i as u32));
            let _ = _context.register_for_backtrack_events(
                x_i.clone(),
                DomainEvents::ANY_INT,
                LocalId::from(i as u32),
            );
            let domain_set = _context.iterate_domain(x_i).collect::<HashSet<i32>>();
            self.current_domains.push(domain_set);
            let _ = self.var_to_index.insert(x_i.clone(), i);
        });

        self.mdd.transitions.iter().for_each(|edge| {
            let _ = self
                .node_to_in_edges
                .entry(edge.to)
                .or_insert(FnvHashSet::with_hasher(FnvBuildHasher::default()))
                .insert(edge.clone());
            let _ = self
                .node_to_out_edges
                .entry(edge.from)
                .or_insert(FnvHashSet::with_hasher(FnvBuildHasher::default()))
                .insert(edge.clone());
            let _ = self
                .var_value_to_edges
                .entry((self.mdd.layers[edge.from.layer].clone(), edge.value))
                .or_insert(FnvHashSet::with_hasher(FnvBuildHasher::default()))
                .insert(edge.clone());
            let _ = self.edge_status.insert(edge.clone(), EdgeStatus::Alive);
            let _ = self.watched.insert(
                edge.clone(),
                FnvHashSet::with_hasher(FnvBuildHasher::default()),
            );
        });

        self.node_to_in_edges.keys().for_each(|node| {
            let incoming_edge = *self
                .node_to_in_edges
                .get(node)
                .unwrap()
                .iter()
                .nth(0)
                .unwrap();
            let _ = self.node_to_watched_in_edge.insert(*node, incoming_edge);
            let _ = self
                .watched
                .get_mut(&incoming_edge)
                .unwrap()
                .insert(EdgeWatchFlag::End);
        });

        self.node_to_out_edges.keys().for_each(|node| {
            let outgoing_edge = *self
                .node_to_out_edges
                .get(node)
                .unwrap()
                .iter()
                .nth(0)
                .unwrap();
            let _ = self.node_to_watched_out_edge.insert(*node, outgoing_edge);
            let _ = self
                .watched
                .get_mut(&outgoing_edge)
                .unwrap()
                .insert(EdgeWatchFlag::Begin);
        });

        self.var_value_to_edges.keys().for_each(|var_value| {
            let edge = *self
                .var_value_to_edges
                .get(var_value)
                .unwrap()
                .iter()
                .nth(0)
                .unwrap();
            let _ = self
                .var_value_to_watched_edge
                .insert(var_value.clone(), edge);
            let _ = self
                .watched
                .get_mut(&edge)
                .unwrap()
                .insert(EdgeWatchFlag::Value);
        });

        self.check_path_from_source_to_sink(_context)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::mdd::MddPropagator;
    use crate::{conjunction, predicate};
    use mdd_compile::mdd::{MddEdge, MddGraph, MddNode};

    #[test]
    /// Test the MDD propagator with BDD example from \[1\] "for a regular constraint 0\*1100\*110\* over the variables
    /// \[x0, x1, x2, x3, x4, x5, x6\], and the effect of propagating x2 != 1 and x3 != 1",
    ///
    /// \[1\] G. Gange, P. J. Stuckey, and R. Szymanek, “Mdd propagators with explanation,” Constraints, vol. 16, pp. 407–429, 4 Oct. 2011, issn: 13837133. Doi: 10.1007/s10601-011-9111-x
    fn bdd_regular_gange() {
        let mut solver = TestSolver::default();
        let x0 = solver.new_variable(0, 1);
        let x1 = solver.new_variable(0, 1);
        let x2 = solver.new_variable(0, 1);
        let x3 = solver.new_variable(0, 1);
        let x4 = solver.new_variable(0, 1);
        let x5 = solver.new_variable(0, 1);
        let x6 = solver.new_variable(0, 1);
        let layers = vec![x0, x1, x2, x3, x4, x5, x6];
        let sink = MddNode { layer: 7, index: 0 };
        let transitions = vec![
            // layer 0 -> 1
            MddEdge {
                from: MddNode { layer: 0, index: 0 },
                to: MddNode { layer: 1, index: 0 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 0, index: 0 },
                to: MddNode { layer: 1, index: 1 },
                value: 0,
            },
            // layer 1 -> 2
            MddEdge {
                from: MddNode { layer: 1, index: 0 },
                to: MddNode { layer: 2, index: 0 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 1, index: 1 },
                to: MddNode { layer: 2, index: 1 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 1, index: 1 },
                to: MddNode { layer: 2, index: 2 },
                value: 0,
            },
            // layer 2 -> 3
            MddEdge {
                from: MddNode { layer: 2, index: 0 },
                to: MddNode { layer: 3, index: 0 },
                value: 0,
            },
            MddEdge {
                from: MddNode { layer: 2, index: 1 },
                to: MddNode { layer: 3, index: 1 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 2, index: 2 },
                to: MddNode { layer: 3, index: 2 },
                value: 1,
            },
            // layer 3 -> 4
            MddEdge {
                from: MddNode { layer: 3, index: 0 },
                to: MddNode { layer: 4, index: 0 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 3, index: 0 },
                to: MddNode { layer: 4, index: 1 },
                value: 0,
            },
            MddEdge {
                from: MddNode { layer: 3, index: 1 },
                to: MddNode { layer: 4, index: 1 },
                value: 0,
            },
            MddEdge {
                from: MddNode { layer: 3, index: 2 },
                to: MddNode { layer: 4, index: 2 },
                value: 1,
            },
            // layer 4 -> 5
            MddEdge {
                from: MddNode { layer: 4, index: 0 },
                to: MddNode { layer: 5, index: 0 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 4, index: 1 },
                to: MddNode { layer: 5, index: 1 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 4, index: 1 },
                to: MddNode { layer: 5, index: 2 },
                value: 0,
            },
            MddEdge {
                from: MddNode { layer: 4, index: 2 },
                to: MddNode { layer: 5, index: 2 },
                value: 0,
            },
            // layer 5 -> 6
            MddEdge {
                from: MddNode { layer: 5, index: 0 },
                to: MddNode { layer: 6, index: 0 },
                value: 0,
            },
            MddEdge {
                from: MddNode { layer: 5, index: 1 },
                to: MddNode { layer: 6, index: 0 },
                value: 1,
            },
            MddEdge {
                from: MddNode { layer: 5, index: 2 },
                to: MddNode { layer: 6, index: 1 },
                value: 1,
            },
            // layer 6 -> 7 (terminal node T)
            MddEdge {
                from: MddNode { layer: 6, index: 0 },
                to: sink, // Terminal node T
                value: 0,
            },
            MddEdge {
                from: MddNode { layer: 6, index: 1 },
                to: sink, // Terminal node T
                value: 1,
            },
        ];
        let mdd = MddGraph {
            layers: layers.clone(),
            transitions,
            sink,
        };

        let mdd_propagator = solver
            .new_propagator(MddPropagator::new(mdd))
            .expect("No Conflict");
        for x in layers.clone() {
            assert_eq!(solver.lower_bound(x), 0);
            assert_eq!(solver.upper_bound(x), 1);
        }
        let notification_status_1 =
            solver.decrease_upper_bound_and_notify(mdd_propagator, 2, x2, 0);
        assert!(match notification_status_1 {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let notification_status_2 =
            solver.decrease_upper_bound_and_notify(mdd_propagator, 3, x3, 0);
        assert!(match notification_status_2 {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(mdd_propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(x0), 1);
        assert_eq!(solver.upper_bound(x0), 1);
        let reason_0 = solver.get_reason_int(predicate!(x0 != 0));
        assert_eq!(conjunction!([x2 != 1]), reason_0);
        assert_eq!(solver.lower_bound(x1), 1);
        assert_eq!(solver.upper_bound(x1), 1);
        let reason_1 = solver.get_reason_int(predicate!(x1 != 0));
        assert_eq!(conjunction!([x3 != 1]), reason_1);
        assert_eq!(solver.lower_bound(x5), 1);
        assert_eq!(solver.upper_bound(x5), 1);
        let reason_1 = solver.get_reason_int(predicate!(x5 != 0));
        assert_eq!(conjunction!([x3 != 1]), reason_1);
        let unchanged_vars = vec![x4, x6];
        for x in unchanged_vars {
            assert_eq!(solver.lower_bound(x), 0);
            assert_eq!(solver.upper_bound(x), 1);
        }
    }
}
