use pumpkin_core::containers::HashSet as Set;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::state::{Conflict, PropagatorConflict};
use pumpkin_core::variables::IntegerVariable;

use crate::propagators::circuit::graph::Graph;
use crate::propagators::circuit::graph::Inference;

declare_inference_label!(CircuitForwardCheck);

#[derive(Clone, Debug)]
pub struct CircuitConstructor<Var> {
    pub successors: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
    pub conflict_detection_only: bool,
}

impl<Var> PropagatorConstructor for CircuitConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = CircuitPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        // Register for events
        for (index, var) in self.successors.iter().enumerate() {
            context.register(
                var.clone(),
                DomainEvents::ANY_INT,
                LocalId::from(index as u32),
            );
        }

        CircuitPropagator {
            successors: self.successors,
            conflict_detection_only: self.conflict_detection_only,
            inference_code: InferenceCode::new(self.constraint_tag, CircuitForwardCheck),
        }
    }
}

/// Propagator for the Circuit constraint.
#[derive(Clone, Debug)]
pub struct CircuitPropagator<Var> {
    // TODO
    pub successors: Box<[Var]>,
    conflict_detection_only: bool,
    inference_code: InferenceCode,
}

const VALUE_OFFSET: i32 = 1;

impl<Var: 'static> Propagator for CircuitPropagator<Var>
where
    Var: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::VeryLow
    }

    fn name(&self) -> &str {
        "Circuit"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        let mut domains: Vec<Set<usize>> = vec![Set::default(); self.successors.len()];
        for (index, var) in self.successors.iter().enumerate() {
            domains[index].extend(
                context
                    .iterate_domain(var)
                    .map(|value| (value - VALUE_OFFSET) as usize),
            );
        }

        let graph = Graph::from_domains(&domains);

        if let Some(explanation) = graph.circuit_check() {
            debug_assert!(
                explanation.check_holds(&graph).is_ok(),
                "{:#?}\n{:#?}\n{:#?}",
                &explanation,
                &graph,
                &domains
            );
            let reason = explanation.to_propositional_conjunction(&self.successors);

            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction: reason,
                inference_code: self.inference_code.clone(),
            }));
        }

        if self.conflict_detection_only {
            // TODO: Only perform conflict detection
            return Ok(());
        }

        for inference in graph.circuit_prevent() {
            let Inference {
                explanation,
                inference,
                positive,
            } = inference;

            debug_assert!(explanation.check_holds(&graph).is_ok());
            let reason = explanation.to_propositional_conjunction(&self.successors);
            let (from, to) = inference;
            let var = &self.successors[from];

            let p = if positive {
                predicate!(var == to as i32 + VALUE_OFFSET)
            } else {
                predicate!(var != to as i32 + VALUE_OFFSET)
            };

            context.post(p, reason, &self.inference_code)?;
        }

        Ok(())
    }
}

mod graph {
    use pumpkin_core::{
        containers::{HashMap as Map, HashSet as Set},
        predicate,
        predicates::PropositionalConjunction,
        variables::IntegerVariable,
    };

    use crate::propagators::circuit::VALUE_OFFSET;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(crate) struct Explanation {
        pub node_len: usize,
        pub required: Map<usize, usize>,
        pub excluded: Set<(usize, usize)>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(crate) struct Inference {
        pub explanation: Explanation,
        pub inference: (usize, usize),
        pub positive: bool,
    }

    impl Inference {
        #[cfg(test)]
        pub(crate) fn negate(self) -> Explanation {
            let Inference {
                explanation,
                inference,
                positive,
            } = self;

            let mut explanation_extended = explanation.clone();

            if positive {
                let _ = explanation_extended.excluded.insert(inference);
            } else {
                let _ = explanation_extended
                    .required
                    .insert(inference.0, inference.1);
            }

            return explanation_extended;
        }
    }

    impl Explanation {
        pub(crate) fn to_propositional_conjunction<Var: IntegerVariable + 'static>(
            self,
            successors: &[Var],
        ) -> PropositionalConjunction {
            let mut reason = vec![];
            let Explanation {
                required, excluded, ..
            } = self;

            for (from, to) in required {
                let var = &successors[from];
                reason.push(predicate!(var == to as i32 + VALUE_OFFSET));
            }

            for (from, to) in excluded {
                let var = &successors[from];
                reason.push(predicate!(var != to as i32 + VALUE_OFFSET));
            }

            PropositionalConjunction::from(reason)
        }

        #[must_use]
        pub(crate) fn check_holds(&self, graph: &Graph) -> Result<(), (usize, usize)> {
            let Explanation {
                required, excluded, ..
            } = self;

            for (from, to) in required {
                if graph.edges[from] != Set::from_iter([*to]) {
                    return Err((*from, *to));
                }
            }

            for (from, to) in excluded {
                if graph.edges[from].contains(to) {
                    return Err((*from, *to));
                }
            }

            Ok(())
        }
    }

    #[derive(Debug)]
    pub(crate) struct Graph {
        node_len: usize,
        edges: Map<usize, Set<usize>>,
    }

    #[cfg(test)]
    struct SccContext<'a> {
        lowest_id: Vec<Option<usize>>,
        next_id: usize,
        node_id: Vec<Option<usize>>,
        stack: Vec<usize>,
        stack_set: Vec<bool>,
        graph: &'a Graph,
        scc_ids: Vec<usize>,
    }

    impl Graph {
        /// Create a graph from domains of variables,
        /// each domain containing a possible successor as indicated by index
        /// from `0` to `domains.len() - 1`.
        pub(crate) fn from_domains(domains: &[Set<usize>]) -> Self {
            let mut graph = Self {
                node_len: domains.len(),
                edges: Map::default(),
            };

            for (index, domain) in domains.iter().enumerate() {
                graph.edges.entry(index).or_default().extend(domain);
            }

            graph
        }

        #[cfg(test)]
        pub(crate) fn from_explanation(explanation: &Explanation) -> Self {
            let mut graph = Self {
                node_len: explanation.node_len,
                edges: Map::default(),
            };

            for node in 0..explanation.node_len {
                let neighbours = graph.edges.entry(node).or_default();
                if let Some(to) = explanation.required.get(&node) {
                    let _ = neighbours.insert(*to);
                } else {
                    for to in 0..explanation.node_len {
                        if !explanation.excluded.contains(&(node, to)) {
                            let _ = neighbours.insert(to);
                        }
                    }
                }
            }

            graph
        }

        /// Calculate strongly connected components (SCCs) of the graph.
        ///
        /// Returns the calculated SCCs in reverse topological order.
        #[cfg(test)]
        fn calculate_sccs(&self) -> Vec<Set<usize>> {
            let mut context = SccContext {
                lowest_id: vec![None; self.node_len],
                next_id: 0,
                node_id: vec![None; self.node_len],
                stack: Vec::new(),
                stack_set: vec![false; self.node_len],
                graph: self,
                scc_ids: Vec::new(),
            };

            for node in 0..self.node_len {
                if context.node_id[node].is_none() {
                    scc_dfs(node, &mut context);
                }
            }

            let mut sccs_by_id: Map<usize, Set<usize>> = Map::default();

            for node in 0..self.node_len {
                let id =
                    context.lowest_id[node].expect("all nodes should have ids after finding SCCs");
                let _ = sccs_by_id.entry(id).or_default().insert(node);
            }

            context
                .scc_ids
                .into_iter()
                .map(|id| sccs_by_id[&id].clone())
                .collect()
        }

        /// Produce an explanation of a graph containing multiple SCCs,
        /// if there are multiple.
        ///
        /// The explanation given states that the SCC that is
        /// last in the topological order is not connected to any SCC before it.
        #[cfg(test)]
        pub(crate) fn explain_multiple_sccs(&self) -> Option<Explanation> {
            let mut explanation = Explanation {
                node_len: self.node_len,
                required: Map::default(),
                excluded: Set::default(),
            };

            let mut sccs = self.calculate_sccs();
            if sccs.len() <= 1 {
                return None;
            }
            sccs.reverse();
            let last = sccs.pop().expect("at least 2 SCCs");

            for from in &last {
                for scc in &sccs {
                    for to in scc {
                        let _ = explanation.excluded.insert((*from, *to));
                    }
                }
            }

            Some(explanation)
        }

        /// Produce an explanation of a graph containing a cycle of nodes with
        /// out degree 1, if there is one.
        pub(crate) fn circuit_check(&self) -> Option<Explanation> {
            for node in 0..self.node_len {
                let mut visited: Set<usize> = Set::default();
                let mut stack = vec![(node, 1)];

                let mut loop_edge = None;
                let mut max_loops = 2 * self.node_len + 1;
                while let Some((node, current_len)) = stack.pop() {
                    debug_assert!(max_loops > 0);
                    max_loops -= 1;

                    if !visited.insert(node) {
                        continue;
                    }
                    if self.edges[&node].len() == 1 {
                        let to = self.edges[&node].iter().next().unwrap();
                        stack.push((*to, current_len + 1));
                        if visited.contains(to) && current_len < self.node_len {
                            loop_edge = Some((node, *to));
                            break;
                        }
                    }
                }

                if let Some((start, mut current)) = loop_edge {
                    let mut explanation = Explanation {
                        node_len: self.node_len,
                        required: Map::default(),
                        excluded: Set::default(),
                    };

                    let mut chain = vec![current];

                    let mut max_loops = 2 * self.node_len + 1;
                    while current != start {
                        debug_assert!(max_loops > 0);
                        max_loops -= 1;

                        let next = *self.edges[&current].iter().next().unwrap();
                        chain.push(next);
                        current = next;
                    }

                    let chain_set: Set<usize> = Set::from_iter(chain.clone());

                    for node in chain {
                        for other in 0..self.node_len {
                            if chain_set.contains(&other) {
                                continue;
                            }
                            let _ = explanation.excluded.insert((node, other));
                        }
                    }

                    return Some(explanation);
                }
            }

            None
        }

        /// Propagate inferences that prevent cycles from being formed.
        ///
        /// Assumes that the current graph does not have a cycle that `circuit_check` would catch.
        pub(crate) fn circuit_prevent(&self) -> Vec<Inference> {
            let mut inferences = vec![];

            for node in 0..self.node_len {
                let mut chain = vec![];

                let mut current = node;

                while chain.len() < self.node_len {
                    chain.push(current);

                    if self.edges[&current].len() == 1 {
                        let next = *self.edges[&current].iter().next().unwrap();
                        current = next;
                    } else {
                        break;
                    }
                }

                if 3 <= chain.len() && chain.len() < self.node_len {
                    let first = chain[0];
                    let last = chain[chain.len() - 1];
                    let chain_set: Set<usize> = Set::from_iter(chain.clone());

                    let mut explanation = Explanation {
                        node_len: self.node_len,
                        required: Map::default(),
                        excluded: Set::default(),
                    };

                    for node in chain {
                        if node == last {
                            continue;
                        }
                        for other in 0..self.node_len {
                            if chain_set.contains(&other) {
                                continue;
                            }

                            let _ = explanation.excluded.insert((node, other));
                        }
                    }

                    let inference = Inference {
                        explanation: explanation,
                        inference: (last, first),
                        positive: false,
                    };
                    inferences.push(inference);
                }
            }

            inferences
        }
    }

    #[cfg(test)]
    fn scc_dfs(current: usize, ctx: &mut SccContext) {
        ctx.stack.push(current);
        ctx.stack_set[current] = true;

        ctx.node_id[current] = Some(ctx.next_id);
        ctx.lowest_id[current] = Some(ctx.next_id);
        ctx.next_id += 1;

        for neighbour in &ctx.graph.edges[&current] {
            if ctx.node_id[*neighbour].is_none() {
                scc_dfs(*neighbour, ctx);
            }

            if ctx.stack_set[*neighbour] {
                // `neighbour` can reach and can be reached by `current`,
                // thus SCC is formed. Update lowest id accordingly.

                let old_lowest_id = ctx.lowest_id[current].expect("set before loop");
                let new_id =
                    ctx.lowest_id[*neighbour].expect("anything on stack must have lowest id");

                ctx.lowest_id[current] = Some(old_lowest_id.min(new_id))
            }
        }

        if ctx.node_id[current] == ctx.lowest_id[current] {
            let scc_id = ctx.lowest_id[current].expect("SCC was found");
            ctx.scc_ids.push(scc_id);

            while let Some(popped) = ctx.stack.pop() {
                ctx.stack_set[popped] = false;
                ctx.lowest_id[popped] = ctx.node_id[current];

                if popped == current {
                    break;
                }
            }
        }
    }

    #[cfg(test)]
    mod test {
        use crate::propagators::circuit::graph::Graph;
        use pumpkin_core::containers::{HashMap as Map, HashSet as Set};

        fn create_graph(node_len: usize, links: impl IntoIterator<Item = (usize, usize)>) -> Graph {
            let mut g = Graph {
                node_len,
                edges: Map::default(),
            };

            for (from, to) in links.into_iter() {
                let _ = g.edges.entry(from).or_default().insert(to);
            }

            for node in 0..node_len {
                let _ = g.edges.entry(node).or_default();
            }

            g
        }

        mod scc {
            use super::*;

            #[test]
            fn single_node_sccs() {
                let g = create_graph(1, []);
                let sccs = g.calculate_sccs();

                assert_eq!(sccs, vec![Set::from_iter([0])]);

                let explanation = g.explain_multiple_sccs();
                assert_eq!(explanation, None);
            }

            #[test]
            fn two_nodes_sccs_1() {
                let g = create_graph(2, [(0, 1)]);
                let sccs = g.calculate_sccs();

                assert_eq!(sccs, vec![Set::from_iter([1]), Set::from_iter([0]),]);

                let explanation = g.explain_multiple_sccs().unwrap();
                assert!(explanation.check_holds(&g).is_ok());

                let g = Graph::from_explanation(&explanation);
                let explanation_2 = g.explain_multiple_sccs().unwrap();
                assert_eq!(explanation, explanation_2);
            }

            #[test]
            fn two_nodes_sccs_2() {
                let g = create_graph(2, [(0, 1), (1, 0)]);
                let sccs = g.calculate_sccs();

                assert_eq!(sccs, vec![Set::from_iter([0, 1])]);

                let explanation = g.explain_multiple_sccs();
                assert_eq!(explanation, None);
            }

            #[test]
            fn multiple_sccs() {
                let g = create_graph(6, [(0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3), (3, 0)]);
                let sccs = g.calculate_sccs();

                assert_eq!(
                    sccs,
                    vec![Set::from_iter([0, 1, 2]), Set::from_iter([3, 4, 5]),]
                );

                let explanation = g.explain_multiple_sccs().unwrap();
                assert!(explanation.check_holds(&g).is_ok());

                let g = Graph::from_explanation(&explanation);
                let explanation_2 = g.explain_multiple_sccs().unwrap();
                assert_eq!(explanation, explanation_2);
            }

            #[test]
            fn single_scc() {
                let g = create_graph(
                    6,
                    [
                        (0, 1),
                        (1, 2),
                        (2, 0),
                        (3, 4),
                        (4, 5),
                        (5, 3),
                        (3, 0),
                        (2, 5),
                    ],
                );
                let sccs = g.calculate_sccs();

                assert_eq!(sccs, vec![Set::from_iter([0, 1, 2, 3, 4, 5])]);

                let explanation = g.explain_multiple_sccs();
                assert_eq!(explanation, None);
            }
        }

        mod check {
            use super::*;

            #[test]
            fn single_node() {
                let g = create_graph(1, [(0, 0)]);

                let explanation = g.circuit_check();
                assert_eq!(explanation, None);
            }

            #[test]
            fn conflict_2() {
                let g = create_graph(2, [(0, 0), (0, 1), (1, 1)]);

                let explanation = g.circuit_check().unwrap();
                assert!(explanation.check_holds(&g).is_ok());

                let g = Graph::from_explanation(&explanation);
                let explanation_2 = g.explain_multiple_sccs().unwrap();
                assert_eq!(explanation, explanation_2);
            }

            #[test]
            fn no_conflict_2() {
                let g = create_graph(2, [(0, 1), (1, 0)]);

                let explanation = g.circuit_check();
                assert_eq!(explanation, None);
            }

            #[test]
            fn no_conflict_6a() {
                #[rustfmt::skip]
                let g = create_graph(6, [
                    (0, 1),
                    (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5),
                    (2, 3),
                    (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5),
                    (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5),
                    (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5)
                ]);

                let explanation = g.circuit_check();
                assert_eq!(explanation, None);
            }

            #[test]
            fn no_conflict_6b() {
                #[rustfmt::skip]
                let g = create_graph(6, [
                    (0, 1),
                    (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5),
                    (2, 3),
                    (3, 4),
                    (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5),
                    (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5)
                ]);

                let explanation = g.circuit_check();
                assert_eq!(explanation, None);
            }

            #[test]
            fn conflict_6() {
                #[rustfmt::skip]
                let g = create_graph(6, [
                    (0, 1),
                    (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5),
                    (2, 3),
                    (3, 4),
                    (4, 2),
                    (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5)
                ]);

                let explanation = g.circuit_check().unwrap();
                explanation.check_holds(&g).unwrap();

                let g = Graph::from_explanation(&explanation);
                let explanation_2 = g.explain_multiple_sccs().unwrap();
                assert_eq!(explanation, explanation_2);
            }
        }

        mod prevent {
            use crate::propagators::circuit::graph::Explanation;

            use super::*;

            #[test]
            fn no_propagation_2() {
                let g = Graph::from_explanation(&Explanation {
                    node_len: 2,
                    required: Map::from_iter([(0, 1)]),
                    excluded: Set::default(),
                });

                let inferences = g.circuit_prevent();
                assert_eq!(inferences, vec![]);
            }

            #[test]
            fn no_propagation_6() {
                let g = Graph::from_explanation(&Explanation {
                    node_len: 6,
                    required: Map::from_iter([(0, 1)]),
                    excluded: Set::default(),
                });

                let inferences = g.circuit_prevent();
                assert_eq!(inferences, vec![]);
            }

            #[test]
            fn propagation_6() {
                let g = Graph::from_explanation(&Explanation {
                    node_len: 6,
                    required: Map::from_iter([(0, 1), (1, 2)]),
                    excluded: Set::default(),
                });

                let mut inferences = g.circuit_prevent();
                assert_eq!(inferences.len(), 1);
                let inference = inferences.pop().unwrap();
                assert!(!inference.positive);
                assert_eq!(inference.explanation.check_holds(&g), Ok(()));
                assert_eq!(inference.inference, (2, 0));

                let explanation = inference.negate();
                let g = Graph::from_explanation(&explanation);
                let _ = g.explain_multiple_sccs().unwrap();
                // negating an inference does not produce an identical explanation
                // assert_eq!(explanation, explanation_2);
            }
        }
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_core::containers::HashMap as Map;
    use pumpkin_core::containers::HashSet as Set;

    use pumpkin_core::{
        TestSolver,
        state::{Conflict, PropagatorId},
        variables::DomainId,
    };

    use crate::propagators::circuit::CircuitConstructor;

    fn set_up_circuit_state_simple(
        node_len: i32,
        set: &[(i32, i32)],
        conflict_detection_only: bool,
    ) -> (TestSolver, Result<PropagatorId, Conflict>, Vec<DomainId>) {
        let mut solver = TestSolver::default();
        let map: Map<i32, i32> = Map::from_iter(set.iter().cloned());

        let mut successors = vec![];
        for i in 1..=node_len {
            let var = if let Some(to) = map.get(&i) {
                solver.new_variable(*to, *to)
            } else {
                solver.new_variable(1, node_len)
            };

            successors.push(var);
        }
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(CircuitConstructor {
            successors: successors.clone().into(),
            constraint_tag,
            conflict_detection_only,
        });

        (solver, result, successors)
    }

    fn set_up_circuit_state_full(
        successors: &[Vec<i32>],
        conflict_detection_only: bool,
    ) -> (TestSolver, Result<PropagatorId, Conflict>, Vec<DomainId>) {
        let mut solver = TestSolver::default();

        let mut vars = vec![];
        for i in 1..=successors.len() {
            let var = solver.new_variable(1, successors.len() as i32);
            let set: Set<i32> = Set::from_iter(successors[i - 1].clone());
            let full_set = Set::from_iter(1..=successors.len() as i32);
            let inverse_set = full_set.difference(&set);
            for value in inverse_set {
                let _ = solver.remove(var, *value);
            }

            vars.push(var);
        }
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(CircuitConstructor {
            successors: vars.clone().into(),
            constraint_tag,
            conflict_detection_only,
        });

        (solver, result, vars)
    }

    #[test]
    fn example_conflict() {
        let (_, result, _) =
            set_up_circuit_state_simple(6, &[(1, 2), (3, 4), (4, 5), (5, 3)], true);
        assert!(result.is_err(), "{:#?}", result);
    }

    #[test]
    fn example_propagation() {
        let (solver, result, variables) =
            set_up_circuit_state_simple(6, &[(1, 2), (3, 4), (4, 5)], false);

        assert!(result.is_ok(), "{:#?}", result);
        assert!(!solver.contains(variables[4], 3));
    }

    #[test]
    fn failing_conflict_0() {
        let (_, result, _) = set_up_circuit_state_full(
            &[
                vec![1, 5],
                vec![2, 5, 3],
                vec![5, 2, 4, 3],
                vec![5, 4, 3],
                vec![3],
            ],
            true,
        );

        assert!(result.is_ok(), "{:#?}", result);
    }

    #[test]
    fn full_loop() {
        let (_, result, _) =
            set_up_circuit_state_simple(5, &[(1, 2), (2, 3), (3, 4), (4, 5), (5, 1)], true);
        assert!(result.is_ok(), "{:#?}", result);
    }

    #[test]
    fn nearly_full_loop() {
        let (_, result, _) =
            set_up_circuit_state_simple(5, &[(1, 2), (2, 3), (3, 4), (4, 5)], true);
        assert!(result.is_ok(), "{:#?}", result);
    }
}
