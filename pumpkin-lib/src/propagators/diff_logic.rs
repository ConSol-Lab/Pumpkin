use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

use crate::basic_types::variables::IntVar;
use crate::basic_types::{Predicate, PropagationStatusCP, PropositionalConjunction};
use crate::engine::{
    CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, DomainEvents,
    EnqueueDecision, LocalId, OpaqueDomainEvent, PropagationContext, PropagatorConstructorContext,
    PropagatorVariable,
};
use crate::predicate;

type DiffLogicVariables<V> = (
    PropagatorVariable<V>,
    PropagatorVariable<V>,
    PropagatorVariable<V>,
    PropagatorVariable<V>,
);

/// Bounds consistent propagator for a set of constraints `x_i + \delta <= x_j`.
pub struct DiffLogicArgs<V> {
    pub difference_constraints: Box<[(V, i32, V)]>,
}

#[derive(Default)]
pub struct DiffLogic<V> {
    #[allow(clippy::type_complexity)]
    /// The elementary constraints of the form `(y_1 <= v) => (y_2 <= v + d)`.
    elementary_constraints: HashMap<PropagatorVariable<V>, Box<[(i32, PropagatorVariable<V>)]>>,
    /// The y variables associated with each original difference_constraint;
    ///  used in translating LocalId back to DiffLogicVariable.
    difference_vars: Box<[DiffLogicVariables<V>]>,
    /// The set of updated variables since the last propagation.
    // potential optimisation: bitset?
    updated: HashSet<LocalId>,
    /// The y_1 var (value) responsible for the change to y_2 with new upperbound (key).
    reasons: HashMap<(PropagatorVariable<V>, i32), Predicate>,
    /// Worklist used in propagation, kept here to save on allocations.
    worklist: VecDeque<PropagatorVariable<V>>,
}

impl<V> CPPropagatorConstructor for DiffLogic<V>
where
    V: IntVar + Hash + Eq + 'static,
    <V as IntVar>::AffineView: Hash + Eq,
{
    type Args = DiffLogicArgs<V>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        // To keep x_i + \delta <= x_j bound consistent, we do:
        //  x_i >= v -> x_j >= v + \delta
        //  x_j <= v -> x_i <= v - \delta
        // We normalise this into all upperbound version by negating everything in the
        //  first implication:
        //  -x_i <= -v -> -x_j <= -v - \delta
        // Now have all implications of the form y1 <= v -> y2 <= v + \delta, these are the
        //  elementary propagators.

        // `register_vars` does the caching logic to make sure we only generate 1
        //  PropagatorVariable per variable. This is relevant for the propagation algorithm that
        //  uses the PropagatorVariable to look up affected formulas on change in
        //  `elementary_constraints`. If no reuse is required, the `LocalId`s will be from `0` to
        //  `4 * args.difference_constraints.len()`, since each constraint has 2 variables and each
        //  variable is registered twice, in a positive and a negative view.
        let mut var_cache = HashMap::new();
        let mut register_vars = |i, x_i: &V, x_j: &V| {
            let (x_i_pos, x_i_neg) = var_cache
                .entry(x_i.clone())
                .or_insert_with(|| {
                    (
                        context.register(
                            x_i.clone().scaled(1), // scale by 1 to make it an AffineView
                            DomainEvents::UPPER_BOUND,
                            LocalId::from(4 * i as u32),
                        ),
                        context.register(
                            x_i.clone().scaled(-1),
                            DomainEvents::UPPER_BOUND,
                            LocalId::from(4 * i as u32 + 1),
                        ),
                    )
                })
                .clone();
            let (x_j_pos, x_j_neg) = var_cache
                .entry(x_j.clone())
                .or_insert_with(|| {
                    (
                        context.register(
                            x_j.clone().scaled(1),
                            DomainEvents::UPPER_BOUND,
                            LocalId::from(4 * i as u32 + 2),
                        ),
                        context.register(
                            x_j.clone().scaled(-1),
                            DomainEvents::UPPER_BOUND,
                            LocalId::from(4 * i as u32 + 3),
                        ),
                    )
                })
                .clone();
            (x_i_pos, x_i_neg, x_j_pos, x_j_neg)
        };

        let mut elementary_constraints = HashMap::new();
        let mut difference_vars = Vec::with_capacity(args.difference_constraints.len());
        args.difference_constraints
            .iter()
            .enumerate()
            .for_each(|(i, (x_i, delta, x_j))| {
                let (y_i_pos, y_i_neg, y_j_pos, y_j_neg) = register_vars(i, x_i, x_j);
                // We keep the negated \delta so we can do `UB(y2) <= UB(y1) + \delta`
                let delta: i32 = -*delta;
                elementary_constraints
                    .entry(y_i_neg.clone())
                    .or_insert_with(Vec::new)
                    .push((delta, y_j_neg.clone()));
                elementary_constraints
                    .entry(y_j_pos.clone())
                    .or_insert_with(Vec::new)
                    .push((delta, y_i_pos.clone()));
                difference_vars.push((y_i_pos, y_i_neg, y_j_pos, y_j_neg));
            });
        Box::new(DiffLogic {
            elementary_constraints: elementary_constraints
                .into_iter()
                .map(|(k, v)| (k, v.into_boxed_slice()))
                .collect(),
            difference_vars: difference_vars.into_boxed_slice(),
            updated: Default::default(),
            reasons: Default::default(),
            worklist: Default::default(),
        })
    }
}

impl<V> DiffLogic<V>
where
    V: IntVar + Hash + Eq,
{
    fn local_id_to_var(&self, id: LocalId) -> &PropagatorVariable<V> {
        // This mirrors the local ids computed in `register_vars` in
        //  `<DiffLogic as CPPropagatorConstructor>::create`.
        // Note that the caching does not break the mapping of `LocalId`s to difference_vars.
        //  Whenever cached `PropagatorVariable`s were used in the tuple, their `LocalId`s simply
        //  map to an earlier offset in the `difference_vars` array.
        let (div, rem) = (id.unpack() / 4, id.unpack() % 4);
        let diff_vars = &self.difference_vars[div as usize];
        match rem {
            0 => &diff_vars.0,
            1 => &diff_vars.1,
            2 => &diff_vars.2,
            3 => &diff_vars.3,
            _ => unreachable!(),
        }
    }
}

impl<V> ConstraintProgrammingPropagator for DiffLogic<V>
where
    V: IntVar + Hash + Eq,
{
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        for &y_start in &self.updated {
            propagate_shared(
                &self.elementary_constraints,
                context,
                &self.local_id_to_var(y_start).clone(),
                &mut self.reasons,
                &mut self.worklist,
            )?;
        }
        self.updated.clear();
        Ok(())
    }

    fn notify(
        &mut self,
        _context: &mut PropagationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        self.updated.insert(local_id);
        EnqueueDecision::Enqueue
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        self.updated.clear();
        self.reasons.retain(|k, _| context.upper_bound(&k.0) <= k.1);
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        let var = self.local_id_to_var(delta.affected_local_id());
        let DomainChange::UpperBound(ub) = var.unpack(delta) else {
            unreachable!()
        };
        vec![*self.reasons.get(&(var.clone(), ub)).unwrap()].into()
    }

    fn priority(&self) -> u32 {
        1
    }

    fn name(&self) -> &str {
        "DiffLogic"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        for y_start in self.elementary_constraints.keys() {
            cycle_check(&self.elementary_constraints, y_start, &mut self.worklist)?;
            propagate_shared(
                &self.elementary_constraints,
                context,
                y_start,
                &mut self.reasons,
                &mut self.worklist,
            )?;
        }
        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        for y_start in self.elementary_constraints.keys() {
            propagate_shared(
                &self.elementary_constraints,
                context,
                y_start,
                &mut Default::default(),
                &mut Default::default(),
            )?;
        }
        Ok(())
    }
}

/// Find negative cycles in the graph of elementary constraints: this will always give an empty
///  domain
#[allow(clippy::type_complexity)]
fn cycle_check<V>(
    elementary_constraints: &HashMap<PropagatorVariable<V>, Box<[(i32, PropagatorVariable<V>)]>>,
    y_start: &PropagatorVariable<V>,
    worklist: &mut VecDeque<PropagatorVariable<V>>,
) -> PropagationStatusCP
where
    V: IntVar + Hash + Eq,
{
    worklist.push_front(y_start.clone());
    while let Some(y1) = worklist.pop_front() {
        let y1_y2_edges = elementary_constraints
            .get(&y1)
            .map(|s| s.iter())
            .unwrap_or([].iter());
        for (delta, y2) in y1_y2_edges {
            if *delta < 0 {
                if y2 == y_start {
                    // return Inconsistency::Other without a reason, since this check happens only
                    //  at the root.
                    return Err([].into());
                }
                worklist.push_back(y2.clone());
            }
        }
    }
    Ok(())
}

#[allow(clippy::type_complexity)]
fn propagate_shared<V>(
    elementary_constraints: &HashMap<PropagatorVariable<V>, Box<[(i32, PropagatorVariable<V>)]>>,
    context: &mut PropagationContext,
    y_start: &PropagatorVariable<V>,
    reasons: &mut HashMap<(PropagatorVariable<V>, i32), Predicate>,
    worklist: &mut VecDeque<PropagatorVariable<V>>,
) -> PropagationStatusCP
where
    V: IntVar + Hash + Eq,
{
    worklist.push_front(y_start.clone());
    while let Some(y1) = worklist.pop_front() {
        let y1_y2_edges = elementary_constraints
            .get(&y1)
            .map(|s| s.iter())
            .unwrap_or([].iter());
        for (delta, y2) in y1_y2_edges {
            let y1_ub = context.upper_bound(&y1);
            let y2_ub_max = y1_ub + delta;
            if y2_ub_max < context.upper_bound(y2) {
                let reason = predicate![y1 <= y1_ub];
                reasons.insert((y2.clone(), y2_ub_max), reason);
                context.set_upper_bound(y2, y2_ub_max)?;
                worklist.push_back(y2.clone());
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::engine::DomainEvent::{LowerBound, UpperBound};

    #[test]
    fn initialisation_and_propagation() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(-3, 3);
        let x_1 = solver.new_variable(-4, 4);
        let x_2 = solver.new_variable(-5, 5);

        // f1   x_0 + 1 <= x_1
        // f2   x_1 + 2 <= x_2

        let mut propagator = solver
            .new_propagator::<DiffLogic<_>>(DiffLogicArgs {
                difference_constraints: vec![(x_0, 1, x_1), (x_1, 2, x_2)].into_boxed_slice(),
            })
            .expect("no empty domains");

        // due to propagation
        assert_eq!(-2, solver.lower_bound(x_1)); // by f1
        assert_eq!(2, solver.upper_bound(x_0)); // by f1 + transitivity from f2
        assert_eq!(0, solver.lower_bound(x_2)); // by f2 + transitivity from f1
        assert_eq!(3, solver.upper_bound(x_1)); // by f2

        // still same
        assert_eq!(-3, solver.lower_bound(x_0));
        assert_eq!(5, solver.upper_bound(x_2));

        solver.set_upper_bound(x_2, 4).expect("no empty domains");
        solver.notify_changed(&mut propagator, x_2, UpperBound);
        solver
            .propagate(&mut propagator)
            .expect("no empty domains or negative cycles");

        // due to propagation
        assert_eq!(2, solver.upper_bound(x_1)); // by f2
        assert_eq!(1, solver.upper_bound(x_0)); // by f1 + transitivity from f2

        // still same
        assert_eq!(-2, solver.lower_bound(x_1));
        assert_eq!(0, solver.lower_bound(x_2));
        assert_eq!(-3, solver.lower_bound(x_0));

        solver.set_lower_bound(x_1, -1).expect("no empty domains");
        solver.notify_changed(&mut propagator, x_1, LowerBound);
        solver
            .propagate(&mut propagator)
            .expect("no empty domains or negative cycles");

        // due to propagation
        assert_eq!(1, solver.lower_bound(x_2)); // by f2

        // still same
        assert_eq!(2, solver.upper_bound(x_1));
        assert_eq!(1, solver.upper_bound(x_0));
        assert_eq!(-3, solver.lower_bound(x_0));
        assert_eq!(4, solver.upper_bound(x_2));
    }

    #[test]
    fn explanations() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(-3, 3);
        let x_1 = solver.new_variable(-4, 4);
        let x_2 = solver.new_variable(-5, 5);

        // f1   x_0 + 1 <= x_1
        // f2   x_1 + 2 <= x_2

        let mut propagator = solver
            .new_propagator::<DiffLogic<_>>(DiffLogicArgs {
                difference_constraints: vec![(x_0, 1, x_1), (x_1, 2, x_2)].into_boxed_slice(),
            })
            .expect("no empty domains");

        // due to propagation
        assert_eq!(-2, solver.lower_bound(x_1)); // by f1
        assert_eq!(2, solver.upper_bound(x_0)); // by f1 + transitivity from f2
        assert_eq!(0, solver.lower_bound(x_2)); // by f2 + transitivity from f1
        assert_eq!(3, solver.upper_bound(x_1)); // by f2

        let mut delta_x_1_lb = solver.to_deltas(&propagator, x_1, DomainChange::LowerBound(-2));
        assert_eq!(1, delta_x_1_lb.len());
        let reason_x_1_lb = solver.get_reason(&mut propagator, delta_x_1_lb.pop().unwrap());
        assert_eq!(conjunction!([x_0 >= -3]), reason_x_1_lb);

        let mut delta_x_0_ub = solver.to_deltas(&propagator, x_0, DomainChange::UpperBound(2));
        assert_eq!(1, delta_x_0_ub.len());
        let reason_x_0_ub = solver.get_reason(&mut propagator, delta_x_0_ub.pop().unwrap());
        assert_eq!(conjunction!([x_1 <= 3]), reason_x_0_ub);
    }

    #[test]
    fn cycle_detected() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(-10, 10);
        let x_1 = solver.new_variable(-10, 10);

        // f1   x_0 + 1 <= x_1
        // f2   x_1 + 1 <= x_0

        let inconsistency = solver
            .new_propagator::<DiffLogic<_>>(DiffLogicArgs {
                difference_constraints: vec![(x_0, 1, x_1), (x_1, 1, x_0)].into_boxed_slice(),
            })
            .expect_err("cycle detected");

        // inconsistency found as soon as first propagation is done
        assert_eq!(Inconsistency::Other(vec![].into()), inconsistency);
    }
}
