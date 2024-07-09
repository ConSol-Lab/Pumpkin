use std::collections::VecDeque;
use std::hash::Hash;

use crate::basic_types::HashMap;
use crate::basic_types::HashSet;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::propagation_context::PropagationContext;
use crate::engine::propagation::propagation_context::PropagationContextMut;
use crate::engine::propagation::propagator::EnqueueDecision;
use crate::engine::propagation::propagator::Propagator;
use crate::engine::propagation::propagator_constructor::PropagatorConstructor;
use crate::engine::propagation::propagator_constructor_context::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::predicate;

type DiffLogicVariables<V> = (V, V, V, V);

/// Bounds consistent propagator for a set of constraints `x_i + \delta <= x_j`.
pub(crate) struct DifferenceLogicConstructor<V> {
    pub(crate) difference_constraints: Box<[(V, i32, V)]>,
}

#[derive(Default)]
pub(crate) struct DifferenceLogicPropagator<V> {
    #[allow(clippy::type_complexity)]
    /// The elementary constraints of the form `(y_1 <= v) => (y_2 <= v + d)`.
    elementary_constraints: HashMap<V, Box<[(i32, V)]>>,
    /// The y variables associated with each original difference_constraint;
    ///  used in translating LocalId back to DiffLogicVariable.
    difference_vars: Box<[DiffLogicVariables<V>]>,
    /// The set of updated variables since the last propagation.
    // potential optimisation: bitset?
    updated: HashSet<LocalId>,
    /// Worklist used in propagation, kept here to save on allocations.
    worklist: VecDeque<V>,
}

impl<V> PropagatorConstructor for DifferenceLogicConstructor<V>
where
    V: IntegerVariable + Hash + Eq + 'static,
    V::AffineView: IntegerVariable + Hash + Eq,
{
    type Propagator = DifferenceLogicPropagator<V::AffineView>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        // To keep x_i + \delta <= x_j bound consistent, we do:
        //  x_i >= v -> x_j >= v + \delta
        //  x_j <= v -> x_i <= v - \delta
        // We normalise this into all upperbound version by negating everything in the
        //  first implication:
        //  -x_i <= -v -> -x_j <= -v - \delta
        // Now have all implications of the form y1 <= v -> y2 <= v + \delta, these are the
        //  elementary propagators.

        // `register_vars` does the caching logic to make sure we only register
        //  each variable once. This is relevant for the propagation algorithm that
        //  uses the changed variables to look up affected formulas on change in
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
                            false,
                        ),
                        context.register(
                            x_i.clone().scaled(-1),
                            DomainEvents::UPPER_BOUND,
                            LocalId::from(4 * i as u32 + 1),
                            false,
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
                            false,
                        ),
                        context.register(
                            x_j.clone().scaled(-1),
                            DomainEvents::UPPER_BOUND,
                            LocalId::from(4 * i as u32 + 3),
                            false,
                        ),
                    )
                })
                .clone();
            (x_i_pos, x_i_neg, x_j_pos, x_j_neg)
        };

        let mut elementary_constraints = HashMap::new();
        let mut difference_vars = Vec::with_capacity(self.difference_constraints.len());
        self.difference_constraints
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
        DifferenceLogicPropagator {
            elementary_constraints: elementary_constraints
                .into_iter()
                .map(|(k, v)| (k, v.into_boxed_slice()))
                .collect(),
            difference_vars: difference_vars.into_boxed_slice(),
            updated: Default::default(),
            worklist: Default::default(),
        }
    }
}

impl<V> DifferenceLogicPropagator<V>
where
    V: IntegerVariable + Hash + Eq,
{
    fn local_id_to_var(&self, id: LocalId) -> &V {
        // This mirrors the local ids computed in `register_vars` in
        //  `<DiffLogic as PropagatorConstructor>::create`.
        // Note that the caching does not break the mapping of `LocalId`s to difference_vars.
        //  Whenever cached variables were used in the tuple, their `LocalId`s simply
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

impl<V> Propagator for DifferenceLogicPropagator<V>
where
    V: IntegerVariable + Hash + Eq,
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        for &y_start in &self.updated {
            propagate_shared::<false, V>(
                &self.elementary_constraints,
                context,
                &self.local_id_to_var(y_start).clone(),
                &mut self.worklist,
            )?;
        }
        self.updated.clear();
        Ok(())
    }

    fn notify(
        &mut self,
        _context: &mut PropagationContextMut,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let _ = self.updated.insert(local_id);
        EnqueueDecision::Enqueue
    }

    fn synchronise(&mut self, _context: &PropagationContext) {
        self.updated.clear();
    }

    fn priority(&self) -> u32 {
        1
    }

    fn name(&self) -> &str {
        "DiffLogic"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        for y_start in self.elementary_constraints.keys() {
            propagate_shared::<true, V>(
                &self.elementary_constraints,
                context,
                y_start,
                &mut self.worklist,
            )?;
        }
        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        for y_start in self.elementary_constraints.keys() {
            propagate_shared::<false, V>(
                &self.elementary_constraints,
                context,
                y_start,
                &mut Default::default(),
            )?;
        }
        Ok(())
    }
}

#[allow(clippy::type_complexity)]
fn propagate_shared<const CYCLE_CHECK: bool, V>(
    elementary_constraints: &HashMap<V, Box<[(i32, V)]>>,
    context: &mut PropagationContextMut,
    y_start: &V,
    worklist: &mut VecDeque<V>,
) -> PropagationStatusCP
where
    V: IntegerVariable + Hash + Eq,
{
    worklist.push_front(y_start.clone());
    let mut cycle_reason = Vec::new();
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
                if CYCLE_CHECK {
                    cycle_reason.push(reason);
                    if y2 == y_start {
                        // return Inconsistency::Other without a reason, since this check happens
                        // only  at the root.
                        return Err(cycle_reason.into());
                    }
                }
                let reason: PropositionalConjunction = reason.into();
                context.set_upper_bound(y2, y2_ub_max, reason)?;
                worklist.push_back(y2.clone());
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::ConflictInfo;
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::engine::variables::TransformableVariable;
    use crate::engine::IntDomainEvent::UpperBound;

    #[test]
    fn initialisation_and_propagation() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(-3, 3);
        let x_1 = solver.new_variable(-4, 4);
        let x_2 = solver.new_variable(-5, 5);

        // f1   x_0 + 1 <= x_1
        // f2   x_1 + 2 <= x_2

        let mut propagator = solver
            .new_propagator(DifferenceLogicConstructor {
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
        #[allow(clippy::identity_op)]
        let _ = solver.notify(&mut propagator, UpperBound.into(), LocalId::from(4 * 1 + 2));
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
        #[allow(clippy::erasing_op, clippy::identity_op)]
        let _ = solver.notify(&mut propagator, UpperBound.into(), LocalId::from(4 * 0 + 3));
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

        let _ = solver
            .new_propagator(DifferenceLogicConstructor {
                difference_constraints: vec![(x_0, 1, x_1), (x_1, 2, x_2)].into_boxed_slice(),
            })
            .expect("no empty domains");

        // due to propagation
        assert_eq!(-2, solver.lower_bound(x_1)); // by f1
        assert_eq!(2, solver.upper_bound(x_0)); // by f1 + transitivity from f2
        assert_eq!(0, solver.lower_bound(x_2)); // by f2 + transitivity from f1
        assert_eq!(3, solver.upper_bound(x_1)); // by f2

        let flipped_x_1 = x_1.scaled(-1);
        let reason_x_1_lb = solver.get_reason_int(predicate![flipped_x_1 <= 2].try_into().unwrap());
        assert_eq!(conjunction!([x_0 >= -3]), *reason_x_1_lb);

        let reason_x_0_ub = solver.get_reason_int(predicate![x_0 <= 2].try_into().unwrap());
        assert_eq!(conjunction!([x_1 <= 3]), *reason_x_0_ub);
    }

    #[test]
    fn cycle_detected() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(-10, 10);
        let x_1 = solver.new_variable(-10, 10);

        // f1   x_0 + 1 <= x_1
        // f2   x_1 + 1 <= x_0

        let inconsistency = solver
            .new_propagator(DifferenceLogicConstructor {
                difference_constraints: vec![(x_0, 1, x_1), (x_1, 1, x_0)].into_boxed_slice(),
            })
            .expect_err("cycle detected");

        // inconsistency found as soon as first propagation is done
        assert!(matches!(
            inconsistency,
            Inconsistency::Other(ConflictInfo::Explanation(_))
        ));

        let Inconsistency::Other(ConflictInfo::Explanation(cycle)) = inconsistency else {
            unreachable!()
        };
        assert!(
            cycle.eq(&conjunction!([x_0 >= -10] & [x_1 >= -9]))
                || cycle.eq(&conjunction!([x_0 >= -9] & [x_1 >= -10]))
                || cycle.eq(&conjunction!([x_0 <= 9] & [x_1 <= 10]))
                || cycle.eq(&conjunction!([x_0 <= 10] & [x_1 <= 9])),
            "Cycle is not based on first propagation of either upper or lower bounds,
             but instead has: {}",
            cycle,
        );
    }
}
