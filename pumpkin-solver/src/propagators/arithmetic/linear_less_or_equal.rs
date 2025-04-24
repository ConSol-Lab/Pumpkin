use itertools::Itertools;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::contexts::ManipulateTrailedValues;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::variables::IntegerVariable;
use crate::engine::TrailedInteger;
use crate::predicate;
use crate::pumpkin_assert_simple;

/// Propagator for the constraint `reif => \sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct LinearLessOrEqualPropagator<Var> {
    x: Box<[Var]>,
    c: i32,

    /// The lower bound of the sum of the left-hand side. This is incremental state.
    lower_bound_left_hand_side: TrailedInteger,
    /// The value at index `i` is the bound for `x[i]`.
    current_bounds: Box<[TrailedInteger]>,
}

impl<Var> LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    pub(crate) fn new(x: Box<[Var]>, c: i32) -> Self {
        let current_bounds = (0..x.len())
            .map(|_| TrailedInteger::default())
            .collect_vec()
            .into();

        // incremental state will be properly initialized in `Propagator::initialise_at_root`.
        LinearLessOrEqualPropagator::<Var> {
            x,
            c,
            lower_bound_left_hand_side: TrailedInteger::default(),
            current_bounds,
        }
    }

    fn create_conflict_reason(&self, context: PropagationContext) -> PropositionalConjunction {
        self.x
            .iter()
            .map(|var| predicate![var >= context.lower_bound(var)])
            .collect()
    }
}

impl<Var: 'static> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        let mut lower_bound_left_hand_side = 0_i64;
        self.x.iter().enumerate().for_each(|(i, x_i)| {
            let _ = context.register(
                x_i.clone(),
                DomainEvents::LOWER_BOUND,
                LocalId::from(i as u32),
            );
            lower_bound_left_hand_side += context.lower_bound(x_i) as i64;
            self.current_bounds[i] = context.new_trailed_integer(context.lower_bound(x_i) as i64);
        });
        self.lower_bound_left_hand_side = context.new_trailed_integer(lower_bound_left_hand_side);

        if let Some(conjunction) = self.detect_inconsistency(context.as_trailed_readonly()) {
            Err(conjunction)
        } else {
            Ok(())
        }
    }

    fn detect_inconsistency(
        &self,
        context: PropagationContextWithTrailedValues,
    ) -> Option<PropositionalConjunction> {
        if (self.c as i64) < context.value(self.lower_bound_left_hand_side) {
            Some(self.create_conflict_reason(context.as_readonly()))
        } else {
            None
        }
    }

    fn notify(
        &mut self,
        mut context: PropagationContextWithTrailedValues,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let index = local_id.unpack() as usize;
        let x_i = &self.x[index];

        let old_bound = context.value(self.current_bounds[index]);
        let new_bound = context.lower_bound(x_i) as i64;

        pumpkin_assert_simple!(
            old_bound < new_bound,
            "propagator should only be triggered when lower bounds are tightened, old_bound={old_bound}, new_bound={new_bound}"
        );

        context.add_assign(self.lower_bound_left_hand_side, new_bound - old_bound);
        context.assign(self.current_bounds[index], new_bound);

        EnqueueDecision::Enqueue
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        if let Some(conjunction) = self.detect_inconsistency(context.as_trailed_readonly()) {
            return Err(conjunction.into());
        }

        let lower_bound_left_hand_side =
            match TryInto::<i32>::try_into(context.value(self.lower_bound_left_hand_side)) {
                Ok(bound) => bound,
                Err(_) if context.value(self.lower_bound_left_hand_side).is_positive() => {
                    // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                    // overflow (hence the check that the lower-bound on the left-hand side is
                    // positive)
                    //
                    // This means that the lower-bounds of the current variables will always be
                    // higher than the right-hand side (with a maximum value of i32). We thus
                    // return a conflict
                    return Err(self.create_conflict_reason(context.as_readonly()).into());
                }
                Err(_) => {
                    // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                    // underflow
                    //
                    // This means that the constraint is always satisfied
                    return Ok(());
                }
            };

        for (i, x_i) in self.x.iter().enumerate() {
            let bound = self.c - (lower_bound_left_hand_side - context.lower_bound(x_i));

            if context.upper_bound(x_i) > bound {
                let reason: PropositionalConjunction = self
                    .x
                    .iter()
                    .enumerate()
                    .filter_map(|(j, x_j)| {
                        if j != i {
                            Some(predicate![x_j >= context.lower_bound(x_j)])
                        } else {
                            None
                        }
                    })
                    .collect();

                context.set_upper_bound(x_i, bound, reason)?;
            }
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let lower_bound_left_hand_side = self
            .x
            .iter()
            .map(|var| context.lower_bound(var) as i64)
            .sum::<i64>();

        let lower_bound_left_hand_side = match TryInto::<i32>::try_into(lower_bound_left_hand_side)
        {
            Ok(bound) => bound,
            Err(_) if context.value(self.lower_bound_left_hand_side).is_positive() => {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // overflow (hence the check that the lower-bound on the left-hand side is
                // positive)
                //
                // This means that the lower-bounds of the current variables will always be
                // higher than the right-hand side (with a maximum value of i32). We thus
                // return a conflict
                return Err(self.create_conflict_reason(context.as_readonly()).into());
            }
            Err(_) => {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // underflow
                //
                // This means that the constraint is always satisfied
                return Ok(());
            }
        };

        for (i, x_i) in self.x.iter().enumerate() {
            let bound = self.c - (lower_bound_left_hand_side - context.lower_bound(x_i));

            if context.upper_bound(x_i) > bound {
                let reason: PropositionalConjunction = self
                    .x
                    .iter()
                    .enumerate()
                    .filter_map(|(j, x_j)| {
                        if j != i {
                            Some(predicate![x_j >= context.lower_bound(x_j)])
                        } else {
                            None
                        }
                    })
                    .collect();

                context.set_upper_bound(x_i, bound, reason)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_solver::TestSolver;

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let propagator = solver
            .new_propagator(LinearLessOrEqualPropagator::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_explanations() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let propagator = solver
            .new_propagator(LinearLessOrEqualPropagator::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y <= 6]);

        assert_eq!(conjunction!([x >= 1]), reason);
    }

    #[test]
    fn overflow_leads_to_conflict() {
        let mut solver = TestSolver::default();

        let x = solver.new_variable(i32::MAX, i32::MAX);
        let y = solver.new_variable(1, 1);

        let _ = solver
            .new_propagator(LinearLessOrEqualPropagator::new([x, y].into(), i32::MAX))
            .expect_err("Expected overflow to be detected");
    }

    #[test]
    fn underflow_leads_to_no_propagation() {
        let mut solver = TestSolver::default();

        let x = solver.new_variable(i32::MIN, i32::MIN);
        let y = solver.new_variable(-1, -1);

        let _ = solver
            .new_propagator(LinearLessOrEqualPropagator::new([x, y].into(), i32::MIN))
            .expect("Expected no error to be detected");
    }
}
