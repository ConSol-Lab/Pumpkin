use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::pumpkin_assert_simple;

/// Propagator for the constraint `reif => \sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct LinearLessOrEqualPropagator<Var> {
    x: Box<[Var]>,
    c: i32,

    /// The lower bound of the sum of the left-hand side. This is incremental state.
    lower_bound_left_hand_side: i64,
    /// The value at index `i` is the bound for `x[i]`.
    current_bounds: Box<[i32]>,
}

impl<Var> LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    pub(crate) fn new(x: Box<[Var]>, c: i32) -> Self {
        let current_bounds = vec![0; x.len()].into();

        // incremental state will be properly initialized in `Propagator::initialise_at_root`.
        LinearLessOrEqualPropagator::<Var> {
            x,
            c,
            lower_bound_left_hand_side: 0,
            current_bounds,
        }
    }

    /// Recalculates the incremental state from scratch.
    fn recalculate_incremental_state(&mut self, context: PropagationContext) {
        self.lower_bound_left_hand_side = self
            .x
            .iter()
            .map(|var| context.lower_bound(var) as i64)
            .sum();

        self.current_bounds
            .iter_mut()
            .enumerate()
            .for_each(|(index, bound)| {
                *bound = context.lower_bound(&self.x[index]);
            });
    }
}

impl<Var> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        self.x.iter().enumerate().for_each(|(i, x_i)| {
            let _ = context.register(
                x_i.clone(),
                DomainEvents::LOWER_BOUND,
                LocalId::from(i as u32),
            );
        });

        self.recalculate_incremental_state(context.as_readonly());

        if let Some(conjunction) = self.detect_inconsistency(context.as_readonly()) {
            Err(conjunction)
        } else {
            Ok(())
        }
    }

    fn detect_inconsistency(
        &self,
        context: PropagationContext,
    ) -> Option<PropositionalConjunction> {
        if (self.c as i64) < self.lower_bound_left_hand_side {
            let reason: PropositionalConjunction = self
                .x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect();
            Some(reason)
        } else {
            None
        }
    }

    fn notify(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let index = local_id.unpack() as usize;

        let x_i = &self.x[index];
        let old_bound = self.current_bounds[index];
        let new_bound = context.lower_bound(x_i);

        pumpkin_assert_simple!(
            old_bound < new_bound,
            "propagator should only be triggered when lower bounds are tightened, old_bound={old_bound}, new_bound={new_bound}"
        );

        self.current_bounds[index] = new_bound;
        self.lower_bound_left_hand_side += (new_bound - old_bound) as i64;

        EnqueueDecision::Enqueue
    }

    fn synchronise(&mut self, context: PropagationContext) {
        self.recalculate_incremental_state(context);
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        if let Some(conjunction) = self.detect_inconsistency(context.as_readonly()) {
            return Err(conjunction.into());
        }

        for (i, x_i) in self.x.iter().enumerate() {
            let bound = (self.c as i64
                - (self.lower_bound_left_hand_side - context.lower_bound(x_i) as i64))
                .try_into()
                .expect("Could not fit the lower-bound of lhs in an i32");

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
    use crate::engine::test_helper::TestSolver;

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(LinearLessOrEqualPropagator::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_explanations() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(LinearLessOrEqualPropagator::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y <= 6].try_into().unwrap());

        assert_eq!(conjunction!([x >= 1]), *reason);
    }
}
