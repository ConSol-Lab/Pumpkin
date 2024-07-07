use std::ops::Not;

use crate::basic_types::ConstraintOperationError;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::AssignmentsInteger;

// todo: at the time of creation, the variables get subscribe to the propagator, but later creating
// new variables does not subscribe them automatically!
// todo: hardcode the notifications.
#[derive(Debug, Clone, Copy)]
pub struct NogoodPropagatorConstructor {}

impl PropagatorConstructor for NogoodPropagatorConstructor {
    type Propagator = NogoodPropagator;

    fn create(self, mut _context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        NogoodPropagator { nogoods: vec![] }
    }
}

#[derive(Default, Clone, Debug)]
pub struct NogoodPropagator {
    nogoods: Vec<Vec<IntegerPredicate>>,
}

// Probably the nogood propagator is just a propagator with additional functionality.
// But somehow all variables are subscribed to it? Actually subscribing goes dynamically!
// Or it is simply treated in a special way...I suppose the nogood propagator is a special case
// 'global' propagator that is potentially tied to all variables? Actually it only cares about
// specific predicates, but this is probably best if it internally tracks this.
// Need to ensure that it is always enqueued!
impl NogoodPropagator {
    #[allow(dead_code)]
    fn add_permanent_nogood(
        &mut self,
        _nogood: Vec<IntegerPredicate>,
        _assignments: &mut AssignmentsInteger,
    ) -> Result<(), ConstraintOperationError> {
        // we do not need this for now?
        todo!();
    }

    #[allow(dead_code)]
    fn is_propagation_complete(&self, _trail_size: usize) -> bool {
        todo!();
    }

    fn propagate_nogood(
        &self,
        nogood: &[IntegerPredicate],
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        // Inefficient way of propagating, but okay for testing purposes
        // Explicitly goes through each predicate, and does multiple passes.

        let num_falsified_predicates = nogood
            .iter()
            .filter(|predicate| context.does_integer_predicate_hold(predicate.not()))
            .count();

        // if at least one predicate is false, then the nogood can be skipped
        if num_falsified_predicates > 0 {
            return Ok(());
        }

        let num_satisfied_predicates = nogood
            .iter()
            .filter(|predicate| context.does_integer_predicate_hold(**predicate))
            .count();

        assert!(num_satisfied_predicates + num_falsified_predicates <= nogood.len());

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood.len() {
            let conjunction: PropositionalConjunction = nogood
                .iter()
                .map(|integer_predicate| Predicate::IntegerPredicate(*integer_predicate))
                .collect();
            return Err(conjunction.into());
        }
        // If all but one predicate are satisfied, then we can propagate.
        // Note that this only makes sense since we know that there are no falsifying predicates at
        // this point.
        else if num_satisfied_predicates == nogood.len() - 1 {
            // Note that we negate the remaining unassigned predicate!
            let propagated_predicate = nogood
                .iter()
                .find(|predicate| !context.does_integer_predicate_hold(**predicate))
                .unwrap()
                .not();
            assert!(nogood.iter().any(|p| *p == propagated_predicate.not()));

            let reason: PropositionalConjunction = nogood
                .iter()
                .filter_map(|predicate| {
                    if context.does_integer_predicate_hold(*predicate) {
                        Some(Predicate::IntegerPredicate(*predicate))
                    } else {
                        None
                    }
                })
                .collect();
            context.apply_integer_predicate(propagated_predicate, reason)?;
        }
        Ok(())
    }
}

impl Propagator for NogoodPropagator {
    fn name(&self) -> &str {
        // In the current hacky version, it is important to keep this name exactly this.
        // The solver looks for a propagator with this name and adds the learned nogood.
        "NogoodPropagator"
    }

    fn priority(&self) -> u32 {
        0
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        // Very inefficient version!

        // The algorithm goes through every nogood explicitly
        // and computes from scratch.
        for nogood in &self.nogoods {
            self.propagate_nogood(nogood, context)?;
        }
        Ok(())
    }

    // Learned nogood during search.
    // Assumption is that it is propagating, and that the propagating predicate is in position [0].
    // Actually the above assumption is not needed in the current version.
    #[allow(dead_code)]
    fn hack_add_asserting_nogood(
        &mut self,
        nogood: Vec<IntegerPredicate>,
        // context: &mut PropagationContextMut,
    ) {
        self.nogoods.push(nogood);
        // let result = self.propagate_nogood(&self.nogoods.last().unwrap(), context);
        // assert!(result.is_ok());
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    // use crate::conjunction;
    // use crate::engine::test_helper::TestSolver;
    // use crate::predicate;

    // detect unsat
    // detect propagation correctly
    //
    // check reason for unsat
    // check reason for propagation
    //
    // inconsistent nogood -> never propagates
    //
    // nogood with redundant predicate

    #[test]
    fn detect_unsat() {
        // let mut solver = TestSolver::default();
        // let a = solver.new_variable(1, 3);
        // let b = solver.new_variable(0, 4);
        // let c = solver.new_variable(-10, 20);
        //
        // let mut propagator = solver
        // .new_propagator(NogoodPropagator { a, b, c })
        // .expect("no empty domains");
        // solver.propagate(&mut propagator).expect("no empty domains");
        //
        // assert_eq!(1, solver.lower_bound(a));
        // assert_eq!(3, solver.upper_bound(a));
        // assert_eq!(0, solver.lower_bound(b));
        // assert_eq!(4, solver.upper_bound(b));
        // assert_eq!(0, solver.lower_bound(c));
        // assert_eq!(12, solver.upper_bound(c));
        //
        // let reason_lb = solver.get_reason_int(predicate![c >= 0].try_into().unwrap());
        // assert_eq!(conjunction!([a >= 0] & [b >= 0]), *reason_lb);
        //
        // let reason_ub = solver.get_reason_int(predicate![c <= 12].try_into().unwrap());
        // assert_eq!(
        // conjunction!([a >= 0] & [a <= 3] & [b >= 0] & [b <= 4]),
        // reason_ub
        // );
    }
}
