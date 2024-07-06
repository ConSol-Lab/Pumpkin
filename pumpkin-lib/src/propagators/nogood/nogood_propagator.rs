// use crate::basic_types::ConflictInfo;
use crate::basic_types::ConstraintOperationError;
use crate::basic_types::PropagationStatusCP;
// use crate::basic_types::PropositionalConjunction;
// use crate::engine::predicates::integer_predicate;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
// use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::AssignmentsInteger;

#[derive(Default, Clone, Debug)]
pub struct NogoodPropagator {
    nogoods: Vec<Vec<IntegerPredicate>>,
}

// probably the nogood propagator is just a propagator with additional functionality.
// todo implement the propagator trait later
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

    /// Learned nogood during search.
    /// Assumption is that it is propagating, and that the propagating predicate is in position [0].
    #[allow(dead_code)]
    fn add_asserting_nogood(
        &mut self,
        nogood: Vec<IntegerPredicate>,
        _assignments: &mut AssignmentsInteger,
    ) {
        self.nogoods.push(nogood);
    }

    #[allow(dead_code)]
    fn is_propagation_complete(&self, _trail_size: usize) -> bool {
        todo!();
    }
}

impl Propagator for NogoodPropagator {
    fn name(&self) -> &str {
        todo!()
    }

    fn debug_propagate_from_scratch(
        &self,
        _context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        todo!();
        // for nogood in &self.nogoods {
        // let num_satisfied_predicates = nogood
        // .iter()
        // .filter(|predicate| assignments.does_integer_predicate_hold(**predicate))
        // .count();
        //
        // conflict detected
        // if num_satisfied_predicates == nogood.len() {
        // let conjunction: PropositionalConjunction = nogood
        // .iter()
        // .map(|predicate| Predicate::IntegerPredicate(*predicate))
        // .collect();
        // return Err(ConflictInfo::Explanation(conjunction));
        // } else if num_satisfied_predicates == nogood.len() - 1 {
        // let propagated_predicate = nogood
        // .iter()
        // .find(|predicate| !assignments.does_integer_predicate_hold(**predicate))
        // .unwrap();
        //
        // let reason: PropositionalConjunction = nogood
        // .iter()
        // .filter_map(|predicate| {
        // if assignments.does_integer_predicate_hold(*predicate) {
        // Some(Predicate::IntegerPredicate(*predicate))
        // } else {
        // None
        // }
        // })
        // .collect();
        // assignments.apply_integer_predicate(propagated_predicate, reason)
        // }
        // }
        // todo!()
    }
}
