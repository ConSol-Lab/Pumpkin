use crate::basic_types::ConflictInfo;
use crate::basic_types::ConstraintOperationError;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::AssignmentsInteger;

#[derive(Default, Clone, Copy, Debug)]
pub struct NogoodPropagator {}

// probably the nogood propagator is just a regular propagator
// todo implement the propagator trait later
impl NogoodPropagator {
    #[allow(dead_code)]
    fn add_permanent_nogood(
        &mut self,
        _nogood: Vec<Predicate>,
        _assignments: &mut AssignmentsInteger,
    ) -> Result<(), ConstraintOperationError> {
        todo!();
    }

    /// Learned nogood during search.
    /// Assumption is that it is propagating, and that the propagating predicate is in position [0].
    #[allow(dead_code)]
    fn add_asserting_nogood(
        &mut self,
        _nogood: Vec<Predicate>,
        _assignments: &mut AssignmentsInteger,
    ) {
        todo!();
    }

    #[allow(dead_code)]
    fn propagate(&mut self, _assignments: &mut AssignmentsInteger) -> Result<(), ConflictInfo> {
        todo!();
    }

    #[allow(dead_code)]
    fn synchronise(&mut self, _trail_size: usize) {
        todo!();
    }

    #[allow(dead_code)]
    fn is_propagation_complete(&self, _trail_size: usize) -> bool {
        todo!();
    }
}
