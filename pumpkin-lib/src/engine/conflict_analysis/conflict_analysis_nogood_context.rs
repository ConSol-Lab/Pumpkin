use crate::engine::constraint_satisfaction_solver::Counters;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::AssignmentsInteger;

#[derive(Debug)]
pub struct ConflictAnalysisNogoodContext<'a> {
    pub assignments_integer: &'a AssignmentsInteger,
    pub counters: &'a mut Counters,
}

impl<'a> ConflictAnalysisNogoodContext<'a> {
    pub fn get_conflict_nogood(&self) -> Vec<Predicate> {
        todo!();
    }

    pub fn get_propagation_reason(&self, _predicate: &IntegerPredicate) -> Vec<Predicate> {
        todo!();
    }
}
