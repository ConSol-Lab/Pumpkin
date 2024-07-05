use crate::basic_types::StoredConflictInfo;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::constraint_satisfaction_solver::Counters;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::PropagationContext;
use crate::engine::reason::ReasonStore;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;

#[derive(Debug)]
pub struct ConflictAnalysisNogoodContext<'a> {
    pub assignments_integer: &'a AssignmentsInteger,
    pub assignments_propositional: &'a AssignmentsPropositional,
    pub solver_state: &'a mut CSPSolverState,
    pub reason_store: &'a mut ReasonStore,
    pub counters: &'a mut Counters,
}

impl<'a> ConflictAnalysisNogoodContext<'a> {
    pub fn get_conflict_nogood(&self) -> Vec<Predicate> {
        match self.solver_state.get_conflict_info() {
            StoredConflictInfo::Explanation {
                conjunction,
                propagator: _,
            } => conjunction.iter().copied().collect(),
            StoredConflictInfo::VirtualBinaryClause { lit1: _, lit2: _ } => unreachable!(),
            StoredConflictInfo::Propagation {
                reference: _,
                literal: _,
            } => unreachable!(),
        }
    }

    pub fn get_propagation_reason(&mut self, predicate: &IntegerPredicate) -> Vec<Predicate> {
        let trail_position = self
            .assignments_integer
            .get_trail_position(predicate)
            .expect("The predicate must be true during conflict analysis.");
        let propagation_context =
            PropagationContext::new(self.assignments_integer, self.assignments_propositional);
        let trail_entry = self.assignments_integer.get_trail_entry(trail_position);
        // We distinguish between two cases:
        // 1) The predicate is explicitly present on the trail.
        if trail_entry.predicate == *predicate {
            // In this case, we can simply return the reason given on the trail.
            let reason_ref = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");
            // let propagator = self.reason_store.get_propagator(reason_ref);
            let reason = self
                .reason_store
                .get_or_compute(reason_ref, &propagation_context)
                .expect("reason reference should not be stale");
            // todo avoid excessive copying in the future
            reason.iter().copied().collect()
        }
        // 2) The predicate is true, but not explicitly on the trail. It is necessary to further
        // analyse what was the reason for setting the predicate true.
        else {
            // The reason for propagation depends on the predicate on the trail at the moment the
            // input predicate became true.
            // match trail_entry.predicate {
            // IntegerPredicate::LowerBound {
            // domain_id,
            // lower_bound,
            // } => todo!(),
            // IntegerPredicate::UpperBound {
            // domain_id,
            // upper_bound,
            // } => todo!(),
            // IntegerPredicate::NotEqual {
            // domain_id,
            // not_equal_constant,
            // } => todo!(),
            // IntegerPredicate::Equal {
            // domain_id,
            // equality_constant,
            // } => unreachable!(),
            // }
            todo!();
        }
    }
}
