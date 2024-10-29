use crate::basic_types::ClauseReference;
use crate::basic_types::ConflictInfo;
use crate::basic_types::ConstraintOperationError;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::variables::Literal;
use crate::engine::AssignmentsPropositional;
use crate::pumpkin_assert_simple;

pub(crate) trait ClausalPropagator {
    fn grow(&mut self);

    fn get_literal_propagation_clause_reference(
        &self,
        propagated_literal: Literal,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> ClauseReference;

    fn add_permanent_clause(
        &mut self,
        literals: Vec<Literal>,
        assignments: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> Result<(), ConstraintOperationError>;

    fn add_asserting_learned_clause(
        &mut self,
        literals: Vec<Literal>,
        assignments: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> Option<ClauseReference>;

    fn add_clause_unchecked(
        &mut self,
        literals: Vec<Literal>,
        is_learned: bool,
        clause_allocator: &mut ClauseAllocator,
    ) -> Option<ClauseReference>;

    fn add_permanent_implication_unchecked(
        &mut self,
        lhs: Literal,
        rhs: Literal,
        clause_allocator: &mut ClauseAllocator,
    );

    fn add_permanent_ternary_clause_unchecked(
        &mut self,
        a: Literal,
        b: Literal,
        c: Literal,
        clause_allocator: &mut ClauseAllocator,
    );

    fn propagate(
        &mut self,
        assignments: &mut AssignmentsPropositional,
        clause_manager: &mut ClauseAllocator,
    ) -> Result<(), ConflictInfo>;

    fn synchronise(&mut self, trail_size: usize);

    fn is_propagation_complete(&self, trail_size: usize) -> bool;

    fn remove_clause_from_consideration(
        &mut self,
        clause: &[Literal],
        clause_reference: ClauseReference,
    );

    fn debug_check_state(
        &self,
        assignments: &AssignmentsPropositional,
        clause_allocator: &ClauseAllocator,
    ) -> bool;
}

/// We determine whether the clause is propagating by using the following reasoning:
///     * the literal at position 0 is set to true. This is the convention with the clausal
/// propagator.
///     * the reason for propagation of the literal is the input clause.
pub(crate) fn is_clause_propagating(
    assignments_propositional: &AssignmentsPropositional,
    clause_allocator: &ClauseAllocator,
    clause_reference: ClauseReference,
) -> bool {
    pumpkin_assert_simple!(
        clause_reference.is_allocated_clause(),
        "Virtual clause support not yet implemented."
    );

    // the code could be simplified

    let propagated_literal = clause_allocator[clause_reference][0];
    if assignments_propositional.is_literal_assigned_true(propagated_literal) {
        let reason_constraint = assignments_propositional
            .get_variable_reason_constraint(propagated_literal.get_propositional_variable());

        if reason_constraint.is_clause() {
            let reason_clause: ClauseReference = reason_constraint.into();
            reason_clause == clause_reference
        } else {
            false
        }
    } else {
        false
    }
}
