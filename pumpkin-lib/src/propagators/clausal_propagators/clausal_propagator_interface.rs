use crate::{
    basic_types::{ClauseReference, ConflictInfo, ConstraintOperationError, Literal},
    engine::{
        constraint_satisfaction_solver::ClauseAllocator, AssignmentsPropositional,
        ExplanationClauseManager,
    },
};

pub trait ClausalPropagatorInterface {
    fn grow(&mut self);

    fn get_literal_propagation_clause_reference(
        &self,
        propagated_literal: Literal,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
        explanation_clause_manager: &mut ExplanationClauseManager,
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
