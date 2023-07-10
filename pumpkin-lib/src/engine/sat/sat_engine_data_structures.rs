use crate::{
    basic_types::{BranchingDecision, ClauseReference, Literal},
    engine::constraint_satisfaction_solver::ClauseAllocator,
    pumpkin_assert_moderate, pumpkin_assert_simple,
};

use super::{AssignmentsPropositional, PropositionalValueSelector, PropositionalVariableSelector};

#[derive(Default)]
pub struct SATEngineDataStructures {
    pub assignments_propositional: AssignmentsPropositional,
    pub propositional_variable_selector: PropositionalVariableSelector,
    pub propositional_value_selector: PropositionalValueSelector,
    pub clause_allocator: ClauseAllocator,
    pub assumptions: Vec<Literal>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum LearnedClauseSortingStrategy {
    Activity,
    LBD,
}

impl std::fmt::Display for LearnedClauseSortingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LearnedClauseSortingStrategy::LBD => write!(f, "lbd"),
            LearnedClauseSortingStrategy::Activity => write!(f, "activity"),
        }
    }
}

impl SATEngineDataStructures {
    fn peek_next_assumption_literal(&self) -> Option<Literal> {
        assert!(
            self.assumptions.is_empty(),
            "Assumptions are not yet supported!"
        );
        None
    }

    pub fn get_next_branching_decision(&mut self) -> Option<BranchingDecision> {
        if let Some(assumption_literal) = self.peek_next_assumption_literal() {
            return Some(BranchingDecision::Assumption { assumption_literal });
        }

        if let Some(decision_variable) = self
            .propositional_variable_selector
            .peek_next_variable(&self.assignments_propositional)
        {
            let selected_value = self
                .propositional_value_selector
                .select_value(decision_variable);

            let decision_literal = Literal::new(decision_variable, selected_value);

            pumpkin_assert_moderate!(self
                .assignments_propositional
                .is_literal_unassigned(decision_literal));

            Some(BranchingDecision::StandardDecision { decision_literal })
        } else {
            None
        }
    }

    pub fn backtrack(&mut self, backtrack_level: u32) {
        pumpkin_assert_simple!(
            backtrack_level < self.assignments_propositional.get_decision_level()
        );

        let num_assignments_for_removal = self.assignments_propositional.trail.len()
            - self.assignments_propositional.trail_delimiter[backtrack_level as usize] as usize;

        for _i in 0..num_assignments_for_removal {
            let last_literal = self.assignments_propositional.pop_trail();

            self.propositional_variable_selector
                .restore(last_literal.get_propositional_variable());

            self.propositional_value_selector.update_if_not_frozen(
                last_literal.get_propositional_variable(),
                last_literal.is_positive(),
            );
        }

        self.assignments_propositional.synchronise(backtrack_level);
    }

    //does simple preprocessing, modifying the input vector of literals
    //	removes duplicate literals
    //	removes falsified literals at the root
    //	if the same variable appears with both polarities or there is a literal that is true at the root, removes all literals from the clause and adds a literal that is true at the root
    //	if the clause is violated at the root, it will become empty
    //  if the clause is satisfied at the root, its content will be changed to only include the true_literal
    //this preprocessing is also for correctness, i.e., clauses should not have duplicated literals for instance
    pub fn preprocess_clause(
        mut literals: Vec<Literal>,
        assignments: &AssignmentsPropositional,
    ) -> Vec<Literal> {
        //the code below is broken down into several parts, could be done more efficiently but probably makes no/little difference

        //remove literals that are falsified at the root level
        //also check if the clause has a true literal at the root level
        let mut satisfied_at_root = false;
        let mut next_location = 0;
        for i in 0..literals.len() {
            if assignments.is_literal_assigned_true(literals[i]) {
                satisfied_at_root = true;
                break;
            }
            //skip falsified literals, only keep unassigned literals
            else if assignments.is_literal_unassigned(literals[i]) {
                literals[next_location] = literals[i];
                next_location += 1;
            }
        }
        literals.truncate(next_location);

        //if satisfied at the root, then remove all literals, add a true literal to the clause, and stop
        if satisfied_at_root {
            literals.resize(1, assignments.true_literal);
            literals[0] = assignments.true_literal;
            return literals;
        }
        //in case the literal is empty, it is unsatisfied at the root, stop
        else if literals.is_empty() {
            return literals;
        }

        // we now remove duplicated literals
        //	the easiest way is to sort the literals and only keep one literal of the same type
        literals.sort_unstable_by_key(|a| a.to_u32());
        next_location = 1;
        for i in 1..literals.len() {
            if literals[i] != literals[next_location - 1] {
                literals[next_location] = literals[i];
                next_location += 1;
            }
        }
        literals.truncate(next_location);

        //check if the clause contains both polarities of the same variable
        //	since we removed duplicates and the literals are sorted, it suffices to check if two neighbouring literals share the same variable
        for i in 1..literals.len() {
            if literals[i - 1].get_propositional_variable()
                == literals[i].get_propositional_variable()
            {
                satisfied_at_root = true;
                break;
            }
        }

        if satisfied_at_root {
            literals.truncate(1);
            literals[0] = assignments.true_literal;
        }

        literals
    }

    pub fn is_clause_propagating(&self, clause_reference: ClauseReference) -> bool {
        pumpkin_assert_simple!(
            clause_reference.is_allocated_clause(),
            "Virtual clause support not yet implemented."
        );

        //we determine whether the clause is propagating by using the following reasoning:
        //  the literal at position 0 is set to true - this is the convention with the clausal propagator
        //  the reason for propagation of the literal is the input clause

        //the code could be simplified

        let propagated_literal = self.clause_allocator[clause_reference][0];
        if self
            .assignments_propositional
            .is_literal_assigned_true(propagated_literal)
        {
            let reason_constraint = self
                .assignments_propositional
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
}
