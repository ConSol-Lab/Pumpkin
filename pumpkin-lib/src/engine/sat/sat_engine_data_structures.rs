use crate::basic_types::BranchingDecision;
use crate::basic_types::ClauseReference;
use crate::basic_types::Literal;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::sat::AssignmentsPropositional;
use crate::engine::sat::PropositionalValueSelector;
use crate::engine::sat::PropositionalVariableSelector;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Default, Debug)]
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
    pub fn are_all_assumptions_assigned(&self) -> bool {
        self.assignments_propositional.get_decision_level() > self.assumptions.len()
    }

    fn peek_next_assumption_literal(&self) -> Option<Literal> {
        if self.are_all_assumptions_assigned() {
            None
        } else {
            //the convention is that at decision level i, the (i-1)th assumption is set
            //  note that the decision level is increased before calling branching hence the minus one
            Some(self.assumptions[self.assignments_propositional.get_decision_level() - 1])
        }
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

    pub fn backtrack(&mut self, backtrack_level: usize) {
        pumpkin_assert_simple!(
            backtrack_level < self.assignments_propositional.get_decision_level()
        );

        self.assignments_propositional
            .synchronise(backtrack_level)
            .for_each(|literal| {
                self.propositional_variable_selector
                    .restore(literal.get_propositional_variable());

                self.propositional_value_selector.update_if_not_frozen(
                    literal.get_propositional_variable(),
                    literal.is_positive(),
                );
            });
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
