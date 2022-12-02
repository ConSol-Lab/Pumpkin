use crate::{
    arguments::ArgumentHandler,
    basic_types::{
        BranchingDecision, ClauseAdditionOutcome, ClauseReference, Literal,
        PropagationStatusClausal,
    },
    propagators::ClausalPropagator,
};

use super::{
    AssignmentsPropositional, ClauseAllocator, PropositionalValueSelector,
    PropositionalVariableSelector,
};

use crate::pumpkin_asserts::*;

pub struct SATEngineDataStructures {
    pub assignments_propositional: AssignmentsPropositional,
    pub propositional_variable_selector: PropositionalVariableSelector,
    pub propositional_value_selector: PropositionalValueSelector,
    pub clausal_propagator: ClausalPropagator,
    pub clause_allocator: ClauseAllocator,
    pub permanent_clauses: Vec<ClauseReference>,
    pub learned_clauses: Vec<ClauseReference>,
    pub explanation_clauses: Vec<ClauseReference>,
    pub assumptions: Vec<Literal>,
    parameters: SATDataStructuresInternalParameters,
    clause_bump_increment: f32,
}

impl SATEngineDataStructures {
    pub fn new(argument_handler: &ArgumentHandler) -> SATEngineDataStructures {
        SATEngineDataStructures {
            assignments_propositional: AssignmentsPropositional::new(),
            clausal_propagator: ClausalPropagator::new(),
            clause_allocator: ClauseAllocator::new(),
            permanent_clauses: vec![],
            learned_clauses: vec![],
            explanation_clauses: vec![],
            propositional_variable_selector: PropositionalVariableSelector::new(),
            propositional_value_selector: PropositionalValueSelector::new(),
            assumptions: vec![],
            parameters: SATDataStructuresInternalParameters::new(argument_handler),
            clause_bump_increment: 1.0,
        }
    }

    pub fn propagate_clauses(&mut self) -> PropagationStatusClausal {
        self.clausal_propagator.propagate(
            &mut self.assignments_propositional,
            &mut self.clause_allocator,
        )
    }

    pub fn add_permanent_clause(&mut self, literals: Vec<Literal>) -> ClauseAdditionOutcome {
        pumpkin_assert_ne_simple!(literals.len(), 0);
        pumpkin_assert_simple!(self.assignments_propositional.is_at_the_root_level());
        //pumpkin_assert_simple!(self.is_propagation_complete()); hehe
        //pumpkin_assert_permanent(state_.IsPropagationComplete(), "Adding clauses is currently only possible once all propagation has been done.");

        let literals =
            SATEngineDataStructures::preprocess_clause(literals, &self.assignments_propositional);

        //infeasible at the root? Note that we do not add the original clause to the database in this case
        if literals.is_empty() {
            return ClauseAdditionOutcome::Infeasible;
        }

        //is unit clause? Unit clauses are added as root assignments, rather than as actual clauses
        //	in case the clause is satisfied at the root, the PreprocessClause method will return a unit clause with a literal that is satisfied at the root

        //add clause unit
        if literals.len() == 1 {
            pumpkin_assert_simple!(!self
                .assignments_propositional
                .is_literal_assigned_false(literals[0]), "Conflict detected at the root level, this is not an error but for now we do not handle this case properly so we abort.");
            if self
                .assignments_propositional
                .is_literal_unassigned(literals[0])
            {
                self.assignments_propositional
                    .enqueue_decision_literal(literals[0]);
                let outcome = self.propagate_clauses();
                pumpkin_assert_simple!(outcome.no_conflict(), "Conflict detected at the root level, this is not an error but for now we do not handle this case properly so we abort.");
            }
        } else {
            //standard case - the clause has at least two unassigned literals
            self.add_clause_unchecked(literals, false);
        }

        ClauseAdditionOutcome::NoConflictDetected
    }

    pub fn add_clause_unchecked(
        &mut self,
        literals: Vec<Literal>,
        is_learned: bool,
    ) -> ClauseReference {
        pumpkin_assert_ne_moderate!(literals.len(), 0);
        pumpkin_assert_moderate!(
            self.clausal_propagator
                .is_propagation_complete(self.assignments_propositional.trail.len()),
            "Adding clauses is currently only possible once all propagation has been done."
        );

        let clause_reference = self.clause_allocator.create_clause(literals, is_learned);
        let clause = self.clause_allocator.get_clause(clause_reference);

        self.permanent_clauses.push(clause_reference);
        self.clausal_propagator
            .start_watching_clause_unchecked(clause, clause_reference);

        clause_reference
    }

    pub fn add_explanation_clause_unchecked(
        &mut self,
        explanation_literals: Vec<Literal>,
    ) -> ClauseReference {
        pumpkin_assert_moderate!(explanation_literals.len() >= 2);

        let clause_reference = self
            .clause_allocator
            .create_clause(explanation_literals, false);

        self.explanation_clauses.push(clause_reference);

        clause_reference
    }

    pub fn add_permanent_implication_unchecked(&mut self, lhs: Literal, rhs: Literal) {
        self.add_clause_unchecked(vec![!lhs, rhs], false);
    }

    pub fn add_permanent_ternary_clause_unchecked(&mut self, a: Literal, b: Literal, c: Literal) {
        self.add_clause_unchecked(vec![a, b, c], false);
    }

    pub fn shrink_learned_clause_database_if_needed(&mut self) {
        pumpkin_assert_moderate!(
            self.assignments_propositional.is_at_the_root_level(),
            "For now learned clause reductions can only be done at the root level."
        );

        if self.learned_clauses.len() <= self.parameters.num_learned_clauses_max as usize {
            return;
        }

        //sort learned clauses
        //the ordering is such that the better clauses are in the front
        //  note that this is not the most efficient sorting comparison, but will do for now
        //  e.g., sort_by_lbd could be moved out, and the comparison of floats could be changed possibly
        self.learned_clauses
            .sort_unstable_by(|clause_reference1, clause_reference2| {
                let clause1 = self.clause_allocator.get_clause(*clause_reference1);
                let clause2 = self.clause_allocator.get_clause(*clause_reference2);

                match self.parameters.learned_clause_sorting_strategy {
                    LearnedClauseSortingStrategy::Activity => {
                        //note that here we reverse clause1 and clause2, because a higher value for activity is better
                        clause2
                            .get_activity()
                            .partial_cmp(&clause1.get_activity())
                            .unwrap()
                    }
                    LearnedClauseSortingStrategy::Lbd => {
                        if clause1.get_lbd() != clause2.get_lbd() {
                            clause1.get_lbd().cmp(&clause2.get_lbd())
                        } else {
                            //note that here we reverse clause1 and clause2, because a higher value for activity is better
                            clause2
                                .get_activity()
                                .partial_cmp(&clause1.get_activity())
                                .unwrap()
                        }
                    }
                }
            });

        //the clauses at the back of the array are the 'bad' clauses
        let mut num_clauses_to_remove =
            self.learned_clauses.len() as u64 - self.parameters.num_learned_clauses_max;
        for i in 0..self.learned_clauses.len() {
            if num_clauses_to_remove == 0 {
                break;
            }

            let i_rev = self.learned_clauses.len() - 1 - i;
            let clause_reference = self.learned_clauses[i_rev];

            if self.clause_allocator[clause_reference].is_protected_aganst_deletion() {
                self.clause_allocator[clause_reference].clear_protection_against_deletion();
                continue;
            }

            //remove clause
            //  clause removal is done in several steps

            //  remove the reference from the learned clause vector
            //      note that because some clauses may be protected from deletion, we need to do more than a simple 'pop' operation
            self.learned_clauses[i_rev] = *self.learned_clauses.last().unwrap();
            self.learned_clauses.pop();

            //  now remove the clause from the watch list
            self.clausal_propagator.remove_clause_consideration(
                &self.clause_allocator[clause_reference],
                clause_reference,
            );
            //  finally delete the clause
            self.clause_allocator.delete_clause(clause_reference);

            num_clauses_to_remove -= 1;
        }

        pumpkin_assert_extreme!(self
            .clausal_propagator
            .debug_check_state(&self.assignments_propositional, &self.clause_allocator));
    }

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

        self.clausal_propagator.synchronise(
            self.assignments_propositional
                .num_assigned_propositional_variables() as usize,
        );
    }

    pub fn clean_up_explanation_clauses(&mut self) {
        for clause_reference in self.explanation_clauses.iter().rev() {
            self.clause_allocator.delete_clause(*clause_reference);
        }
        self.explanation_clauses.clear();
    }

    pub fn update_clause_lbd_and_bump_activity(&mut self, clause_reference: ClauseReference) {
        if self
            .clause_allocator
            .get_clause(clause_reference)
            .is_learned()
            && self.clause_allocator.get_clause(clause_reference).get_lbd() > 2
        {
            self.bump_clause_activity(clause_reference);
            self.update_lbd(clause_reference);
        }
    }

    pub fn update_lbd(&mut self, clause_reference: ClauseReference) {
        let new_lbd = self
            .compute_lbd_for_literals(self.clause_allocator[clause_reference].get_literal_slice());
        if new_lbd < self.clause_allocator[clause_reference].get_lbd() {
            self.clause_allocator[clause_reference].update_lbd(new_lbd);
            self.clause_allocator[clause_reference].mark_protection_against_deletion();
        }
    }

    pub fn compute_lbd_for_literals(&self, literals: &[Literal]) -> u32 {
        pumpkin_assert_moderate!(
            literals
                .iter()
                .all(|lit| self.assignments_propositional.is_literal_assigned(*lit)),
            "Cannot compute LBD if not all literals are assigned."
        );
        //the LBD is the number of literals at different decision levels
        //  this implementation should be improved
        let mut codes: Vec<u32> = literals
            .iter()
            .map(|lit| {
                self.assignments_propositional
                    .get_literal_assignment_level(*lit)
            })
            .collect::<Vec<u32>>();
        codes.sort_unstable();
        codes.dedup();
        codes.len() as u32
    }

    pub fn bump_clause_activity(&mut self, clause_reference: ClauseReference) {
        //check if bumping the activity would lead to a large activity value
        if self
            .clause_allocator
            .get_clause(clause_reference)
            .get_activity()
            + self.clause_bump_increment
            > self.parameters.max_clause_activity
        {
            //if so, rescale all activity values
            self.rescale_clause_activities();
        }
        //at this point, it is safe to increase the activity value
        self.clause_allocator
            .get_mutable_clause(clause_reference)
            .increase_activity(self.clause_bump_increment);
    }

    pub fn rescale_clause_activities(&mut self) {
        self.learned_clauses.iter().for_each(|clause_reference| {
            let clause = self.clause_allocator.get_mutable_clause(*clause_reference);
            clause.divide_activity(self.parameters.max_clause_activity);
        });
        self.clause_bump_increment /= self.parameters.max_clause_activity;
    }

    pub fn decay_clause_activities(&mut self) {
        self.clause_bump_increment /= self.parameters.clause_activity_decay_factor;
    }

    pub fn is_clausal_propagation_at_fixed_point(&self) -> bool {
        self.clausal_propagator
            .is_propagation_complete(self.assignments_propositional.trail.len() as usize)
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
}

struct SATDataStructuresInternalParameters {
    pub num_learned_clauses_max: u64,
    pub max_clause_activity: f32,
    pub clause_activity_decay_factor: f32,
    pub learned_clause_sorting_strategy: LearnedClauseSortingStrategy,
}

impl SATDataStructuresInternalParameters {
    fn new(argument_handler: &ArgumentHandler) -> SATDataStructuresInternalParameters {
        SATDataStructuresInternalParameters {
            num_learned_clauses_max: argument_handler
                .get_integer_argument("threshold-learned-clauses")
                as u64,
            max_clause_activity: 1e20,
            clause_activity_decay_factor: 0.99,
            learned_clause_sorting_strategy:
                SATDataStructuresInternalParameters::parse_learned_clause_sorting_strategy(
                    argument_handler,
                ),
        }
    }

    fn parse_learned_clause_sorting_strategy(
        argument_handler: &ArgumentHandler,
    ) -> LearnedClauseSortingStrategy {
        let param = argument_handler.get_string_argument("learned-clause-sorting-strategy");
        match param.as_str() {
            "activity" => LearnedClauseSortingStrategy::Activity,
            "lbd" => LearnedClauseSortingStrategy::Lbd,
            _ => panic!("Unknown parameter given for the learned clause strategy: {}. See parameters for more details.", param)
        }
    }
}

#[derive(Default)]
pub enum LearnedClauseSortingStrategy {
    Activity,
    #[default]
    Lbd,
}
