use std::collections::{HashMap, HashSet};

use sat_cp_mediator::SATCPMediator;

use crate::{
    basic_types::Literal,
    engine::{
        clause_allocators::{ClauseAllocatorInterface, ClauseInterface},
        constraint_satisfaction_solver::{ClausalPropagator, ConflictAnalysisResult},
        sat_cp_mediator, CPEngineDataStructures, ConstraintProgrammingPropagator,
    },
    pumpkin_assert_moderate, pumpkin_assert_simple,
};

use super::{AssignmentsPropositional, SATEngineDataStructures};

#[derive(Default)]
pub struct LearnedClauseMinimiser {
    current_depth: usize,
    allowed_decision_levels: HashSet<u32>, //could consider direct hashing here
    label_assignments: HashMap<Literal, Option<Label>>,
    num_minimisation_calls: usize,
    num_literals_removed_total: usize,
    num_literals_seen_total: usize,
}

impl LearnedClauseMinimiser {
    //assumes the first literal is the asserting literal
    //removes literals that are dominated in the implication graph from the learned clause
    //  A literal is dominated if a subset of the other literals in the learned clause imply that literal, making the dominated literal redundant
    //note that the asserting literal cannot be removed
    //the implementation is based on the algorithm from the papers:
    //  "Improved conflict-clause minimization leads to improved propositional proof traces.", Allen Van Gelder. SAT'09
    //  "Minimizing learned clauses", Niklas Sörensson and Armin Biere, SAT'09
    pub fn remove_dominated_literals(
        &mut self,
        analysis_result: &mut ConflictAnalysisResult,
        clausal_propagator: &ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
        sat_cp_mediator: &mut SATCPMediator,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) {
        self.num_minimisation_calls += 1;
        self.num_literals_seen_total += analysis_result.learned_literals.len();
        let num_literals_before_minimisation = analysis_result.learned_literals.len();

        self.initialise(
            analysis_result,
            &sat_data_structures.assignments_propositional,
        );

        //iterate over each literal and check whether it is a dominated literal
        let mut end_position: usize = 1; // the propagated literals must stay, so we skip it
        for i in 1..analysis_result.learned_literals.len() {
            let learned_literal = analysis_result.learned_literals[i];

            self.compute_label(
                !learned_literal,
                clausal_propagator,
                sat_data_structures,
                cp_data_structures,
                sat_cp_mediator,
                cp_propagators,
            );

            let label = self.get_literal_label(!learned_literal);
            //keep the literal in case it was not deemed deemed redundant
            //  note that in other cases, since 'end_position' is not incremented, the literal is effectively removed
            if label == Label::Poison || label == Label::Keep {
                analysis_result.learned_literals[end_position] = learned_literal;
                end_position += 1;
                //ensure that the literal at position 1 is at the highest level
                //  this is an important invariant for the conflict analysis result
                let literal_at_index_1 = analysis_result.learned_literals[1];
                if sat_data_structures
                    .assignments_propositional
                    .get_literal_assignment_level(literal_at_index_1)
                    < sat_data_structures
                        .assignments_propositional
                        .get_literal_assignment_level(learned_literal)
                {
                    analysis_result.learned_literals.swap(1, end_position - 1); //notice the minus one, since we already incremented end_position above
                }
            }
        }
        analysis_result.learned_literals.truncate(end_position);

        self.clean_up();

        let num_literals_removed =
            num_literals_before_minimisation - analysis_result.learned_literals.len();
        self.num_literals_removed_total += num_literals_removed;
    }

    fn compute_label(
        &mut self,
        input_literal: Literal,
        clausal_propagator: &ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
        cp_data_structures: &mut CPEngineDataStructures,
        sat_cp_mediator: &mut SATCPMediator,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) {
        pumpkin_assert_moderate!(sat_data_structures
            .assignments_propositional
            .is_literal_assigned_true(input_literal));

        self.current_depth += 1;

        if self.is_literal_label_already_computed(input_literal) {
            self.current_depth -= 1;
            return;
        }

        //for performance reasons we stop the analysis if we need to many recursive calls
        if self.is_at_max_allowed_depth() {
            self.assign_literal_label(input_literal, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        //at this point the literal is either SEEN ('present') or unlabelled
        //if the literal is a decision literal, it cannot be a literal from the original learned clause since those are labelled as part of initialisation
        //therefore the decision literal is labelled as poison and then return
        if sat_data_structures
            .assignments_propositional
            .is_literal_decision(input_literal)
        {
            self.assign_literal_label(input_literal, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        //a literal that is not part of the allowed decision levels (levels from the original learned clause) cannot be removed
        if !self.is_decision_level_allowed(
            sat_data_structures
                .assignments_propositional
                .get_literal_assignment_level(input_literal),
        ) {
            self.assign_literal_label(input_literal, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        let reason_reference = sat_cp_mediator.get_propagation_clause_reference(
            input_literal,
            clausal_propagator,
            sat_data_structures,
            cp_data_structures,
            cp_propagators,
        );

        for i in 1..sat_data_structures
            .clause_allocator
            .get_clause(reason_reference)
            .len()
        {
            let antecedent_literal = !sat_data_structures
                .clause_allocator
                .get_clause(reason_reference)[i];

            //root assignments can be safely ignored
            if sat_data_structures
                .assignments_propositional
                .is_literal_root_assignment(antecedent_literal)
            {
                continue;
            }

            //compute the label of the antecedent literal
            self.compute_label(
                antecedent_literal,
                clausal_propagator,
                sat_data_structures,
                cp_data_structures,
                sat_cp_mediator,
                cp_propagators,
            );

            //in case one of the antecedents is Poison, the input literal is not deemed redundant
            if self.get_literal_label(antecedent_literal) == Label::Poison {
                //now it needs to be determined whether the input literal will be labelled as Keep or Poison

                //if the input literal is part of the original learned clause, the literal is Keep
                if self.is_literal_assigned_seen(input_literal) {
                    self.assign_literal_label(input_literal, Label::Keep);
                    self.current_depth -= 1;
                    return;
                }
                //otherwise, the input literal is not part of the original learned clause
                //  so it cannot be Keep but is labelled Poison instead
                else {
                    self.assign_literal_label(input_literal, Label::Poison);
                    self.current_depth -= 1;
                    return;
                }
            }
        }
        //if the code reaches this part, i.e., it did not get into one of the previous 'return' statements
        //  all antecedents of the literal are either KEEP or REMOVABLE, meaning this literal is REMOVABLE
        self.assign_literal_label(input_literal, Label::Removable);
        self.current_depth -= 1;
    }

    fn is_decision_level_allowed(&self, decision_level: u32) -> bool {
        self.allowed_decision_levels.contains(&decision_level)
    }

    fn mark_decision_level_as_allowed(&mut self, decision_level: u32) {
        self.allowed_decision_levels.insert(decision_level);
    }

    fn is_literal_assigned_seen(&self, literal: Literal) -> bool {
        let entry = self.label_assignments.get(&literal);
        if let Some(label) = entry {
            label.expect("Stored label is None, error?") == Label::Seen
        } else {
            false
        }
    }

    fn get_literal_label(&self, literal: Literal) -> Label {
        self.label_assignments
            .get(&literal)
            .expect("Cannot ask for a label of an unlabelled literal?")
            .expect("Stored label is None, error?")
    }

    fn assign_literal_label(&mut self, literal: Literal, label: Label) {
        pumpkin_assert_moderate!(
            !self.label_assignments.contains_key(&literal)
                || self.is_literal_assigned_seen(literal),
            "Cannot assign the label of an already labelled literal"
        );
        self.label_assignments.insert(literal, Some(label));
    }

    fn is_literal_label_already_computed(&self, literal: Literal) -> bool {
        let entry = self.label_assignments.get(&literal);
        if let Some(label) = entry {
            label.expect("Stored label is None, error?") != Label::Seen
        } else {
            false
        }
    }

    fn initialise(
        &mut self,
        analysis_result: &ConflictAnalysisResult,
        assignments: &AssignmentsPropositional,
    ) {
        pumpkin_assert_simple!(self.current_depth == 0);

        //mark literals from the initial learned clause
        //   the asserting literal is always kept
        self.label_assignments
            .insert(analysis_result.learned_literals[0], Some(Label::Keep));
        //  go through the other literals
        for i in 1..analysis_result.learned_literals.len() {
            let literal = !analysis_result.learned_literals[i];
            //decision literals must be kept
            if assignments.is_literal_decision(literal) {
                self.assign_literal_label(literal, Label::Keep);
            } else {
                self.assign_literal_label(literal, Label::Seen);
            }

            self.mark_decision_level_as_allowed(assignments.get_literal_assignment_level(literal));
        }
    }

    fn clean_up(&mut self) {
        pumpkin_assert_simple!(self.current_depth == 0);

        self.allowed_decision_levels.clear();
        self.label_assignments.clear();
    }

    fn is_at_max_allowed_depth(&self) -> bool {
        pumpkin_assert_moderate!(self.current_depth <= 500);
        self.current_depth == 500
    }
}

impl LearnedClauseMinimiser {
    pub fn num_literals_removed_total(&self) -> usize {
        self.num_literals_removed_total
    }

    pub fn percentage_num_removed_literals_per_clause(&self) -> f64 {
        if self.num_literals_seen_total > 0 {
            1.0_f64 - (self.num_literals_removed_total as f64 / self.num_literals_seen_total as f64)
        } else {
            0.0
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
enum Label {
    Seen, //'Present'
    Poison,
    Removable,
    Keep,
}
