use super::explanation_clause_manager::ExplanationClauseManager;
use super::ConflictAnalysisContext;
use super::ConflictAnalysisResult;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::HashMap;
use crate::basic_types::HashSet;
use crate::engine::AssignmentsPropositional;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::Literal;

/// A minimiser which removes redundant literals from the learned clause.
///
/// Redundancy is detected by looking at the implication graph:
/// * A literal is **redundant/dominated** if a subset of the other literals in the learned clause
///   imply that literal.
///
/// The implementation is based on the algorithm from the papers:
///
/// \[1\] A. Van Gelder, ‘Improved conflict-clause minimization leads
/// to improved propositional proof traces’. SAT'09.
///
/// \[2\] N. Sörensson and A. Biere, ‘Minimizing learned clauses’. SAT'09
#[derive(Debug, Default)]
pub(crate) struct RecursiveMinimiser {
    /// Indicates what [`Label`] the current [`Literal`] is assigned to
    label_assignments: HashMap<Literal, Option<Label>>,
    /// The current depth of the minimization
    current_depth: usize,
    /// The decision levels from which [`Literal`]s can be removed during minimisation
    allowed_decision_levels: HashSet<usize>,
}

/// The maximum number of recursive calls which can be made
const MAX_DEPTH: usize = 500;

/// The possible labels of a [`Literal`]
#[derive(PartialEq, Copy, Clone, Debug)]
enum Label {
    /// The [`Literal`] has been seen at some point during the minimisation
    Seen, //'Present'
    /// The state of the [`Literal`] is unknown (potentially during the depth limit); it is thus
    /// kept
    Poison,
    /// The [`Literal`] can be removed
    Removable,
    /// The [`Literal`] should be kept
    Keep,
}

impl RecursiveMinimiser {
    /// Removes redundant literals from the learned clause.
    /// Redundancy is detected by looking at the implication graph:
    /// * A literal is **redundant/dominated** if a subset of the other literals in the learned
    ///   clause imply that literal.
    ///
    /// The function assumes that the learned clause is stored
    /// in `analysis_result`, and that the first literal is
    /// asserting. The asserting literal cannot be removed.
    ///
    /// The implementation is based on the algorithm from the papers:
    ///
    /// \[1\] A. Van Gelder, ‘Improved conflict-clause minimization leads
    /// to improved propositional proof traces’. SAT'09.
    ///
    /// \[2\] N. Sörensson and A. Biere, ‘Minimizing learned clauses’. SAT'09
    pub(crate) fn remove_dominated_literals(
        &mut self,
        context: &mut ConflictAnalysisContext,
        explanation_clause_manager: &mut ExplanationClauseManager,
        analysis_result: &mut ConflictAnalysisResult,
    ) {
        let num_literals_before_minimisation = analysis_result.learned_literals.len();

        self.initialise_minimisation_data_structures(
            context.assignments_propositional,
            analysis_result,
        );

        // iterate over each literal and check whether it is a dominated literal
        let mut end_position: usize = 1; // the propagated literals must stay, so we skip it
        for i in 1..analysis_result.learned_literals.len() {
            let learned_literal = analysis_result.learned_literals[i];

            self.compute_label(!learned_literal, context, explanation_clause_manager);

            let label = self.get_literal_label(!learned_literal);
            // keep the literal in case it was not deemed deemed redundant
            //  note that in other cases, since 'end_position' is not incremented, the literal is
            // effectively removed
            if label == Label::Poison || label == Label::Keep {
                analysis_result.learned_literals[end_position] = learned_literal;
                end_position += 1;
                // ensure that the literal at position 1 is at the highest level
                //  this is an important invariant for the conflict analysis result
                let literal_at_index_1 = analysis_result.learned_literals[1];
                if context
                    .assignments_propositional
                    .get_literal_assignment_level(literal_at_index_1)
                    < context
                        .assignments_propositional
                        .get_literal_assignment_level(learned_literal)
                {
                    // Notice the minus one, since we already incremented `end_position` above
                    analysis_result.learned_literals.swap(1, end_position - 1);
                }
            }
        }
        if analysis_result.learned_literals.len() > 1 {
            analysis_result.backjump_level = context
                .assignments_propositional
                .get_literal_assignment_level(analysis_result.learned_literals[1]);
        }

        analysis_result.learned_literals.truncate(end_position);

        self.clean_up();

        let num_literals_removed =
            num_literals_before_minimisation - analysis_result.learned_literals.len();
        context
            .counters
            .learned_clause_statistics
            .average_number_of_removed_literals_recursive
            .add_term(num_literals_removed as u64);
    }

    fn initialise_minimisation_data_structures(
        &mut self,
        assignments: &AssignmentsPropositional,
        analysis_result: &mut ConflictAnalysisResult,
    ) {
        pumpkin_assert_simple!(self.current_depth == 0);

        // mark literals from the initial learned clause
        //   the asserting literal is always kept
        let _ = self
            .label_assignments
            .insert(analysis_result.learned_literals[0], Some(Label::Keep));
        //  go through the other literals
        for i in 1..analysis_result.learned_literals.len() {
            let literal = !analysis_result.learned_literals[i];
            // decision literals must be kept
            if assignments.is_literal_decision(literal) {
                self.assign_literal_label(literal, Label::Keep);
            } else {
                self.assign_literal_label(literal, Label::Seen);
            }

            self.mark_decision_level_as_allowed(assignments.get_literal_assignment_level(literal));
        }
    }

    fn compute_label(
        &mut self,
        input_literal: Literal,
        context: &mut ConflictAnalysisContext,
        explanation_clause_manager: &mut ExplanationClauseManager,
    ) {
        pumpkin_assert_moderate!(context
            .assignments_propositional
            .is_literal_assigned_true(input_literal));

        self.current_depth += 1;

        if self.is_literal_label_already_computed(input_literal) {
            self.current_depth -= 1;
            return;
        }

        // for performance reasons we stop the analysis if we need to many recursive calls
        if self.is_at_max_allowed_depth() {
            self.assign_literal_label(input_literal, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        // at this point the literal is either SEEN ('present') or unlabelled
        // if the literal is a decision literal, it cannot be a literal from the original learned
        // clause since those are labelled as part of initialisation therefore the decision
        // literal is labelled as poison and then return
        if context
            .assignments_propositional
            .is_literal_decision(input_literal)
        {
            self.assign_literal_label(input_literal, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        // a literal that is not part of the allowed decision levels (levels from the original
        // learned clause) cannot be removed
        if !self.is_decision_level_allowed(
            context
                .assignments_propositional
                .get_literal_assignment_level(input_literal),
        ) {
            self.assign_literal_label(input_literal, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        let reason_reference = explanation_clause_manager.get_propagation_clause_reference(
            context,
            input_literal,
            &mut |_| {},
        );

        for i in 1..explanation_clause_manager
            .get_clause(context.clause_allocator, &reason_reference)
            .len()
        {
            let antecedent_literal = !explanation_clause_manager
                .get_clause(context.clause_allocator, &reason_reference)[i];

            // root assignments can be safely ignored
            if context
                .assignments_propositional
                .is_literal_root_assignment(antecedent_literal)
            {
                continue;
            }

            // compute the label of the antecedent literal
            self.compute_label(antecedent_literal, context, explanation_clause_manager);

            // in case one of the antecedents is Poison, the input literal is not deemed redundant
            if self.get_literal_label(antecedent_literal) == Label::Poison {
                // now it needs to be determined whether the input literal will be labelled as Keep
                // or Poison

                // if the input literal is part of the original learned clause, the literal is Keep
                if self.is_literal_assigned_seen(input_literal) {
                    self.assign_literal_label(input_literal, Label::Keep);
                    self.current_depth -= 1;
                    return;
                }
                // otherwise, the input literal is not part of the original learned clause
                //  so it cannot be Keep but is labelled Poison instead
                else {
                    self.assign_literal_label(input_literal, Label::Poison);
                    self.current_depth -= 1;
                    return;
                }
            }
        }
        // if the code reaches this part, i.e., it did not get into one of the previous 'return'
        // statements  all antecedents of the literal are either KEEP or REMOVABLE, meaning
        // this literal is REMOVABLE
        self.assign_literal_label(input_literal, Label::Removable);
        self.current_depth -= 1;
    }

    fn is_decision_level_allowed(&self, decision_level: usize) -> bool {
        self.allowed_decision_levels.contains(&decision_level)
    }

    fn mark_decision_level_as_allowed(&mut self, decision_level: usize) {
        let _ = self.allowed_decision_levels.insert(decision_level);
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
        let _ = self.label_assignments.insert(literal, Some(label));
    }

    fn is_literal_label_already_computed(&self, literal: Literal) -> bool {
        let entry = self.label_assignments.get(&literal);
        if let Some(label) = entry {
            label.expect("Stored label is None, error?") != Label::Seen
        } else {
            false
        }
    }

    fn clean_up(&mut self) {
        pumpkin_assert_simple!(self.current_depth == 0);

        self.allowed_decision_levels.clear();
        self.label_assignments.clear();
    }

    fn is_at_max_allowed_depth(&self) -> bool {
        pumpkin_assert_moderate!(self.current_depth <= MAX_DEPTH);
        self.current_depth == MAX_DEPTH
    }
}
