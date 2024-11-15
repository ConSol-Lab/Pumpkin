use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::HashMap;
use crate::basic_types::HashSet;
use crate::conflict_resolution::ConflictAnalysisContext;
use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Debug, Clone, Default)]
pub(crate) struct RecursiveMinimiser {
    // data structures used for clause minimisation after conflict analysis
    current_depth: usize,
    allowed_decision_levels: HashSet<usize>, // could consider direct hashing here
    label_assignments: HashMap<Predicate, Option<Label>>,
}

impl RecursiveMinimiser {
    /// Removes redundant literals from the learned clause.
    /// Redundancy is detected by looking at the implication graph:
    /// * a literal is redundant/dominated if a subset of the other literals in the learned clause
    ///   imply that literal.
    ///
    /// The function assumes that the learned clause is stored internally
    /// in `analysis_result`, and that the first literal is
    /// asserting. The asserting literal cannot be removed.
    ///
    /// The implementation is based on the algorithm from the papers:
    ///
    /// \[1\] A. Van Gelder, ‘Improved conflict-clause minimization leads
    /// to improved propositional proof traces’. SAT'09.
    ///
    /// \[2\] N. Sörensson and A. Biere, ‘Minimizing learned clauses’. SAT'09
    pub(crate) fn remove_dominated_predicates(
        &mut self,
        nogood: &mut Vec<Predicate>,
        context: &mut ConflictAnalysisContext,
    ) {
        let num_literals_before_minimisation = nogood.len();

        self.initialise_minimisation_data_structures(nogood, context.assignments);

        // Iterate over each predicate and check whether it is a dominated predicate.
        let mut end_position: usize = 0;
        let initial_nogood_size = nogood.len();
        for i in 0..initial_nogood_size {
            let learned_predicate = nogood[i];

            self.compute_label(learned_predicate, context);

            let label = self.get_predicate_label(learned_predicate);
            // Keep the predicate in case it was not deemed deemed redundant.
            // Note that in other cases, since 'end_position' is not incremented,
            // the predicate is effectively removed.
            if label == Label::Poison || label == Label::Keep {
                nogood[end_position] = learned_predicate;
                end_position += 1;
            }
        }

        nogood.truncate(end_position);

        self.clean_up_minimisation();

        let num_predicates_removed = num_literals_before_minimisation - nogood.len();
        context
            .counters
            .learned_clause_statistics
            .average_number_of_removed_literals_recursive
            .add_term(num_predicates_removed as u64);
    }

    fn compute_label(&mut self, input_predicate: Predicate, context: &mut ConflictAnalysisContext) {
        pumpkin_assert_moderate!(context.assignments.is_predicate_satisfied(input_predicate));

        self.current_depth += 1;

        if self.is_predicate_label_already_computed(input_predicate) {
            self.current_depth -= 1;
            return;
        }

        // Stop analysis when too deep in the recursion.
        if self.is_at_max_allowed_depth() {
            self.assign_predicate_label(input_predicate, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        // At this point the predicate is either SEEN ('present') or unlabelled.
        // If the predicate is a decision predicate, it cannot be a predicate from the original
        // learned nogood since those are labelled as part of initialisation.
        // Therefore the decision literal is labelled as poison and then return.
        if context.assignments.is_decision_predicate(&input_predicate) {
            self.assign_predicate_label(input_predicate, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        // A predicate that is not part of the allowed decision levels
        // (levels from the original learned clause) cannot be removed.
        if !self.is_decision_level_allowed(
            context
                .assignments
                .get_decision_level_for_predicate(&input_predicate)
                .unwrap(),
        ) {
            self.assign_predicate_label(input_predicate, Label::Poison);
            self.current_depth -= 1;
            return;
        }

        // Due to ownership rules, we retrieve the reason each time we need it, and then drop it.
        // Here we retrieve the reason and just record the length, dropping the ownership of the
        // reason.
        let reason_size = ConflictAnalysisContext::get_propagation_reason(
            input_predicate,
            context.assignments,
            context.reason_store,
            context.propagators,
            context.proof_log,
            context.unit_nogood_step_ids,
        )
        .len();

        for i in 0..reason_size {
            let antecedent_predicate = ConflictAnalysisContext::get_propagation_reason(
                input_predicate,
                context.assignments,
                context.reason_store,
                context.propagators,
                context.proof_log,
                context.unit_nogood_step_ids,
            )[i];

            // Root assignments can be safely ignored.
            if context
                .assignments
                .get_decision_level_for_predicate(&antecedent_predicate)
                .unwrap()
                == 0
            {
                continue;
            }

            // Compute the label of the antecedent predicate.
            self.compute_label(antecedent_predicate, context);

            // In case one of the antecedents is Poison,
            // the input predicate is not deemed redundant.
            if self.get_predicate_label(antecedent_predicate) == Label::Poison {
                // Determine whether the input predicate will be labelled as Keep or Poison.

                // If the input predicate is part of the original learned nogood,
                // the predicate is Keep.
                if self.is_predicate_assigned_seen(input_predicate) {
                    self.assign_predicate_label(input_predicate, Label::Keep);
                    self.current_depth -= 1;
                    return;
                }
                // Otherwise, the input predicate is not part of the original nogood,
                // so it cannot be Keep but is labelled Poison instead.
                else {
                    self.assign_predicate_label(input_predicate, Label::Poison);
                    self.current_depth -= 1;
                    return;
                }
            }
        }
        // If the code reaches this part (it did not get into one of the previous 'return'
        // statements, so all antecedents of the literal are either KEEP or REMOVABLE),
        // meaning this literal is REMOVABLE.
        self.assign_predicate_label(input_predicate, Label::Removable);
        self.current_depth -= 1;
    }

    fn is_decision_level_allowed(&self, decision_level: usize) -> bool {
        self.allowed_decision_levels.contains(&decision_level)
    }

    fn mark_decision_level_as_allowed(&mut self, decision_level: usize) {
        let _ = self.allowed_decision_levels.insert(decision_level);
    }

    fn is_predicate_assigned_seen(&self, predicate: Predicate) -> bool {
        let entry = self.label_assignments.get(&predicate);
        if let Some(label) = entry {
            label.expect("Stored label is None, error?") == Label::Seen
        } else {
            false
        }
    }

    fn get_predicate_label(&self, predicate: Predicate) -> Label {
        self.label_assignments
            .get(&predicate)
            .expect("Cannot ask for a label of an unlabelled literal?")
            .expect("Stored label is None, error?")
    }

    fn assign_predicate_label(&mut self, predicate: Predicate, label: Label) {
        pumpkin_assert_moderate!(
            !self.label_assignments.contains_key(&predicate)
                || self.is_predicate_assigned_seen(predicate),
            "Cannot assign the label of an already labelled literal"
        );
        let _ = self.label_assignments.insert(predicate, Some(label));
    }

    fn is_predicate_label_already_computed(&self, predicate: Predicate) -> bool {
        let entry = self.label_assignments.get(&predicate);
        if let Some(label) = entry {
            label.expect("Stored label is None, error?") != Label::Seen
        } else {
            false
        }
    }

    fn initialise_minimisation_data_structures(
        &mut self,
        nogood: &Vec<Predicate>,
        assignments: &Assignments,
    ) {
        pumpkin_assert_simple!(self.current_depth == 0);

        // Mark literals from the initial learned nogood.
        for &predicate in nogood {
            // Predicates from the current decision level are always kept.
            // This is the analogue of asserting literals.
            if assignments
                .get_decision_level_for_predicate(&predicate)
                .unwrap()
                == assignments.get_decision_level()
            {
                let _ = self.label_assignments.insert(predicate, Some(Label::Keep));
                continue;
            }

            // Decision predicate must be kept.
            if assignments.is_decision_predicate(&predicate) {
                self.assign_predicate_label(predicate, Label::Keep);
            } else {
                self.assign_predicate_label(predicate, Label::Seen);
            }

            self.mark_decision_level_as_allowed(
                assignments
                    .get_decision_level_for_predicate(&predicate)
                    .unwrap(),
            );
        }
    }

    fn clean_up_minimisation(&mut self) {
        pumpkin_assert_simple!(self.current_depth == 0);

        self.allowed_decision_levels.clear();
        self.label_assignments.clear();
    }

    fn is_at_max_allowed_depth(&self) -> bool {
        pumpkin_assert_moderate!(self.current_depth <= 500);
        self.current_depth == 500
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum Label {
    Seen, //'Present'
    Poison,
    Removable,
    Keep,
}
