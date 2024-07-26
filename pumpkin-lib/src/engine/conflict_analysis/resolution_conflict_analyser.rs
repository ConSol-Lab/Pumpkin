use super::ConflictAnalysisNogoodContext;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::HashMap;
use crate::basic_types::HashSet;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::branching::Brancher;
use crate::containers::KeyValueHeap;
use crate::containers::StorageKey;
use crate::engine::conflict_analysis::semantic_minimiser::Mode;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::Assignments;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Clone, Debug, Default)]
pub(crate) struct ResolutionNogoodConflictAnalyser {
    /// Heap containing the current decision level predicates.
    /// It sorts the predicates based on trail position.
    heap_current_decision_level: KeyValueHeap<PredicateId, u32>,
    /// The generator is used in combination with the heap.
    predicate_id_generator: PredicateIdGenerator,
    /// Vector containing predicates above the current decision level.
    /// The vector may contain duplicates, but will be removed at the end.
    predicates_lower_decision_level: Vec<Predicate>,

    // data structures used for clause minimisation after conflict analysis
    current_depth: usize,
    allowed_decision_levels: HashSet<usize>, // could consider direct hashing here
    label_assignments: HashMap<Predicate, Option<Label>>,
    num_minimisation_calls: usize,
    num_predicates_removed_total: usize,
    num_literals_seen_total: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct LearnedNogood {
    pub(crate) predicates: Vec<Predicate>,
    pub(crate) backjump_level: usize,
}

impl ResolutionNogoodConflictAnalyser {
    /// Clears all data structures to prepare for the new conflict analysis.
    fn clean_up(&mut self) {
        self.predicates_lower_decision_level.clear();
        self.predicate_id_generator.clear();
        self.heap_current_decision_level.clear();
        // println!("start...........");
    }

    fn add_predicate_to_conflict_nogood(
        &mut self,
        predicate: Predicate,
        assignments: &Assignments,
        brancher: &mut dyn Brancher,
    ) {
        let dec_level = assignments
            .get_decision_level_for_predicate(&predicate)
            .unwrap();

        // Ignore root level predicates.
        if dec_level == 0 {
            // do nothing
        }
        // We distinguish between predicates from the current decision level and other predicates.
        else if dec_level == assignments.get_decision_level() {
            let predicate_id = self.predicate_id_generator.get_id(predicate);
            // The first time we encounter the predicate, we initialise its value in the heap.
            // Note that if the predicate is already in the heap, no action needs to be taken. It
            // can happen that a predicate is returned multiple times as a reason for other
            // predicates.

            // todo: could improve the heap structure to be more user-friendly.
            // Here we manually adjust the size of the heap to accommodate new elements.
            while self.heap_current_decision_level.len() <= predicate_id.index() {
                let next_id = PredicateId {
                    id: self.heap_current_decision_level.len() as u32,
                };
                self.heap_current_decision_level.grow(next_id, 0);
                self.heap_current_decision_level.delete_key(next_id);
            }

            if !self
                .heap_current_decision_level
                .is_key_present(predicate_id)
                && *self.heap_current_decision_level.get_value(predicate_id) == 0
            {
                brancher.on_appearance_in_conflict_predicate(predicate);

                let trail_position = assignments.get_trail_position(&predicate).unwrap();

                // The goal is to traverse predicate in reverse order of the trail.
                // However some predicates may share the trail position. For example, if a predicate
                // that was posted to trail resulted in some other predicates being true, then all
                // these predicates would have the same trail position. When considering the
                // predicates in reverse order of the trail, the implicitly set predicates are
                // posted after the explicitly set one, but they all have the same trail position.
                // To remedy this, we make a tie-breaking scheme to prioritise implied predicates
                // over explicit predicates. This is done by assigning explicitly set predicates the
                // value 2*trail_position, whereas implied predicates get 2*trail_position + 1.
                let heap_value = if assignments.trail[trail_position].predicate == predicate {
                    trail_position * 2
                } else {
                    trail_position * 2 + 1
                };

                self.heap_current_decision_level.restore_key(predicate_id);
                self.heap_current_decision_level
                    .increment(predicate_id, heap_value as u32);

                // todo: I think this is not needed, but double check.
                if *self.heap_current_decision_level.get_value(predicate_id)
                    != heap_value.try_into().unwrap()
                {
                    self.heap_current_decision_level.delete_key(predicate_id);
                }

                // The way we compute reasons, this could happen actually.
                // pumpkin_assert_moderate!(
                // self.heap_current_decision_level.get_value(predicate_id)
                // == trail_position as u32,
                // "Can only add the predicate to the heap once."
                // );
            }
        } else {
            // We do not check for duplicate, we simply add the predicate.
            // Semantic minimisation will later remove duplicates and do other processing.
            self.predicates_lower_decision_level.push(predicate);
        }
    }

    fn pop_predicate_from_conflict_nogood(&mut self) -> Predicate {
        let next_predicate_id = self.heap_current_decision_level.pop_max().unwrap();
        self.predicate_id_generator
            .get_predicate(next_predicate_id)
            .unwrap()
    }

    fn extract_final_nogood(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
    ) -> LearnedNogood {
        // The final nogood is composed of the predicates encountered from the lower decision
        // levels, plus the predicate remaining in the heap.

        // First obtain a semantically minimised nogood.
        // We reuse the vector with lower decision levels for simplicity.
        let last_predicate = self.pop_predicate_from_conflict_nogood();
        self.predicates_lower_decision_level.push(last_predicate);

        let mut clean_nogood: Vec<Predicate> = context.semantic_minimiser.minimise(
            &self.predicates_lower_decision_level,
            context.assignments,
            Mode::DisableEqualityMerging,
        );

        self.remove_dominated_predicates(&mut clean_nogood, context);

        clean_nogood = context.semantic_minimiser.minimise(
            &clean_nogood,
            context.assignments,
            Mode::EnableEqualityMerging,
        );

        // Sorting does the trick with placing the correct predicates at the first two positions,
        // however this can be done more efficiently, since we only need the first two positions
        // to be properly sorted.
        clean_nogood.sort_by_key(|p| context.assignments.get_trail_position(p).unwrap());
        clean_nogood.reverse();
        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if clean_nogood.len() > 1 {
            context
                .assignments
                .get_decision_level_for_predicate(&clean_nogood[1])
                .unwrap()
        }
        // For unit nogoods, the solver backtracks to the root level.
        else {
            0
        };

        pumpkin_assert_advanced!(clean_nogood[1..]
            .iter()
            .all(|p| context.assignments.is_predicate_satisfied(*p)));

        // todo: asserting predicate may be bumped twice, probably not a problem.
        for predicate in clean_nogood.iter() {
            context
                .brancher
                .on_appearance_in_conflict_predicate(*predicate);
        }

        LearnedNogood {
            backjump_level,
            predicates: clean_nogood,
        }
    }

    /// Computes the learned nogood according to the 1UIP scheme.
    /// The asserting predicate is at position zero, and the second decision level positioned
    /// predicate is at position one.
    pub(crate) fn compute_1uip(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
    ) -> LearnedNogood {
        self.clean_up();

        // Initialise the data structures with the conflict nogood.
        for predicate in context.get_conflict_nogood().iter() {
            self.add_predicate_to_conflict_nogood(
                *predicate,
                context.assignments,
                context.brancher,
            );
        }
        // Record conflict nogood size statistics.
        let num_initial_conflict_predicates =
            self.heap_current_decision_level.num_nonremoved_elements()
                + self.predicates_lower_decision_level.len();
        context
            .counters
            .average_conflict_size
            .add_term(num_initial_conflict_predicates as u64);

        // println!("lower dec lvls: {:?}", self.predicates_lower_decision_level);
        //
        // println!("VARIABLE DOMAINS");
        // for d in context.assignments.get_domains() {
        // println!(
        // "{}: [{}, {}]",
        // d,
        // context.assignments.get_lower_bound(d),
        // context.assignments.get_upper_bound(d)
        // );
        // }
        //
        // println!("curr dec lvl: {}", context.assignments.get_decision_level());
        // println!("TRAIL");
        // for t in context.assignments.trail.iter() {
        // println!("\t{} {}", t.predicate, t.reason.is_none());
        // }

        // Keep refining the conflict nogood until there is only one predicate from the current
        // decision level. There is an exception special case: when posting the decision [x = v], it
        // gets decomposed into two decisions ([x >= v] & [x <= v]). In this case there will be two
        // predicates left from the current decision level, and both will be decisions. This
        // is accounted for below.
        while self.heap_current_decision_level.num_nonremoved_elements() > 1 {
            // Replace the predicate from the nogood that has been assigned last on the trail.
            // This is done in two steps:
            // 1) Pop the predicate last assigned on the trail from the nogood.

            let next_predicate = self.pop_predicate_from_conflict_nogood();

            // println!("Next pred: {}", next_predicate);
            // 2) Add the reason of the next_predicate to the nogood.

            // 2.a) Here we treat the special case: if the next predicate is a decision, this means
            // that we are done with analysis since the only remaining predicate in the heap is the
            // other decision.
            if context.assignments.is_decision_predicate(&next_predicate) {
                // if self.heap_current_decision_level.num_nonremoved_elements() != 1 &&
                // self.heap_current_decision_level.num_nonremoved_elements() != 2 {
                // println!("NUM ELEMETNS {}",
                // self.heap_current_decision_level.num_nonremoved_elements());
                // while self.heap_current_decision_level.num_nonremoved_elements() != 0
                // {
                // let poppers = self.heap_current_decision_level.pop_max();
                // println!("\tpopped {}",
                // self.predicate_id_generator.get_predicate(poppers.unwrap()).unwrap());
                // }
                // panic!();
                // }

                // assert!(self.heap_current_decision_level.num_nonremoved_elements() == 1 ||
                // self.heap_current_decision_level.num_nonremoved_elements() == 2);
                // As a simple workaround, we add the currently analysed predicate to set of
                // predicates from the lower predicate level, and stop analysis. Semantic
                // minimisation will ensure the bound predicates get converted into an equality
                // decision predicate.
                while self.heap_current_decision_level.num_nonremoved_elements() != 1 {
                    let p = self.pop_predicate_from_conflict_nogood();
                    self.predicates_lower_decision_level.push(p);
                }

                self.predicates_lower_decision_level.push(next_predicate);
                break;
            }
            // 2.b) Standard case, get the reason for the predicate and add it to the nogood.
            let reason = ConflictAnalysisNogoodContext::get_propagation_reason_simple(
                next_predicate,
                context.assignments,
                context.reason_store,
                context.propagators,
            );

            // println!("reason: {:?}", reason);
            for predicate in reason.iter() {
                self.add_predicate_to_conflict_nogood(
                    *predicate,
                    context.assignments,
                    context.brancher,
                );
            }
        }
        self.extract_final_nogood(context)
    }
}

// clause minimisation
impl ResolutionNogoodConflictAnalyser {
    /// Removes redundant literals from the learned clause.
    /// Redundancy is detected by looking at the implication graph:
    /// * a literal is redundant/dominated if a subset of the other
    /// literals in the learned clause imply that literal.
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
        context: &mut ConflictAnalysisNogoodContext,
    ) {
        self.num_minimisation_calls += 1;
        self.num_literals_seen_total += nogood.len();
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
        self.num_predicates_removed_total += num_predicates_removed;
    }

    fn compute_label(
        &mut self,
        input_predicate: Predicate,
        context: &mut ConflictAnalysisNogoodContext,
    ) {
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
        // reason. Todo: this may cause undesired (?) bumps for lazy explanations.
        let reason_size = ConflictAnalysisNogoodContext::get_propagation_reason_simple(
            input_predicate,
            context.assignments,
            context.reason_store,
            context.propagators,
        )
        .len();

        for i in 0..reason_size {
            let antecedent_predicate = ConflictAnalysisNogoodContext::get_propagation_reason_simple(
                input_predicate,
                context.assignments,
                context.reason_store,
                context.propagators,
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

    #[allow(dead_code)]
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
                let _ = self.label_assignments.insert(nogood[0], Some(Label::Keep));
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

    #[allow(dead_code)]
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
