use crate::basic_types::KeyValueHeap;
use crate::basic_types::KeyedVec;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::SolutionReference;
use crate::basic_types::StorageKey;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
use crate::engine::AssignmentsInteger;

/// A [`Brancher`] that combines [VSIDS \[1\]](https://dl.acm.org/doi/pdf/10.1145/378239.379017)
/// and \[2\]](https://people.eng.unimelb.edu.au/pstuckey/papers/lns-restarts.pdf) [`ValueSelector`].
/// There are two components: 1) predicate selection, and 2) truth value assignment.
///
/// Predicate selection: The VSIDS algorithm is an adaptation for the CP case. It determines which
/// predicate should be branched on based on how often it appears in conflicts. Intuitively, the
/// more often a predicate appears in *recent* conflicts, the more "important" it is during the
/// search process. VSIDS is originally from the SAT field (see \[1\]) but we adapted it for
/// constraint programming by considering predicates from recent conflicts rather than Boolean
/// variables.
///
/// Truth value assignment: The truth value for the predicate is selected to be consistent with the
/// best solution known so far. In this way, the search is directed around this existing solution.
/// In case where there is no known solution, then the predicate is assigned to true. This resembles
/// a fail-first strategy with the idea that the given predicate was encountered in conflicts, so
/// assigning it to true may cause another conflict soon.
///
/// # Bibliography
/// \[1\] M. W. Moskewicz, C. F. Madigan, Y. Zhao, L. Zhang, and S. Malik, ‘Chaff: Engineering an
/// efficient SAT solver’, in Proceedings of the 38th annual Design Automation Conference, 2001.
/// \[2\] E. Demirović, G. Chu, and P. J. Stuckey, ‘Solution-based phase saving for CP: A
/// value-selection heuristic to simulate local search behavior in complete solvers’, in the
/// proceedings of the Principles and Practice of Constraint Programming (CP 2018).
#[derive(Debug)]
pub struct AutonomousSearch {
    /// Predicates are mapped to ids. This is used internally in the heap.
    predicate_id_info: PredicateIdGenerator,
    /// Stores the activities for a predicate, represented with its id.
    heap: KeyValueHeap<PredicateId, f64>,
    /// After popping predicates off the heap that current have a truth value, the predicates are
    /// labelled as dormant because they do not contribute to VSIDS at the moment. When
    /// backtracking, dormant predicates are examined and readded to the heap. Dormant predicates
    /// with low activities are removed.
    dormant_predicates: Vec<IntegerPredicate>,
    /// How much the activity of a predicate is increased when it appears in a conflict.
    /// This value changes during search (see [`Vsids::decay_activities`]).
    increment: f64,
    /// The maximum allowed [`Vsids`] value, if this value is reached then all of the values are
    /// divided by this value. The increment is constant.
    max_threshold: f64,
    /// Whenever a conflict is found, the [`Vsids::increment`] is multiplied by
    /// 1 / [`Vsids::decay_factor`] (this is synonymous with increasing the
    /// [`Vsids::increment`] since 0 <= [`Vsids::decay_factor`] <= 1).
    /// The decay factor is constant.
    decay_factor: f64,

    best_known_solution: KeyedVec<DomainId, i32>,
}

const DEFAULT_VSIDS_INCREMENT: f64 = 1.0;
const DEFAULT_VSIDS_MAX_THRESHOLD: f64 = 1e100;
const DEFAULT_VSIDS_DECAY_FACTOR: f64 = 0.95;
// const DEFAULT_VSIDS_VALUE: f64 = 0.0;

impl Default for AutonomousSearch {
    /// Creates a new instance of the [`Vsids`] [`VariableSelector`] with default values for
    /// the parameters (`1.0` for the increment, `1e100` for the max threshold,
    /// `0.95` for the decay factor and `0.0` for the initial VSIDS value).
    fn default() -> Self {
        AutonomousSearch {
            predicate_id_info: PredicateIdGenerator::default(),
            heap: KeyValueHeap::default(),
            dormant_predicates: vec![],
            increment: DEFAULT_VSIDS_INCREMENT,
            max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
            decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
            best_known_solution: KeyedVec::default(),
        }
    }
}

impl AutonomousSearch {
    /// Resizes the heap to accommodate for the id.
    /// Recall that the underlying heap uses direct hashing.
    fn resize_heap(&mut self, id: PredicateId) {
        while self.heap.len() <= id.index() {
            self.heap.grow(id, 0.0);
        }
    }
    /// Bumps the activity of a predicate by [`Vsids::increment`].
    /// Used when a predicate is encountered during a conflict.
    fn bump_activity(&mut self, predicate: IntegerPredicate) {
        let id = self.predicate_id_info.get_id(predicate);
        self.resize_heap(id);
        // Scale the activities if the values are too large.
        // Also remove predicates that have activities close to zero.
        let activity = self.heap.get_value(id);
        if activity + self.increment >= self.max_threshold {
            // Adjust heap values.
            self.heap.divide_values(self.max_threshold);
            // Remove inactive predicates from the heap,
            // and stage the ids for removal from the id generator.
            let mut deleted_ids = vec![];
            let small_activity = 1_f64 / self.increment;
            for id in self.predicate_id_info.iter() {
                if *self.heap.get_value(id) <= small_activity {
                    self.heap.delete_key(id);
                    deleted_ids.push(id);
                }
            }
            // Now remove the ids from the generator.
            for deleted_id in deleted_ids {
                self.predicate_id_info.delete_id(deleted_id);
            }
            // Adjust increment. It is important to adjust the increment after the above code.
            self.increment /= self.max_threshold;
        }
        // Now perform the standard bumping
        self.heap.increment(id, self.increment);
    }

    /// Decays the activities (i.e. increases the [`Vsids::increment`] by multiplying it
    /// with 1 / [`Vsids::decay_factor`]) such that future bumps (see
    /// [`Vsids::bump_activity`]) is more impactful.
    ///
    /// Doing it in this manner is cheaper than dividing each activity value eagerly.
    fn decay_activities(&mut self) {
        self.increment *= 1.0 / self.decay_factor;
    }

    fn next_candidate_predicate(
        &mut self,
        context: &mut SelectionContext,
    ) -> Option<IntegerPredicate> {
        loop {
            // We peek the next variable, since we do not pop since we do not (yet) want to
            // remove the value from the heap.
            if let Some((candidate, _)) = self.heap.peek_max() {
                let predicate = self
                    .predicate_id_info
                    .get_predicate(*candidate)
                    .expect("We expected present predicates to be registered.");
                if context.is_predicate_assigned(predicate) {
                    let _ = self.heap.pop_max();
                } else {
                    return Some(predicate);
                }
            } else {
                return None;
            }
        }
    }

    fn determine_polarity(&self, predicate: IntegerPredicate) -> IntegerPredicate {
        if self.best_known_solution.is_empty() {
            predicate
        } else {
            assert!(
                predicate.get_domain().id < self.best_known_solution.len() as u32,
                "For now we do not expect new variables during the search"
            );
            // Match the truth value according to the best solution.
            todo!();
        }
    }
}

impl Brancher for AutonomousSearch {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<IntegerPredicate> {
        self.next_candidate_predicate(context)
            .map(|predicate| self.determine_polarity(predicate))
    }

    /// Restores dormant predicates after backtracking.
    fn synchronise(&mut self, assignments: &AssignmentsInteger) {
        // Note that while iterating with 'retain', the function also
        // readds the predicates to the heap that are no longer dormant.
        self.dormant_predicates.retain(|predicate| {
            // Only unassigned predicates are readded.
            if assignments.evaluate_predicate(*predicate).is_none() {
                let id = self.predicate_id_info.get_id(*predicate);
                self.heap.restore_key(id);
                false
            }
            // Otherwise the predicate has a truth value, so it can be kept in the dormant vector.
            else {
                true
            }
        });
    }

    fn on_conflict(&mut self) {
        self.decay_activities();
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        todo!();
    }

    // On predicate in conflict!
}

#[cfg(test)]
mod tests {
    // use super::Vsids;
    // use crate::basic_types::tests::TestRandom;
    // use crate::branching::variable_selection::VariableSelector;
    // use crate::branching::SelectionContext;

    // #[test]
    // fn vsids_bumped_var_is_max() {
    // let (assignments_integer, assignments_propositional) =
    // SelectionContext::create_for_testing(2, 0, None);
    // let mut test_rng = TestRandom::default();
    // let context = SelectionContext::new(
    // &assignments_integer,
    // &assignments_propositional,
    // &mut test_rng,
    // );
    // let domains = context.get_domains().collect::<Vec<_>>();
    //
    // let mut vsids = Vsids::new(&domains);
    // vsids.bump_activity(domains[1]);
    //
    // let chosen = vsids.select_variable(&context);
    //
    // assert!(chosen.is_some());
    // assert_eq!(chosen.unwrap(), domains[1]);
    // }
    //
    // #[test]
    // fn vsids_no_variables_will_return_none() {
    // let mut vsids: Vsids<PropositionalVariable> = Vsids::new(&Vec::new());
    //
    // let (assignments_integer, assignments_propositional) =
    // SelectionContext::create_for_testing(0, 0, None);
    // let mut test_rng = TestRandom::default();
    // let context = SelectionContext::new(
    // &assignments_integer,
    // &assignments_propositional,
    // &mut test_rng,
    // );
    // let chosen = vsids.select_variable(&context);
    //
    // assert!(chosen.is_none());
    // }
}
