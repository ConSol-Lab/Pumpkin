use super::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::SolutionReference;
use crate::branching::value_selection::InDomainMin;
use crate::branching::variable_selection::Smallest;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::containers::KeyValueHeap;
use crate::containers::StorageKey;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::Assignments;
use crate::results::Solution;
use crate::DefaultBrancher;
/// A [`Brancher`] that combines [VSIDS \[1\]](https://dl.acm.org/doi/pdf/10.1145/378239.379017)
/// and [Solution-based phase saving \[2\]](https://people.eng.unimelb.edu.au/pstuckey/papers/lns-restarts.pdf).
///
/// There are three components:
/// 1. Predicate selection
/// 2. Truth value assignment
/// 3. Backup Selection
///
/// # Predicate selection
/// The VSIDS algorithm is an adaptation for the CP case. It determines which
/// [`Predicate`] should be branched on based on how often it appears in conflicts.
///
/// Intuitively, the more often a [`Predicate`] appears in *recent* conflicts, the more "important"
/// it is during the search process. VSIDS is originally from the SAT field (see \[1\]) but we
/// adapted it for constraint programming by considering [`Predicate`]s from recent conflicts
/// directly rather than Boolean variables.
///
/// # Truth value assignment
/// The truth value for the [`Predicate`] is selected to be consistent with the
/// best solution known so far. In this way, the search is directed around this existing solution.
///
/// In case where there is no known solution, then the predicate is assigned to true. This resembles
/// a fail-first strategy with the idea that the given predicate was encountered in conflicts, so
/// assigning it to true may cause another conflict soon.
///
/// # Backup selection
/// VSIDS relies on [`Predicate`]s appearing in conflicts to discover which [`Predicate`]s are
/// "important". However, it could be the case that all [`Predicate`]s which VSIDS has discovered
/// are already assigned.
///
/// In this case, [`AutonomousSearch`] defaults either to the backup described in
/// [`DefaultBrancher`] (when created using [`AutonomousSearch::default_over_all_variables`]) or it
/// defaults to the [`Brancher`] provided to [`AutonomousSearch::new`].
///
/// # Bibliography
/// \[1\] M. W. Moskewicz, C. F. Madigan, Y. Zhao, L. Zhang, and S. Malik, ‘Chaff: Engineering an
/// efficient SAT solver’, in Proceedings of the 38th annual Design Automation Conference, 2001.
///
/// \[2\] E. Demirović, G. Chu, and P. J. Stuckey, ‘Solution-based phase saving for CP: A
/// value-selection heuristic to simulate local search behavior in complete solvers’, in the
/// proceedings of the Principles and Practice of Constraint Programming (CP 2018).
#[derive(Debug)]

pub struct AutonomousSearch<BackupBrancher> {
    /// Predicates are mapped to ids. This is used internally in the heap.
    predicate_id_info: PredicateIdGenerator,
    /// Stores the activities for a predicate, represented with its id.
    heap: KeyValueHeap<PredicateId, f64>,
    /// After popping predicates off the heap that current have a truth value, the predicates are
    /// labelled as dormant because they do not contribute to VSIDS at the moment. When
    /// backtracking, dormant predicates are examined and readded to the heap. Dormant predicates
    /// with low activities are removed.
    dormant_predicates: Vec<Predicate>,
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
    /// Contains the best-known solution or [`None`] if no solution has been found.
    best_known_solution: Option<Solution>,
    /// If the heap does not contain any more unfixed predicates then this backup_brancher will be
    /// used instead.
    backup_brancher: BackupBrancher,
}

const DEFAULT_VSIDS_INCREMENT: f64 = 1.0;
const DEFAULT_VSIDS_MAX_THRESHOLD: f64 = 1e100;
const DEFAULT_VSIDS_DECAY_FACTOR: f64 = 0.95;
const DEFAULT_VSIDS_VALUE: f64 = 0.0;

impl DefaultBrancher {
    /// Creates a new instance with default values for
    /// the parameters (`1.0` for the increment, `1e100` for the max threshold,
    /// `0.95` for the decay factor and `0.0` for the initial VSIDS value).
    ///
    /// If there are no more predicates left to select, this [`Brancher`] switches to
    /// [`Smallest`] with [`InDomainMin`].
    pub fn default_over_all_variables(assignments: &Assignments) -> DefaultBrancher {
        let variables = assignments.get_domains().collect::<Vec<_>>();
        AutonomousSearch {
            predicate_id_info: PredicateIdGenerator::default(),
            heap: KeyValueHeap::default(),
            dormant_predicates: vec![],
            increment: DEFAULT_VSIDS_INCREMENT,
            max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
            decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
            best_known_solution: None,
            backup_brancher: IndependentVariableValueBrancher::new(
                Smallest::new(&variables),
                InDomainMin,
            ),
        }
    }
}

impl<BackupSelector> AutonomousSearch<BackupSelector> {
    /// Creates a new instance with default values for
    /// the parameters (`1.0` for the increment, `1e100` for the max threshold,
    /// `0.95` for the decay factor and `0.0` for the initial VSIDS value).
    ///
    /// Uses the `backup_brancher` in case there are no more predicates to be selected by VSIDS.
    pub fn new(backup_brancher: BackupSelector) -> Self {
        AutonomousSearch {
            predicate_id_info: PredicateIdGenerator::default(),
            heap: KeyValueHeap::default(),
            dormant_predicates: vec![],
            increment: DEFAULT_VSIDS_INCREMENT,
            max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
            decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
            best_known_solution: None,
            backup_brancher,
        }
    }

    /// Resizes the heap to accommodate for the id.
    /// Recall that the underlying heap uses direct hashing.
    fn resize_heap(&mut self, id: PredicateId) {
        while self.heap.len() <= id.index() {
            self.heap.grow(id, DEFAULT_VSIDS_VALUE);
        }
    }

    /// Bumps the activity of a predicate by [`Vsids::increment`].
    /// Used when a predicate is encountered during a conflict.
    fn bump_activity(&mut self, predicate: Predicate) {
        let id = self.predicate_id_info.get_id(predicate);
        self.resize_heap(id);
        self.heap.restore_key(id);

        // Scale the activities if the values are too large.
        // Also remove predicates that have activities close to zero.
        let activity = self.heap.get_value(id);
        if activity + self.increment >= self.max_threshold {
            // Adjust heap values.
            self.heap.divide_values(self.max_threshold);

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

    fn next_candidate_predicate(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
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

                    // We know that this predicate is now dormant
                    let predicate_id = self.predicate_id_info.get_id(predicate);
                    self.heap.delete_key(predicate_id);
                    self.predicate_id_info.delete_id(predicate_id);
                    self.dormant_predicates.push(predicate);
                } else {
                    return Some(predicate);
                }
            } else {
                return None;
            }
        }
    }

    /// Determines whether the provided [`Predicate`] should be returned as is or whether its
    /// negation should be returned. This is determined based on its assignment in the best-known
    /// solution.
    ///
    /// For example, if we have found the solution `x = 5` then the call `determine_polarity([x >=
    /// 3])` would return `true`.
    fn determine_polarity(&self, predicate: Predicate) -> Predicate {
        if let Some(solution) = &self.best_known_solution {
            // We have a solution
            if !solution.contains_domain_id(predicate.get_domain()) {
                // This can occur if an encoding is used
                return predicate;
            }
            // Match the truth value according to the best solution.
            if solution.is_predicate_satisfied(predicate) {
                predicate
            } else {
                !predicate
            }
        } else {
            // We do not have a solution to match against, we simply return the predicate with
            // positive polarity
            predicate
        }
    }
}

impl<BackupBrancher: Brancher> Brancher for AutonomousSearch<BackupBrancher> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        let result = self
            .next_candidate_predicate(context)
            .map(|predicate| self.determine_polarity(predicate));
        if result.is_none() && !context.are_all_variables_assigned() {
            // There are variables for which we do not have a predicate, rely on the backup
            self.backup_brancher.next_decision(context)
        } else {
            result
        }
    }

    fn on_backtrack(&mut self) {
        self.backup_brancher.on_backtrack()
    }

    /// Restores dormant predicates after backtracking.
    fn synchronise(&mut self, assignments: &Assignments) {
        // Note that while iterating with 'retain', the function also
        // re-adds the predicates to the heap that are no longer dormant.
        self.dormant_predicates.retain(|predicate| {
            // Only unassigned predicates are readded.
            if assignments.evaluate_predicate(*predicate).is_none() {
                let id = self.predicate_id_info.get_id(*predicate);

                while self.heap.len() <= id.index() {
                    self.heap.grow(id, DEFAULT_VSIDS_VALUE);
                }

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

    fn on_solution(&mut self, solution: SolutionReference) {
        // We store the best known solution
        self.best_known_solution = Some(solution.into());
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.bump_activity(predicate)
    }

    fn is_restart_pointless(&mut self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::AutonomousSearch;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::branchers::autonomous_search::DEFAULT_VSIDS_MAX_THRESHOLD;
    use crate::branching::Brancher;
    use crate::branching::SelectionContext;
    use crate::engine::Assignments;
    use crate::predicate;
    use crate::results::SolutionReference;

    #[test]
    fn brancher_picks_bumped_values() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(0, 10);
        let y = assignments.grow(-10, 0);

        let mut brancher = AutonomousSearch::default_over_all_variables(&assignments);
        brancher.on_appearance_in_conflict_predicate(predicate!(x >= 5));
        brancher.on_appearance_in_conflict_predicate(predicate!(x >= 5));
        brancher.on_appearance_in_conflict_predicate(predicate!(y >= -5));

        (0..100).for_each(|_| brancher.on_conflict());
    }

    #[test]
    fn value_removed_if_threshold_too_small() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(0, 10);
        let y = assignments.grow(-10, 0);

        let mut brancher = AutonomousSearch::default_over_all_variables(&assignments);
        brancher.on_appearance_in_conflict_predicate(predicate!(x >= 5));
        brancher.on_appearance_in_conflict_predicate(predicate!(y >= -5));

        brancher.increment = DEFAULT_VSIDS_MAX_THRESHOLD;

        brancher.on_appearance_in_conflict_predicate(predicate!(y >= -5));

        assert!(!brancher
            .predicate_id_info
            .has_id_for_predicate(predicate!(x >= 5)));
        assert!(brancher
            .predicate_id_info
            .has_id_for_predicate(predicate!(y >= -5)));
    }

    #[test]
    fn dormant_values() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(0, 10);

        let mut brancher = AutonomousSearch::default_over_all_variables(&assignments);

        let predicate = predicate!(x >= 5);
        brancher.on_appearance_in_conflict_predicate(predicate);
        let decision = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert_eq!(decision, Some(predicate));

        assignments.increase_decision_level();
        // Decision Level 1
        let _ = assignments.tighten_lower_bound(x, 5, None);

        assignments.increase_decision_level();
        // Decision Level 2
        let _ = assignments.tighten_lower_bound(x, 7, None);

        assignments.increase_decision_level();
        // Decision Level 3
        let _ = assignments.tighten_lower_bound(x, 10, None);

        assignments.increase_decision_level();
        // We end at decision level 4

        let decision = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(decision.is_none());
        assert!(brancher.dormant_predicates.contains(&predicate));

        let _ = assignments.synchronise(3, usize::MAX, false);

        let decision = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(decision.is_none());
        assert!(brancher.dormant_predicates.contains(&predicate));

        let _ = assignments.synchronise(0, usize::MAX, false);
        brancher.synchronise(&assignments);

        let decision = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert_eq!(decision, Some(predicate));
        assert!(!brancher.dormant_predicates.contains(&predicate));
    }

    #[test]
    fn uses_fallback() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(0, 10);

        let mut brancher = AutonomousSearch::default_over_all_variables(&assignments);

        let result = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom {
                usizes: vec![],
                weighted_choice: |_| unreachable!(),
                ..Default::default()
            },
        ));

        assert_eq!(result, Some(predicate!(x <= 0)));
    }

    #[test]
    fn uses_stored_solution() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(0, 10);

        assignments.increase_decision_level();
        let _ = assignments.make_assignment(x, 7, None);

        let mut brancher = AutonomousSearch::default_over_all_variables(&assignments);

        brancher.on_solution(SolutionReference::new(&assignments));

        let _ = assignments.synchronise(0, usize::MAX, false);

        assert_eq!(
            predicate!(x >= 5),
            brancher.determine_polarity(predicate!(x >= 5))
        );
        assert_eq!(
            !predicate!(x >= 10),
            brancher.determine_polarity(predicate!(x >= 10))
        );
        assert_eq!(
            predicate!(x <= 8),
            brancher.determine_polarity(predicate!(x <= 8))
        );
        assert_eq!(
            !predicate!(x <= 5),
            brancher.determine_polarity(predicate!(x <= 5))
        );

        brancher.on_appearance_in_conflict_predicate(predicate!(x >= 5));

        let result = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert_eq!(result, Some(predicate!(x >= 5)));
    }
}
