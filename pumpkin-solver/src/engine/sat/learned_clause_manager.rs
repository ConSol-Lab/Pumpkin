use super::AssignmentsPropositional;
use crate::basic_types::ClauseReference;
use crate::engine::clause_allocators::ClauseAllocatorInterface;
use crate::engine::clause_allocators::ClauseInterface;
use crate::engine::constraint_satisfaction_solver::ClausalPropagatorType;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::engine::variables::Literal;
use crate::propagators::clausal::is_clause_propagating;
use crate::propagators::clausal::ClausalPropagator;
use crate::pumpkin_assert_moderate;
#[cfg(doc)]
use crate::Solver;

/// Options which determine how the learned clauses are handled within the [`Solver`]. These options
/// influence when the learned clause database removed clauses.
#[derive(Debug, Copy, Clone)]
pub struct LearningOptions {
    /// Determines when to rescale the activites of the learned clauses in the database.
    pub max_clause_activity: f32,
    /// Determines the factor by which the activities are divided when a conflict is found.
    pub clause_activity_decay_factor: f32,
    /// The maximum number of clauses with an LBD higher than [`LearningOptions::lbd_threshold`]
    /// allowed by the learned clause database. If there are more clauses with an LBD higher than
    /// [`LearningOptions::lbd_threshold`] then removal from the database will be considered.
    pub num_high_lbd_learned_clauses_max: u64,
    /// Specifies how the learned clauses are sorted when considering removal.
    pub high_lbd_learned_clause_sorting_strategy: LearnedClauseSortingStrategy,
    /// The treshold which specifies whether a learned clause database is considered to be with
    /// "High" LBD or "Low" LBD. Learned clauses with high LBD will be considered for removal.
    pub lbd_threshold: u32,
}

impl Default for LearningOptions {
    fn default() -> Self {
        Self {
            max_clause_activity: 1e20,
            clause_activity_decay_factor: 0.99,
            num_high_lbd_learned_clauses_max: 4000,
            high_lbd_learned_clause_sorting_strategy: LearnedClauseSortingStrategy::Activity,
            lbd_threshold: 5,
        }
    }
}

/// The sorting strategy which is used when considering removal from the clause database.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LearnedClauseSortingStrategy {
    /// Sorts based on the activity, the activity is bumped when a literal is encountered during
    /// conflict analysis.
    Activity,
    /// Sorts based on the literal block distance (LBD) which is an indication of how "good" a
    /// learned clause is.
    Lbd,
}

impl std::fmt::Display for LearnedClauseSortingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LearnedClauseSortingStrategy::Lbd => write!(f, "lbd"),
            LearnedClauseSortingStrategy::Activity => write!(f, "activity"),
        }
    }
}

#[derive(Default, Debug)]
struct LearnedClauses {
    low_lbd: Vec<ClauseReference>,
    high_lbd: Vec<ClauseReference>,
}

// todo explain the learned clause removal strategy

#[derive(Debug)]
pub(crate) struct LearnedClauseManager {
    learned_clauses: LearnedClauses,
    parameters: LearningOptions,
    clause_bump_increment: f32,
}

impl LearnedClauseManager {
    pub(crate) fn new(sat_options: LearningOptions) -> Self {
        LearnedClauseManager {
            learned_clauses: LearnedClauses::default(),
            parameters: sat_options,
            clause_bump_increment: 1.0,
        }
    }

    pub(crate) fn add_learned_clause(
        &mut self,
        learned_clause_literals: Vec<Literal>,
        clausal_propagator: &mut ClausalPropagatorType,
        assignments: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) -> ClauseReference {
        let result = clausal_propagator.add_asserting_learned_clause(
            learned_clause_literals,
            assignments,
            clause_allocator,
        );
        // only update if the clause is treated as a standard clause
        //  note that in case of binary clauses, these may be stored directly in the watch lists and
        // not as a standard clause
        if let Some(clause_reference) = result {
            self.update_lbd(clause_reference, assignments, clause_allocator);

            if clause_allocator[clause_reference].lbd() <= self.parameters.lbd_threshold {
                self.learned_clauses.low_lbd.push(clause_reference);
            } else {
                self.learned_clauses.high_lbd.push(clause_reference);
            }

            return clause_reference;
        }

        unreachable!("This should always allocate a clause");
    }

    pub(crate) fn shrink_learned_clause_database_if_needed(
        &mut self,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
        clausal_propagator: &mut ClausalPropagatorType,
    ) {
        // only consider clause removals once the threshold is reached
        if self.learned_clauses.high_lbd.len()
            <= self.parameters.num_high_lbd_learned_clauses_max as usize
        {
            return;
        }

        // we divide the procedure in two steps:
        //  + promote clauses that are in the high lbd group but achieved low lbd
        //  + remove roughly of the clauses that have high lbd
        // this could be done in a single step but for simplicity we keep it as two separate steps

        self.promote_high_lbd_clauses(clause_allocator);

        self.remove_high_lbd_clauses(assignments, clause_allocator, clausal_propagator);
    }

    fn remove_high_lbd_clauses(
        &mut self,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
        clausal_propagator: &mut ClausalPropagatorType,
    ) {
        // roughly half of the learned clauses will be removed

        self.sort_high_lbd_clauses_by_quality_decreasing_order(clause_allocator);

        // the removal is done in two phases
        //  in the first phase, clauses are deleted but the clause references are not removed from
        // self.learned_clauses  in the second phase, the corresponding clause references
        // are removed from the learned clause vector
        let mut num_clauses_to_remove = self.learned_clauses.high_lbd.len() as u64
            - self.parameters.num_high_lbd_learned_clauses_max / 2;
        // note the 'rev', since we give priority to poor clauses for deletion
        //  even though we aim to remove half of the clauses, less could be removed if many clauses
        // are protected or in propagation
        for &clause_reference in self.learned_clauses.high_lbd.iter().rev() {
            if num_clauses_to_remove == 0 {
                break;
            }

            // protected clauses are skipped
            if clause_allocator[clause_reference].is_protected_against_deletion() {
                clause_allocator[clause_reference].clear_protection_against_deletion();
                continue;
            }

            // clauses that are currently in propagation are skipped
            //  otherwise there may be problems with conflict analysis
            if is_clause_propagating(assignments, clause_allocator, clause_reference) {
                continue;
            }

            // remove the clause from the watch list
            clausal_propagator.remove_clause_from_consideration(
                clause_allocator[clause_reference].get_literal_slice(),
                clause_reference,
            );

            // delete the clause
            clause_allocator.delete_clause(clause_reference);

            num_clauses_to_remove -= 1;
        }

        self.learned_clauses
            .high_lbd
            .retain(|&clause_reference| !clause_allocator[clause_reference].is_deleted());
    }

    fn sort_high_lbd_clauses_by_quality_decreasing_order(
        &mut self,
        clause_allocator: &mut ClauseAllocator,
    ) {
        // sort the learned clauses
        // the ordering is such that the better clauses are in the front
        //  note that this is not the most efficient sorting comparison, but will do for now
        //  e.g., sort_by_lbd could be moved out, and the comparison of floats could be changed
        // possibly
        self.learned_clauses
            .high_lbd
            .sort_unstable_by(|clause_reference1, clause_reference2| {
                let clause1 = clause_allocator.get_clause(*clause_reference1);
                let clause2 = clause_allocator.get_clause(*clause_reference2);

                match self.parameters.high_lbd_learned_clause_sorting_strategy {
                    LearnedClauseSortingStrategy::Activity => {
                        // note that here we reverse clause1 and clause2, because a higher value for
                        // activity is better
                        clause2
                            .get_activity()
                            .partial_cmp(&clause1.get_activity())
                            .unwrap()
                    }
                    LearnedClauseSortingStrategy::Lbd => {
                        if clause1.lbd() != clause2.lbd() {
                            clause1.lbd().cmp(&clause2.lbd())
                        } else {
                            // note that here we reverse clause1 and clause2, because a higher value
                            // for activity is better
                            clause2
                                .get_activity()
                                .partial_cmp(&clause1.get_activity())
                                .unwrap()
                        }
                    }
                }
            });
    }

    fn promote_high_lbd_clauses(&mut self, clause_allocator: &mut ClauseAllocator) {
        // promote clauses: we do this in two passes for simplicity of implementation
        //  add the clauses references to the low_lbd group
        for &clause_reference in &self.learned_clauses.high_lbd {
            let lbd = clause_allocator[clause_reference].lbd();
            if lbd <= self.parameters.lbd_threshold {
                self.learned_clauses.low_lbd.push(clause_reference);
            }
        }
        //  remove the low lbd clauses from the high_lbd group
        self.learned_clauses.high_lbd.retain(|&clause_reference| {
            clause_allocator[clause_reference].lbd() > self.parameters.lbd_threshold
        });
    }

    pub(crate) fn update_clause_lbd_and_bump_activity(
        &mut self,
        clause_reference: ClauseReference,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) {
        if clause_allocator.get_clause(clause_reference).is_learned()
            && clause_allocator.get_clause(clause_reference).lbd() > self.parameters.lbd_threshold
        {
            self.bump_clause_activity(clause_reference, clause_allocator);
            self.update_lbd(clause_reference, assignments, clause_allocator);
        }
    }

    pub(crate) fn update_lbd(
        &mut self,
        clause_reference: ClauseReference,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) {
        let new_lbd = self.compute_lbd_for_literals(
            clause_allocator[clause_reference].get_literal_slice(),
            assignments,
        );
        if new_lbd < clause_allocator[clause_reference].lbd() {
            clause_allocator[clause_reference].update_lbd(new_lbd);
            if new_lbd <= 30 {
                clause_allocator[clause_reference].mark_protection_against_deletion();
            }
        }
    }

    pub(crate) fn compute_lbd_for_literals(
        &self,
        literals: &[Literal],
        assignments: &AssignmentsPropositional,
    ) -> u32 {
        pumpkin_assert_moderate!(
            literals
                .iter()
                .all(|lit| assignments.is_literal_assigned(*lit)),
            "Cannot compute LBD if not all literals are assigned."
        );
        // the LBD is the number of literals at different decision levels
        //  this implementation should be improved
        let mut codes: Vec<usize> = literals
            .iter()
            .filter_map(|lit| {
                let level = assignments.get_literal_assignment_level(*lit);
                if level > 0 {
                    Some(level)
                } else {
                    // level zero should not be counted towards the LBD
                    None
                }
            })
            .collect();
        codes.sort_unstable();
        codes.dedup();
        codes.len() as u32
    }

    pub(crate) fn bump_clause_activity(
        &mut self,
        clause_reference: ClauseReference,
        clause_allocator: &mut ClauseAllocator,
    ) {
        // check if bumping the activity would lead to a large activity value
        if clause_allocator.get_clause(clause_reference).get_activity() + self.clause_bump_increment
            > self.parameters.max_clause_activity
        {
            // if so, rescale all activity values
            self.rescale_clause_activities(clause_allocator);
        }
        // at this point, it is safe to increase the activity value
        clause_allocator
            .get_mutable_clause(clause_reference)
            .increase_activity(self.clause_bump_increment);
    }

    pub(crate) fn rescale_clause_activities(&mut self, clause_allocator: &mut ClauseAllocator) {
        self.learned_clauses
            .high_lbd
            .iter()
            .for_each(|clause_reference| {
                let clause = clause_allocator.get_mutable_clause(*clause_reference);
                clause.divide_activity(self.parameters.max_clause_activity);
            });
        self.clause_bump_increment /= self.parameters.max_clause_activity;
    }

    pub(crate) fn decay_clause_activities(&mut self) {
        self.clause_bump_increment /= self.parameters.clause_activity_decay_factor;
    }
}
