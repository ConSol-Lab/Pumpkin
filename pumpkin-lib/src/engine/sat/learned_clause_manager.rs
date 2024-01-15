use super::AssignmentsPropositional;
use super::LearnedClauseSortingStrategy;
use super::SATEngineDataStructures;
use crate::basic_types::ClauseReference;
use crate::basic_types::Literal;
use crate::engine::clause_allocators::ClauseAllocatorInterface;
use crate::engine::clause_allocators::ClauseInterface;
use crate::engine::constraint_satisfaction_solver::ClausalPropagator;
use crate::engine::constraint_satisfaction_solver::ClauseAllocator;
use crate::propagators::clausal_propagators::ClausalPropagatorInterface;
use crate::pumpkin_assert_moderate;

pub struct SatOptions {
    pub max_clause_activity: f32,
    pub clause_activity_decay_factor: f32,
    pub num_high_lbd_learned_clauses_max: u64,
    pub high_lbd_learned_clause_sorting_strategy: LearnedClauseSortingStrategy,
    pub lbd_threshold: u32,
}

impl Default for SatOptions {
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

#[derive(Default)]
struct LearnedClauses {
    low_lbd: Vec<ClauseReference>,
    high_lbd: Vec<ClauseReference>,
}

//todo explain the learned clause removal strategy

pub struct LearnedClauseManager {
    learned_clauses: LearnedClauses,
    parameters: SatOptions,
    clause_bump_increment: f32,
}

impl LearnedClauseManager {
    pub fn new(sat_options: SatOptions) -> Self {
        LearnedClauseManager {
            learned_clauses: LearnedClauses::default(),
            parameters: sat_options,
            clause_bump_increment: 1.0,
        }
    }

    pub fn add_learned_clause(
        &mut self,
        learned_clause_literals: Vec<Literal>,
        clausal_propagator: &mut ClausalPropagator,
        assignments: &mut AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) {
        let result = clausal_propagator.add_asserting_learned_clause(
            learned_clause_literals,
            assignments,
            clause_allocator,
        );
        //only update if the clause is treated as a standard clause
        //  note that in case of binary clauses, these may be stored directly in the watch lists and not as a standard clause
        if let Some(clause_reference) = result {
            self.update_lbd(clause_reference, assignments, clause_allocator);

            if clause_allocator[clause_reference].lbd() <= self.parameters.lbd_threshold {
                self.learned_clauses.low_lbd.push(clause_reference);
            } else {
                self.learned_clauses.high_lbd.push(clause_reference);
            }
        }
    }

    pub fn shrink_learned_clause_database_if_needed(
        &mut self,
        sat_data_structures: &mut SATEngineDataStructures,
        clausal_propagator: &mut ClausalPropagator,
    ) {
        //only consider clause removals once the threshold is reached
        if self.learned_clauses.high_lbd.len()
            <= self.parameters.num_high_lbd_learned_clauses_max as usize
        {
            return;
        }

        //we divide the procedure in two steps:
        //  + promote clauses that are in the high lbd group but achieved low lbd
        //  + remove roughly of the clauses that have high lbd
        //this could be done in a single step but for simplicity we keep it as two separate steps

        self.promote_high_lbd_clauses(sat_data_structures);

        self.remove_high_lbd_clauses(sat_data_structures, clausal_propagator);
    }

    fn remove_high_lbd_clauses(
        &mut self,
        sat_data_structures: &mut SATEngineDataStructures,
        clausal_propagator: &mut ClausalPropagator,
    ) {
        //roughly half of the learned clauses will be removed

        self.sort_high_lbd_clauses_by_quality_decreasing_order(sat_data_structures);

        //the removal is done in two phases
        //  in the first phase, clauses are deleted but the clause references are not removed from self.learned_clauses
        //  in the second phase, the corresponding clause references are removed from the learned clause vector
        let mut num_clauses_to_remove = self.learned_clauses.high_lbd.len() as u64
            - self.parameters.num_high_lbd_learned_clauses_max / 2;
        //note the 'rev', since we give priority to poor clauses for deletion
        //  even though we aim to remove half of the clauses, less could be removed if many clauses are protected or in propagation
        for &clause_reference in self.learned_clauses.high_lbd.iter().rev() {
            if num_clauses_to_remove == 0 {
                break;
            }

            //protected clauses are skipped
            if sat_data_structures.clause_allocator[clause_reference]
                .is_protected_against_deletion()
            {
                sat_data_structures.clause_allocator[clause_reference]
                    .clear_protection_against_deletion();
                continue;
            }

            //clauses that are currently in propagation are skipped
            //  otherwise there may be problems with conflict analysis
            if sat_data_structures.is_clause_propagating(clause_reference) {
                continue;
            }

            //remove the clause from the watch list
            clausal_propagator.remove_clause_from_consideration(
                sat_data_structures.clause_allocator[clause_reference].get_literal_slice(),
                clause_reference,
            );

            //delete the clause
            sat_data_structures
                .clause_allocator
                .delete_clause(clause_reference);

            num_clauses_to_remove -= 1;
        }

        self.learned_clauses.high_lbd.retain(|&clause_reference| {
            !sat_data_structures.clause_allocator[clause_reference].is_deleted()
        });
    }

    fn sort_high_lbd_clauses_by_quality_decreasing_order(
        &mut self,
        sat_data_structures: &mut SATEngineDataStructures,
    ) {
        //sort the learned clauses
        //the ordering is such that the better clauses are in the front
        //  note that this is not the most efficient sorting comparison, but will do for now
        //  e.g., sort_by_lbd could be moved out, and the comparison of floats could be changed possibly
        self.learned_clauses
            .high_lbd
            .sort_unstable_by(|clause_reference1, clause_reference2| {
                let clause1 = sat_data_structures
                    .clause_allocator
                    .get_clause(*clause_reference1);
                let clause2 = sat_data_structures
                    .clause_allocator
                    .get_clause(*clause_reference2);

                match self.parameters.high_lbd_learned_clause_sorting_strategy {
                    LearnedClauseSortingStrategy::Activity => {
                        //note that here we reverse clause1 and clause2, because a higher value for activity is better
                        clause2
                            .get_activity()
                            .partial_cmp(&clause1.get_activity())
                            .unwrap()
                    }
                    LearnedClauseSortingStrategy::LBD => {
                        if clause1.lbd() != clause2.lbd() {
                            clause1.lbd().cmp(&clause2.lbd())
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
    }

    fn promote_high_lbd_clauses(&mut self, sat_data_structures: &mut SATEngineDataStructures) {
        //promote clauses: we do this in two passes for simplicity of implementation
        //  add the clauses references to the low_lbd group
        for &clause_reference in &self.learned_clauses.high_lbd {
            let lbd = sat_data_structures.clause_allocator[clause_reference].lbd();
            if lbd <= self.parameters.lbd_threshold {
                self.learned_clauses.low_lbd.push(clause_reference);
            }
        }
        //  remove the low lbd clauses from the high_lbd group
        self.learned_clauses.high_lbd.retain(|&clause_reference| {
            sat_data_structures.clause_allocator[clause_reference].lbd()
                > self.parameters.lbd_threshold
        });
    }

    pub fn update_clause_lbd_and_bump_activity(
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

    pub fn update_lbd(
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

    pub fn compute_lbd_for_literals(
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
        //the LBD is the number of literals at different decision levels
        //  this implementation should be improved
        let mut codes: Vec<usize> = literals
            .iter()
            .filter_map(|lit| {
                let level = assignments.get_literal_assignment_level(*lit);
                if level > 0 {
                    Some(level)
                } else {
                    //level zero should not be counted towards the LBD
                    None
                }
            })
            .collect();
        codes.sort_unstable();
        codes.dedup();
        codes.len() as u32
    }

    pub fn bump_clause_activity(
        &mut self,
        clause_reference: ClauseReference,
        clause_allocator: &mut ClauseAllocator,
    ) {
        //check if bumping the activity would lead to a large activity value
        if clause_allocator.get_clause(clause_reference).get_activity() + self.clause_bump_increment
            > self.parameters.max_clause_activity
        {
            //if so, rescale all activity values
            self.rescale_clause_activities(clause_allocator);
        }
        //at this point, it is safe to increase the activity value
        clause_allocator
            .get_mutable_clause(clause_reference)
            .increase_activity(self.clause_bump_increment);
    }

    pub fn rescale_clause_activities(&mut self, clause_allocator: &mut ClauseAllocator) {
        self.learned_clauses
            .high_lbd
            .iter()
            .for_each(|clause_reference| {
                let clause = clause_allocator.get_mutable_clause(*clause_reference);
                clause.divide_activity(self.parameters.max_clause_activity);
            });
        self.clause_bump_increment /= self.parameters.max_clause_activity;
    }

    pub fn decay_clause_activities(&mut self) {
        self.clause_bump_increment /= self.parameters.clause_activity_decay_factor;
    }
}
