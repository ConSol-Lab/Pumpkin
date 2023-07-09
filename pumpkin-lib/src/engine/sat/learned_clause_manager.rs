use crate::{
    basic_types::{ClauseReference, Literal},
    engine::{
        clause_allocators::{ClauseAllocatorInterface, ClauseInterface},
        constraint_satisfaction_solver::{ClausalPropagator, ClauseAllocator},
    },
    propagators::clausal_propagators::ClausalPropagatorInterface,
    pumpkin_assert_extreme, pumpkin_assert_moderate,
};

use super::{AssignmentsPropositional, LearnedClauseSortingStrategy, SATEngineDataStructures};

pub struct SatOptions {
    pub max_clause_activity: f32,
    pub clause_activity_decay_factor: f32,
    pub num_learned_clauses_max: u64,
    pub learned_clause_sorting_strategy: LearnedClauseSortingStrategy,
}

impl Default for SatOptions {
    fn default() -> Self {
        Self {
            max_clause_activity: 1e20,
            clause_activity_decay_factor: 0.99,
            num_learned_clauses_max: 4000,
            learned_clause_sorting_strategy: LearnedClauseSortingStrategy::Lbd,
        }
    }
}

pub struct LearnedClauseManager {
    learned_clauses: Vec<ClauseReference>,
    parameters: SatOptions,
    clause_bump_increment: f32,
}

impl LearnedClauseManager {
    pub fn new(sat_options: SatOptions) -> Self {
        LearnedClauseManager {
            learned_clauses: vec![],
            parameters: sat_options,
            clause_bump_increment: 1.0,
        }
    }

    pub fn add_learned_clause(
        &mut self,
        learned_clause_literals: Vec<Literal>,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) {
        let asserting_literal = learned_clause_literals[0];

        let clause_reference = clausal_propagator.add_clause_unchecked(
            learned_clause_literals,
            true,
            &mut sat_data_structures.clause_allocator,
        );

        self.learned_clauses.push(clause_reference);

        sat_data_structures
            .assignments_propositional
            .enqueue_propagated_literal(asserting_literal, clause_reference.into());

        self.update_lbd(
            clause_reference,
            &sat_data_structures.assignments_propositional,
            &mut sat_data_structures.clause_allocator,
        );
    }

    pub fn shrink_learned_clause_database_if_needed(
        &mut self,
        clausal_propagator: &mut ClausalPropagator,
        sat_data_structures: &mut SATEngineDataStructures,
    ) {
        pumpkin_assert_moderate!(
            sat_data_structures
                .assignments_propositional
                .is_at_the_root_level(),
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
                let clause1 = sat_data_structures
                    .clause_allocator
                    .get_clause(*clause_reference1);
                let clause2 = sat_data_structures
                    .clause_allocator
                    .get_clause(*clause_reference2);

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

        //clauses are sorted by quality in increasing order

        //removal of clauses is done in two phases
        //  in the first phase, clauses are deleted but the clause references are not removed from self.learned_clauses
        //  in the second phase, the corresponding clause references are removed from the learned clause vector
        let mut num_clauses_to_remove =
            self.learned_clauses.len() as u64 - self.parameters.num_learned_clauses_max;
        //note the 'rev', since we give priority to poor clauses
        for &clause_reference in self.learned_clauses.iter().rev() {
            if num_clauses_to_remove == 0 {
                break;
            }

            if sat_data_structures.clause_allocator[clause_reference]
                .is_protected_against_deletion()
            {
                sat_data_structures.clause_allocator[clause_reference]
                    .clear_protection_against_deletion();
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

        self.learned_clauses.retain(|&clause_reference| {
            !sat_data_structures.clause_allocator[clause_reference].is_deleted()
        });

        pumpkin_assert_extreme!(clausal_propagator.debug_check_state(
            &sat_data_structures.assignments_propositional,
            &sat_data_structures.clause_allocator
        ));
    }

    pub fn update_clause_lbd_and_bump_activity(
        &mut self,
        clause_reference: ClauseReference,
        assignments: &AssignmentsPropositional,
        clause_allocator: &mut ClauseAllocator,
    ) {
        if clause_allocator.get_clause(clause_reference).is_learned()
            && clause_allocator.get_clause(clause_reference).get_lbd() > 2
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
        if new_lbd < clause_allocator[clause_reference].get_lbd() {
            clause_allocator[clause_reference].update_lbd(new_lbd);
            clause_allocator[clause_reference].mark_protection_against_deletion();
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
        let mut codes: Vec<u32> = literals
            .iter()
            .map(|lit| assignments.get_literal_assignment_level(*lit))
            .collect::<Vec<u32>>();
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
        self.learned_clauses.iter().for_each(|clause_reference| {
            let clause = clause_allocator.get_mutable_clause(*clause_reference);
            clause.divide_activity(self.parameters.max_clause_activity);
        });
        self.clause_bump_increment /= self.parameters.max_clause_activity;
    }

    pub fn decay_clause_activities(&mut self) {
        self.clause_bump_increment /= self.parameters.clause_activity_decay_factor;
    }
}
