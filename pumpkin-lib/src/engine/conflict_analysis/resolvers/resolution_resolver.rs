use super::ConflictResolver;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::branching::Brancher;
use crate::containers::KeyValueHeap;
use crate::containers::StorageKey;
use crate::engine::conflict_analysis::recursive_minimiser::RecursiveMinimiser;
use crate::engine::conflict_analysis::ConflictAnalysisNogoodContext;
use crate::engine::conflict_analysis::LearnedNogood;
use crate::engine::conflict_analysis::Mode;
use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::pumpkin_assert_advanced;

#[derive(Clone, Debug, Default)]
pub struct ResolutionResolver {
    /// Heap containing the current decision level predicates.
    /// It sorts the predicates based on trail position.
    heap_current_decision_level: KeyValueHeap<PredicateId, u32>,
    /// The generator is used in combination with the heap.
    predicate_id_generator: PredicateIdGenerator,
    /// Vector containing predicates above the current decision level.
    /// The vector may contain duplicates, but will be removed at the end.
    predicates_lower_decision_level: Vec<Predicate>,
    recursive_minimiser: RecursiveMinimiser,
}

impl ConflictResolver for ResolutionResolver {
    fn resolve_conflict(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
    ) -> Option<LearnedNogood> {
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
                    let p_replacement = if context.assignments.is_decision_predicate(&p) {
                        p
                    } else {
                        let m = ConflictAnalysisNogoodContext::get_propagation_reason_simple(
                            p,
                            context.assignments,
                            context.reason_store,
                            context.propagators,
                        );
                        // We expect only not equals predicates here, which just have one reason.
                        assert!(m.len() == 1);
                        m[0]
                    };
                    self.predicates_lower_decision_level.push(p_replacement);
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
        Some(self.extract_final_nogood(context))
    }

    fn process(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
        learned_nogood: &Option<LearnedNogood>,
    ) -> Result<(), ()> {
        let learned_nogood = learned_nogood.as_ref().expect("Expected nogood");

        context.backtrack(learned_nogood.backjump_level);
        Ok(())
    }
}

impl ResolutionResolver {
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

        self.recursive_minimiser
            .remove_dominated_predicates(&mut clean_nogood, context);

        clean_nogood = context.semantic_minimiser.minimise(
            &clean_nogood,
            context.assignments,
            Mode::EnableEqualityMerging,
        );

        let num_predicates_from_current_decision_level = |nogood: &Vec<Predicate>| -> usize {
            nogood
                .iter()
                .filter(|p| {
                    context
                        .assignments
                        .get_decision_level_for_predicate(p)
                        .unwrap()
                        == context.assignments.get_decision_level()
                })
                .count()
        };

        // Due to reasoning with holes, it can be the case that we learn a nogood with multiple
        // predicates from the current decision level. This does not interact well with the
        // expectations of the solver (each nogood has an asserting predicate), so for now we work
        // around this issue by making the nogood less general.
        // The code below is not very elegant, but works ok as a temporary workaround.
        if num_predicates_from_current_decision_level(&clean_nogood) > 1 {
            let mut current_lvl_predicates: Vec<Predicate> = vec![];
            clean_nogood.retain(|p| {
                if context
                    .assignments
                    .get_decision_level_for_predicate(p)
                    .unwrap()
                    == context.assignments.get_decision_level()
                {
                    current_lvl_predicates.push(*p);
                    false
                } else {
                    true
                }
            });

            let mut revised_version: Vec<Predicate> = vec![];
            for p in current_lvl_predicates {
                let p_replacement = if !context.assignments.is_decision_predicate(&p) {
                    let m = ConflictAnalysisNogoodContext::get_propagation_reason_simple(
                        p,
                        context.assignments,
                        context.reason_store,
                        context.propagators,
                    );
                    // We expect only expect single-reason substitutions, since the problem comes
                    // from assigning a [x = v] as a decision.
                    assert!(m.len() == 1);
                    m[0]
                } else {
                    p
                };
                revised_version.push(p_replacement);
            }

            let decision_level_predicate = context.semantic_minimiser.minimise(
                &revised_version,
                context.assignments,
                Mode::EnableEqualityMerging,
            );
            assert!(decision_level_predicate.len() == 1);
            clean_nogood.push(decision_level_predicate[0]);
        }

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
}
