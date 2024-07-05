use super::ConflictAnalysisNogoodContext;
use crate::basic_types::moving_averages::MovingAverage;
use crate::engine::predicates::predicate::Predicate;
use crate::pumpkin_assert_moderate;

#[derive(Clone, Copy, Debug, Default)]
pub struct ResolutionNogoodConflictAnalyser {}

#[derive(Clone, Debug)]
pub struct LearnedNogood {
    pub predicates: Vec<Predicate>,
    pub backjump_level: usize,
}

impl ResolutionNogoodConflictAnalyser {
    // Computes the nogood according to the 1UIP scheme.
    // The asserting nogood is at position [0], and the second decision level positioned predicate
    // is at position [1].
    pub fn compute_1uip(&mut self, context: &mut ConflictAnalysisNogoodContext) -> LearnedNogood {
        // temporary functions that will eventually be removed
        // using these to not change the structure of the solver too much in the early stages of
        // this implementation
        let int_pred = |predicate: &Predicate| {
            match predicate {
            Predicate::IntegerPredicate(integer_predicate) => *integer_predicate,
            Predicate::Literal(_) => unreachable!("I think we should never get a literal in the nogood? Unless we use reified literals...ok for now."),
            Predicate::False => unreachable!(),
            Predicate::True => unreachable!(),
        }
        };

        let is_nogood_propagating = |nogood: &Vec<Predicate>| {
            let num_predicates_at_current_decision_level = nogood
                .iter()
                .filter(|predicate| {
                    context
                        .assignments_integer
                        .get_decision_level_for_predicate(&int_pred(predicate))
                        .expect("Nogood predicate must have a decision level.")
                        == context.assignments_integer.get_decision_level()
                })
                .count();
            assert!(num_predicates_at_current_decision_level > 0);
            num_predicates_at_current_decision_level == 1
        };
        // the initial learned nogood is the conflict nogood
        // and below it gets processed during the analysis
        let mut learned_nogood = context.get_conflict_nogood();
        pumpkin_assert_moderate!(is_nogood_propagating(&learned_nogood));
        // record the nogood size for statistical purposes
        context
            .counters
            .average_conflict_size
            .add_term(learned_nogood.len() as u64);

        while !is_nogood_propagating(&learned_nogood) {
            // take the predicate last assigned on the trail
            let next_predicate = int_pred(
                learned_nogood
                    .iter()
                    .min_by_key(|predicate| {
                        context
                            .assignments_integer
                            .get_trail_position(&int_pred(predicate))
                            .unwrap()
                    })
                    .expect("Cannot have an empty nogood during analysis."),
            );
            // todo: we are not doing lazy,
            // so just ask for the reason based on the trail entry
            let reason = context.get_propagation_reason(&next_predicate);
            // todo: bump the LBD and activity of the reason
            // replace the next_predicate with its reason
            let next_predicate_index = learned_nogood
                .iter()
                .position(|predicate| int_pred(predicate) == next_predicate)
                .expect("Must be able to find the predicate again.");
            let _ = learned_nogood.swap_remove(next_predicate_index);
            // replace the next_predicate by its reason
            for predicate in reason {
                // todo: do semantic minimisation - not strictly necessary
                if !learned_nogood.contains(&predicate) {
                    learned_nogood.push(predicate);
                }
            }
        }
        // todo: clause minimisation?

        // todo: can be done more efficiently
        learned_nogood.sort_by_key(|predicate| {
            context
                .assignments_integer
                .get_trail_position(&int_pred(predicate))
        });
        let backjump_level = context
            .assignments_integer
            .get_decision_level_for_predicate(&int_pred(&learned_nogood[0]))
            .unwrap();

        // sanity check
        pumpkin_assert_moderate!(
            context.assignments_integer.get_decision_level() == backjump_level
        );

        LearnedNogood {
            predicates: learned_nogood,
            backjump_level,
        }
    }
}
