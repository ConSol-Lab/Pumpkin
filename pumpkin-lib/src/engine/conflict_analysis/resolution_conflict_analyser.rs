use super::ConflictAnalysisNogoodContext;
use crate::basic_types::moving_averages::MovingAverage;
use crate::engine::conflict_analysis::advanced_nogood::AdvancedNogood;
use crate::engine::predicates::predicate::Predicate;
use crate::pumpkin_assert_advanced;

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct ResolutionNogoodConflictAnalyser {}

#[derive(Clone, Debug)]
pub(crate) struct LearnedNogood {
    pub(crate) predicates: Vec<Predicate>,
    pub(crate) backjump_level: usize,
}

impl ResolutionNogoodConflictAnalyser {
    /// Computes the learned nogood according to the 1UIP scheme.
    /// The asserting predicate is at position zero, and the second decision level positioned
    /// predicate is at position one.
    pub(crate) fn compute_1uip(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
    ) -> LearnedNogood {
        let mut nogood = AdvancedNogood::new(context.assignments.get_decision_level());

        let conflict_nogood = context.get_conflict_nogood();
        pumpkin_assert_advanced!(conflict_nogood
            .iter()
            .all(|p| context.assignments.is_predicate_satisfied(*p)));

        // record the nogood size for statistical purposes
        context
            .counters
            .average_conflict_size
            .add_term(conflict_nogood.len() as u64);

        // println!("before conflict: {:?}", conflict_nogood);

        // Initialise the nogood with the conflict nogood.
        nogood.add_predicates(conflict_nogood, context.assignments, Some(context.brancher));

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
        // println!("TRAIL");
        // for t in context.assignments.trail.iter() {
        // println!("\t{} {}", t.predicate, t.reason.is_none());
        // }

        // Keep refining the nogood until it propagates.
        while !nogood.is_nogood_propagating() {
            // Replace the predicate from the nogood that has been assigned last on the trail.
            // This is done in two steps:
            // 1) Pop the predicate last assigned on the trail from the nogood.
            let next_predicate = nogood.pop_highest_trail_predicate().unwrap();
            // println!("Next pred: {}", next_predicate);
            // 2) Add the reason of the next_predicate to the nogood.
            let reason = context.get_propagation_reason(&next_predicate);
            // println!("reason: {:?}", reason);
            nogood.add_predicates(reason, context.assignments, Some(context.brancher));
        }
        // todo: clause minimisation?
        let mut nogood = nogood.extract_final_learned_nogood(context.assignments);
        // Sorting does the trick with placing the correct predicates at the first two positions,
        // however this can be done more efficiently, since we only need the first two positions
        // to be properly sorted.
        nogood.sort_by_key(|p| context.assignments.get_trail_position(p));
        nogood.reverse();

        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if nogood.len() > 1 {
            context
                .assignments
                .get_decision_level_for_predicate(&nogood[1])
                .unwrap()
        }
        // For unit nogoods, the solver backtracks to the root level.
        else {
            0
        };

        pumpkin_assert_advanced!(nogood[1..]
            .iter()
            .all(|p| context.assignments.is_predicate_satisfied(*p)));

        LearnedNogood {
            backjump_level,
            predicates: nogood,
        }
    }
}
