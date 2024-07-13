use super::ConflictAnalysisNogoodContext;
use crate::basic_types::moving_averages::MovingAverage;
use crate::engine::conflict_analysis::advanced_nogood::AdvancedNogood;
use crate::engine::predicates::integer_predicate::IntegerPredicate;

#[derive(Clone, Copy, Debug, Default)]
pub struct ResolutionNogoodConflictAnalyser {}

#[derive(Clone, Debug)]
pub struct LearnedNogood {
    pub predicates: Vec<IntegerPredicate>,
    pub backjump_level: usize,
}

impl ResolutionNogoodConflictAnalyser {
    /// Computes the learned nogood according to the 1UIP scheme.
    /// The asserting predicate is at position zero, and the second decision level positioned
    /// predicate is at position one.
    pub fn compute_1uip(&mut self, context: &mut ConflictAnalysisNogoodContext) -> LearnedNogood {
        let mut nogood = AdvancedNogood::new(context.assignments.get_decision_level());

        let conflict_nogood = context.get_conflict_nogood();
        // record the nogood size for statistical purposes
        context
            .counters
            .average_conflict_size
            .add_term(conflict_nogood.len() as u64);

        // println!("before conflict: {:?}", conflict_nogood);

        // Initialise the nogood with the conflict nogood.
        nogood.add_predicates(conflict_nogood, context);

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
        //
        // println!("conflict: {:?}", nogood.predicates);

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
            nogood.add_predicates(reason, context);
            // println!("nogood: {:?}", nogood.predicates);
        }
        // todo: clause minimisation?
        nogood.extract_final_learned_nogood()
    }
}
