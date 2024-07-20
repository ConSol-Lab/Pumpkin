use super::ConflictAnalysisNogoodContext;
use super::SemanticMinimiser;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::KeyValueHeap;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::StorageKey;
use crate::branching::Brancher;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::Assignments;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;

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
        //println!("start...........");
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
            return;
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

                self.heap_current_decision_level.restore_key(predicate_id);
                self.heap_current_decision_level
                    .increment(predicate_id, trail_position as u32);
                
                if *self.heap_current_decision_level.get_value(predicate_id)
                != trail_position.try_into().unwrap()
                {
                    self.heap_current_decision_level.delete_key(predicate_id);
                }

                // The way we compute reasons, this could happen actually.
                /*pumpkin_assert_moderate!(
                    *self.heap_current_decision_level.get_value(predicate_id)
                        == trail_position as u32,
                    "Can only add the predicate to the heap once."
                );*/
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
        assignments: &Assignments,
        semantic_minimiser: &mut SemanticMinimiser,
        brancher: &mut dyn Brancher,
    ) -> LearnedNogood {
        assert!(self.heap_current_decision_level.num_nonremoved_elements() == 1 || self.heap_current_decision_level.num_nonremoved_elements() == 2);
        // The final nogood is composed of the predicates encountered from the lower decision
        // levels, plus the predicate remaining in the heap.

        // First obtain a semantically minimised nogood.
        // We reuse the vector with lower decision levels for simplicity.
        let last_predicate = self.pop_predicate_from_conflict_nogood();
        self.predicates_lower_decision_level.push(last_predicate);
        let mut clean_nogood: Vec<Predicate> =
            semantic_minimiser.minimise(&self.predicates_lower_decision_level, assignments);

        // todo: clause minimisation based on the implication graph?

        // Sorting does the trick with placing the correct predicates at the first two positions,
        // however this can be done more efficiently, since we only need the first two positions
        // to be properly sorted.
        clean_nogood.sort_by_key(|p| assignments.get_trail_position(p));
        clean_nogood.reverse();
        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if clean_nogood.len() > 1 {
            assignments
                .get_decision_level_for_predicate(&clean_nogood[1])
                .unwrap()
        }
        // For unit nogoods, the solver backtracks to the root level.
        else {
            0
        };
        pumpkin_assert_advanced!(clean_nogood[1..]
            .iter()
            .all(|p| assignments.is_predicate_satisfied(*p)));

        // todo: asserting predicate may be bumped twice, probably not a problem.
        for predicate in clean_nogood.iter() {
            brancher.on_appearance_in_conflict_predicate(*predicate);
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

        /*println!("lower dec lvls: {:?}", self.predicates_lower_decision_level);

        println!("VARIABLE DOMAINS");
        for d in context.assignments.get_domains() {
            println!(
                "{}: [{}, {}]",
                d,
                context.assignments.get_lower_bound(d),
                context.assignments.get_upper_bound(d)
            );
        }

        println!("curr dec lvl: {}", context.assignments.get_decision_level());
        println!("TRAIL");
        for t in context.assignments.trail.iter() {
            println!("\t{} {}", t.predicate, t.reason.is_none());
        }*/

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

            //println!("Next pred: {}", next_predicate);
            // 2) Add the reason of the next_predicate to the nogood.

            // 2.a) Here we treat the special case: if the next predicate is a decision, this means
            // that we are done with analysis since the only remaining predicate in the heap is the
            // other decision.
            if context.assignments.is_decision_predicate(&next_predicate) {
                /*if self.heap_current_decision_level.num_nonremoved_elements() != 1 &&  self.heap_current_decision_level.num_nonremoved_elements() != 2
                {
                    println!("NUM ELEMETNS {}", self.heap_current_decision_level.num_nonremoved_elements());
                    while self.heap_current_decision_level.num_nonremoved_elements() != 0
                    {
                        let poppers = self.heap_current_decision_level.pop_max();
                        println!("\tpopped {}", self.predicate_id_generator.get_predicate(poppers.unwrap()).unwrap());
                    }
                    panic!();
                }*/
                
                //assert!(self.heap_current_decision_level.num_nonremoved_elements() == 1 || self.heap_current_decision_level.num_nonremoved_elements() == 2);
                // As a simple workaround, we add the currently analysed predicate to set of
                // predicates from the lower predicate level, and stop analysis. Semantic
                // minimisation will ensure the bound predicates get converted into an equality
                // decision predicate.
                while self.heap_current_decision_level.num_nonremoved_elements() != 1
                {
                    let p = self.pop_predicate_from_conflict_nogood();
                    self.predicates_lower_decision_level.push(p);
                }

                self.predicates_lower_decision_level.push(next_predicate);
                break;
            }
            // 2.b) Standard case, get the reason for the predicate and add it to the nogood.
            let reason = context.get_propagation_reason_simple(next_predicate);
            //println!("reason: {:?}", reason);
            for predicate in reason.iter() {
                self.add_predicate_to_conflict_nogood(
                    *predicate,
                    context.assignments,
                    context.brancher,
                );
            }
        }
        self.extract_final_nogood(
            context.assignments,
            context.semantic_minimiser,
            context.brancher,
        )
    }
}
