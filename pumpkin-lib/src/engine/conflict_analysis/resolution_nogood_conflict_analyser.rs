use std::cmp;

use super::ConflictAnalysisNogoodContext;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::HashMap;
use crate::basic_types::HashSet;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_moderate;

#[derive(Clone, Copy, Debug, Default)]
pub struct ResolutionNogoodConflictAnalyser {}

#[derive(Clone, Debug)]
pub struct LearnedNogood {
    pub predicates: Vec<IntegerPredicate>,
    pub backjump_level: usize,
}

impl ResolutionNogoodConflictAnalyser {
    // General comment:
    // Much of the code below is implemented with simplicity in mind rather than efficiency.
    // If this turns out to work fine, then we could look into addressing the todos.
    // It could also very well be the case that this inefficiency is very minor.

    // Computes the nogood according to the 1UIP scheme.
    // The asserting nogood is at position [0], and the second decision level positioned predicate
    // is at position [1].
    pub fn compute_1uip(&mut self, context: &mut ConflictAnalysisNogoodContext) -> LearnedNogood {
        let num_pred_cur_dl = |nogood: &Vec<IntegerPredicate>| {
            nogood
                .iter()
                .filter(|predicate| {
                    context
                        .assignments_integer
                        .get_decision_level_for_predicate(predicate)
                        .expect("Nogood predicate must have a decision level.")
                        == context.assignments_integer.get_decision_level()
                })
                .count()
        };
        // Currently we need to use this minimiser. The problem stems from the fact that when we
        // make assignments, we make them as two updates: one for the lower bound, and one
        // for the upper bound. This means that we may have _two_ decision predicates at the given
        // level: this is a problem for the learning stopping criteria, because it stops when only
        // one predicate from the current decision level, but when we can make these
        // compound decisions, then this does not work with that criteria.
        // So as a work around we do this minimisation to merge two predicate literals into one, and
        // only do it for the current decision level if there are two predicates left.
        let semantic_minimiser_draft = |nogood: &Vec<IntegerPredicate>| -> Vec<IntegerPredicate> {
            // the idea is to collect all the bounds present,
            // then replace predicates if the bound meet.
            let mut lbs: HashMap<DomainId, i32> = HashMap::default();
            let mut ubs: HashMap<DomainId, i32> = HashMap::default();
            // first collect bounds
            for predicate in nogood {
                match predicate {
                    IntegerPredicate::LowerBound {
                        domain_id,
                        lower_bound: predicate_bound,
                    } => {
                        if let Some(stored_bound) = lbs.get_mut(domain_id) {
                            let better_bound = cmp::max(*predicate_bound, *stored_bound);
                            *stored_bound = better_bound;
                        } else {
                            let _ = lbs.insert(*domain_id, *predicate_bound);
                        }
                    }
                    IntegerPredicate::UpperBound {
                        domain_id,
                        upper_bound: predicate_bound,
                    } => {
                        if let Some(stored_bound) = ubs.get_mut(domain_id) {
                            let better_bound = cmp::min(*predicate_bound, *stored_bound);
                            *stored_bound = better_bound;
                        } else {
                            let _ = ubs.insert(*domain_id, *predicate_bound);
                        }
                    }
                    IntegerPredicate::NotEqual {
                        domain_id: _,
                        not_equal_constant: _,
                    } => { // do nothing
                    }
                    IntegerPredicate::Equal {
                        domain_id,
                        equality_constant,
                    } => {
                        if let Some(stored_bound) = lbs.get_mut(domain_id) {
                            assert!(*stored_bound <= *equality_constant);
                            *stored_bound = *equality_constant;
                        } else {
                            let _ = lbs.insert(*domain_id, *equality_constant);
                        }

                        if let Some(stored_bound) = ubs.get_mut(domain_id) {
                            assert!(*stored_bound >= *equality_constant);
                            *stored_bound = *equality_constant;
                        } else {
                            let _ = ubs.insert(*domain_id, *equality_constant);
                        }
                    }
                }
            }

            let is_assigned = |predicate: &IntegerPredicate| -> bool {
                let d = predicate.get_domain();
                if let Some(lb) = lbs.get(&d) {
                    if let Some(ub) = ubs.get(&d) {
                        if *lb == *ub {
                            return true;
                        }
                    }
                }
                false
            };
            // now rewrite the predicates
            let mut already_added_equality: HashSet<DomainId> = HashSet::default();
            let mut new_nogood: Vec<IntegerPredicate> = vec![];
            for predicate in nogood {
                match predicate {
                    IntegerPredicate::LowerBound {
                        domain_id,
                        lower_bound: _,
                    } => {
                        if is_assigned(predicate) {
                            // do nothing
                            // the upper bound predicate will add the equality predicate
                        }
                        // otherwise the predicate is not fixed, so add the better version of it
                        else if lbs.contains_key(domain_id) {
                            new_nogood.push(IntegerPredicate::LowerBound {
                                domain_id: *domain_id,
                                lower_bound: *lbs.get(domain_id).unwrap(),
                            })
                        } else {
                            new_nogood.push(*predicate);
                        }
                    }
                    IntegerPredicate::UpperBound {
                        domain_id,
                        upper_bound: _,
                    } => {
                        if is_assigned(predicate) {
                            if !already_added_equality.contains(domain_id) {
                                new_nogood.push(IntegerPredicate::Equal {
                                    domain_id: *domain_id,
                                    equality_constant: *ubs.get(domain_id).unwrap(),
                                });
                                let _ = already_added_equality.insert(*domain_id);
                            }
                        }
                        // otherwise the predicate is not fixed, so add the better version of it
                        else if ubs.contains_key(domain_id) {
                            new_nogood.push(IntegerPredicate::UpperBound {
                                domain_id: *domain_id,
                                upper_bound: *ubs.get(domain_id).unwrap(),
                            })
                        } else {
                            new_nogood.push(*predicate);
                        }
                    }
                    IntegerPredicate::NotEqual {
                        domain_id,
                        not_equal_constant,
                    } => {
                        // only add the not equal if it is within the bounds
                        // todo: now we do not remove corner values...
                        let mut should_add = true;
                        if let Some(lb) = lbs.get(domain_id) {
                            if lb > not_equal_constant {
                                should_add = false;
                            }
                        }

                        if let Some(ub) = ubs.get(domain_id) {
                            if ub < not_equal_constant {
                                should_add = false;
                            }
                        }

                        if should_add {
                            new_nogood.push(*predicate);
                        }
                    }
                    IntegerPredicate::Equal {
                        domain_id,
                        equality_constant: _,
                    } => {
                        if !already_added_equality.contains(domain_id) {
                            new_nogood.push(*predicate);
                            let _ = already_added_equality.insert(*domain_id);
                        }
                    }
                }
            }
            new_nogood
        };
        // todo: could be done more efficiently with additional data structures.
        let is_nogood_propagating = |nogood: &Vec<IntegerPredicate>| {
            let num_predicates_at_current_decision_level = num_pred_cur_dl(nogood);
            assert!(num_predicates_at_current_decision_level > 0);
            num_predicates_at_current_decision_level == 1
        };
        // the initial learned nogood is the conflict nogood
        // and below it gets processed during the analysis
        let mut learned_nogood = context.get_conflict_nogood();
        pumpkin_assert_moderate!(!is_nogood_propagating(&learned_nogood));

        // println!("Conflict nogood: {:?}", learned_nogood);

        learned_nogood = semantic_minimiser_draft(&learned_nogood);

        // println!("Conflict nogood minimised: {:?}", learned_nogood);

        // println!("num curr: {}", num_pred_cur_dl(&learned_nogood));

        // for pred in &learned_nogood {
        // println!(
        // "\t{} {} {}",
        // pred,
        // context
        // .assignments_integer
        // .get_decision_level_for_predicate(pred)
        // .unwrap(),
        // context
        // .assignments_integer
        // .get_trail_position(pred)
        // .unwrap()
        // );
        //}

        // record the nogood size for statistical purposes
        context
            .counters
            .average_conflict_size
            .add_term(learned_nogood.len() as u64);

        // Keep refining the nogood until it propagates!
        while !is_nogood_propagating(&learned_nogood) {
            // take the predicate last assigned on the trail
            let next_predicate = *learned_nogood
                .iter()
                .max_by_key(|predicate| {
                    context
                        .assignments_integer
                        .get_trail_position(predicate)
                        .unwrap()
                })
                .expect("Cannot have an empty nogood during analysis.");
            // println!("Next pred: {}", next_predicate);
            // Replace the next_predicate with its reason. This is done in two steps:
            // 1) Remove the predicate from the nogood.
            let next_predicate_index = learned_nogood
                .iter()
                .position(|predicate| *predicate == next_predicate)
                .expect("Must be able to find the predicate again.");
            let _ = learned_nogood.swap_remove(next_predicate_index);
            // 2) Add the reason of the next_predicate to the nogood.
            let reason = context.get_propagation_reason(&next_predicate);
            // println!("Reason: {:?}", reason);
            for predicate in reason {
                // todo: do semantic minimisation - not strictly necessary for the first version
                if !learned_nogood.contains(&predicate) {
                    learned_nogood.push(predicate);
                }
            }
            // println!("\t resulting nogood: {:?}", learned_nogood);
            learned_nogood = semantic_minimiser_draft(&learned_nogood);
            // println!("\t after min: {:?}", learned_nogood);
        }
        // todo: clause minimisation?

        // todo: can be done more efficiently
        learned_nogood
            .sort_by_key(|predicate| context.assignments_integer.get_trail_position(predicate));
        learned_nogood.reverse();

        // sanity check
        pumpkin_assert_moderate!(
            context.assignments_integer.get_decision_level()
                == context
                    .assignments_integer
                    .get_decision_level_for_predicate(&learned_nogood[0])
                    .unwrap()
        );

        let backjump_level = if learned_nogood.len() > 1 {
            context
                .assignments_integer
                .get_decision_level_for_predicate(&learned_nogood[1])
                .unwrap()
        } else {
            0
        };

        LearnedNogood {
            predicates: learned_nogood,
            backjump_level,
        }
    }
}
