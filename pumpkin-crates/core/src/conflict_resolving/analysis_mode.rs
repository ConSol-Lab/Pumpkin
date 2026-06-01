use std::collections::hash_map::Entry;

use itertools::Itertools;

use crate::basic_types::PredicateId;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::containers::HashMap;
use crate::containers::KeyValueHeap;
use crate::predicates::Predicate;
use crate::predicates::PredicateIdGenerator;
use crate::predicates::PredicateType;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

/// Determines the different type of resolution-based analysis modes that are supported.
#[derive(Debug, Clone, Copy)]
pub enum AnalysisMode {
    /// Standard conflict analysis which returns as soon as the first unit implication point is
    /// found (i.e. when a nogood is created which only contains a single predicate from the
    /// current decision level).
    OneUIP,
    /// An alternative to 1-UIP which stops as soon as the learned nogood only creates decision
    /// predicates.
    AllDecision,
    /// Learns CPIP nogoods (i.e., nogoods which only have predicates from the current decision
    /// level which reason over a single variable when learning).
    CPIP,
    /// Learns CPIP nogoods but rather than stopping at the first point where extended nogood
    /// propagation can take place, it stops when extended nogood propagation can adjust a bound
    /// upon learning.
    BoundsCPIP,
}

impl AnalysisMode {
    /// Returns whether the provided [`Predicate`] (which became true at `decision_level`) should
    /// be processed further.
    ///
    /// If false is returned, then the provided [`Predicate`] is added directly to the nogood.
    pub fn predicate_should_be_processed(
        &self,
        predicate: Predicate,
        decision_level: usize,
        context: &ConflictAnalysisContext,
    ) -> bool {
        match self {
            AnalysisMode::OneUIP | AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                // The predicate should be processed further if it is not from the current decision
                // level
                decision_level == context.get_checkpoint()
            }
            AnalysisMode::AllDecision => {
                // The predicate should be processed further if it is not a decision
                !context.is_decision_predicate(predicate)
            }
        }
    }

    /// Returns whether to continue resolving.
    pub fn should_continue_resolving(
        &self,
        to_process_heap: &KeyValueHeap<PredicateId, u32>,
        predicate_id_generator: &mut PredicateIdGenerator,
        unique_variable_helper: &mut HashMap<DomainId, u32>,
    ) -> bool {
        match self {
            AnalysisMode::OneUIP => {
                // We wait until there is only a single element from the current decision level
                // left.
                to_process_heap.num_nonremoved_elements() > 1
            }
            AnalysisMode::AllDecision => {
                // We wait until there are only decisions left.
                to_process_heap.num_nonremoved_elements() > 0
            }
            AnalysisMode::CPIP => {
                // We wait until there are only elements over a single variable left.
                unique_variable_helper.len() > 1
            }
            AnalysisMode::BoundsCPIP => {
                // We wait until extended nogood propagation can propagate a bound.
                //
                // Firstly, there should be only elements over a single element.
                // Secondly, one of the following should hold:
                // - There is a lower-bound present but no upper-bound OR there is an upper-bound
                //   present but no lower-bound
                // - There are only holes present
                // - There is an equality present (would necessarily lead to a single predicate due
                //   to semantic minimisation)
                let present_domain_ids = to_process_heap
                    .keys()
                    .map(|predicate_id| {
                        predicate_id_generator
                            .get_predicate(predicate_id)
                            .get_domain()
                    })
                    .unique()
                    .collect::<Vec<_>>();
                if present_domain_ids.len() > 1 {
                    true
                } else {
                    // We calculate the number of predicate types from the current decision
                    // level (note that they are necessarily over a
                    // single variable) to determine when bound
                    // propagation can take place.
                    let (mut lower_bounds, mut upper_bounds, mut _disequalities, mut equalities) =
                        (0, 0, 0, 0);
                    for predicate_id in to_process_heap.keys() {
                        let predicate = predicate_id_generator.get_predicate(predicate_id);
                        match predicate.get_predicate_type() {
                            PredicateType::LowerBound => lower_bounds += 1,
                            PredicateType::NotEqual => _disequalities += 1,
                            PredicateType::Equal => equalities += 1,
                            PredicateType::UpperBound => upper_bounds += 1,
                        }
                    }
                    // We return true if we cannot propagate any bounds
                    //
                    // We can propagate bounds in the following situations:
                    // - There is a lower-bound present but no upper-bound OR there is an
                    //   upper-bound present but no lower-bound
                    // - There are only holes present
                    // - There is an equality present (would necessarily lead to a single
                    // predicate due to semantic minimisation)
                    !((lower_bounds > 0 && upper_bounds == 0)
                        || (lower_bounds == 0 && upper_bounds > 0)
                        || (lower_bounds == 0 && upper_bounds == 0)
                        || equalities > 0)
                }
            }
        }
    }

    /// Removes the left-over predicates in `to_process_heap` after
    /// [`AnalysisMode::should_continue_resolving`] returned false (e.g., when finding the 1UIP, the
    /// `to_process_heap` will contain the asserting predicate) and returns the number of
    /// elements which were left in the `to_process_heap`.
    pub fn remove_final_predicates(
        &self,
        to_process_heap: &mut KeyValueHeap<PredicateId, u32>,
        predicate_id_generator: &mut PredicateIdGenerator,
        processed_nogood_predicates: &mut Vec<Predicate>,
    ) -> usize {
        let num_removed = to_process_heap.num_nonremoved_elements();

        match self {
            AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                // When using extended UIP, we need to ensure that all of the remaining predicates
                // are added to the domain.
                pumpkin_assert_simple!(
                    to_process_heap.num_nonremoved_elements() > 0,
                    "There should be at least one element in the final nogood"
                );
                pumpkin_assert_moderate!(
                    to_process_heap
                        .keys()
                        .map(|predicate_id| predicate_id_generator
                            .get_predicate(predicate_id)
                            .get_domain())
                        .unique()
                        .count()
                        == 1,
                    "There should be only one variable in the final nogood from teh current decision level"
                );

                let propagating_domain = predicate_id_generator
                    .get_predicate(*to_process_heap.peek_max().unwrap().0)
                    .get_domain();

                // We need to add all of the remaining predicates to the nogood; due to the way in
                // which the extended UIP is calculated, this could be multiple elements.
                while to_process_heap.num_nonremoved_elements() > 0 {
                    let predicate = Self::pop_predicate_from_conflict_nogood(
                        to_process_heap,
                        predicate_id_generator,
                    );
                    pumpkin_assert_simple!(predicate.get_domain() == propagating_domain);
                    processed_nogood_predicates.push(predicate);
                }
            }
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {
                if to_process_heap.num_nonremoved_elements() > 0 {
                    let last_predicate = Self::pop_predicate_from_conflict_nogood(
                        to_process_heap,
                        predicate_id_generator,
                    );
                    processed_nogood_predicates.push(last_predicate);
                } else {
                    pumpkin_assert_simple!(
                        matches!(self, AnalysisMode::AllDecision),
                        "If the heap is empty when extracting the final nogood then we should be performing all decision learning"
                    )
                }
            }
        }

        num_removed
    }

    /// Removes the element with the highest value from `to_process_heap` and returns the
    /// corresponding [`Predicate`].
    pub fn pop_predicate_from_conflict_nogood(
        to_process_heap: &mut KeyValueHeap<PredicateId, u32>,
        predicate_id_generator: &mut PredicateIdGenerator,
    ) -> Predicate {
        let next_predicate_id = to_process_heap.pop_max().unwrap();
        predicate_id_generator.get_predicate(next_predicate_id)
    }

    /// Whether the analysis mode learns CPIP nogoods.
    pub fn uses_cpip(&self) -> bool {
        matches!(self, AnalysisMode::CPIP | AnalysisMode::BoundsCPIP)
    }

    pub fn add_predicate_to_nogood(
        &self,
        predicate: Predicate,
        unique_variable_helper: &mut HashMap<DomainId, u32>,
    ) {
        match self {
            AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                let entry = unique_variable_helper
                    .entry(predicate.get_domain())
                    .or_default();

                *entry += 1
            }
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {}
        }
    }

    pub fn remove_predicate_from_nogood(
        &self,
        predicate: Predicate,
        unique_variable_helper: &mut HashMap<DomainId, u32>,
    ) {
        match self {
            AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                let entry = unique_variable_helper.entry(predicate.get_domain());

                match entry {
                    Entry::Occupied(mut occupied_entry) => {
                        let value = occupied_entry.get_mut();
                        pumpkin_assert_simple!(*value > 0);
                        if *value == 1 {
                            let _ = occupied_entry.remove();
                        } else {
                            *value -= 1;
                        }
                    }
                    Entry::Vacant(_) => {
                        panic!("Whne removing a predicate from a nogood, it should exist.")
                    }
                }
            }
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {}
        }
    }
}
