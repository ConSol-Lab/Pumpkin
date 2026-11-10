use std::collections::hash_map::Entry;

use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashMap;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateIdGenerator;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::DomainId;

use crate::resolvers::WorkingNogood;
use crate::resolvers::working_nogood::get_heap_value;

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
    /// Learns CPIP nogoods \[1\] (i.e., nogoods which only have predicates from the current
    /// decision level which reason over a single variable when learning).
    ///
    /// # Bibliography
    /// - \[1\] I. Marijnissen, M. Flippo, and E. Demirović, ‘From Literals to Atomic Constraints:
    ///   Generalising Conflict-Driven Clause Learning for Constraint Programming’, in 32nd
    ///   International Conference on Principles and Practice of Constraint Programming (CP 2026),
    ///   2026, vol. 379, p. 42:1-42:21.
    CPIP,
    /// Learns CPIP nogoods \[1\] but rather than stopping at the first point where extended nogood
    /// propagation can take place, it stops when extended nogood propagation can adjust a bound
    /// upon learning.
    ///
    /// # Bibliography
    /// - \[1\] I. Marijnissen, M. Flippo, and E. Demirović, ‘From Literals to Atomic Constraints:
    ///   Generalising Conflict-Driven Clause Learning for Constraint Programming’, in 32nd
    ///   International Conference on Principles and Practice of Constraint Programming (CP 2026),
    ///   2026, vol. 379, p. 42:1-42:21.
    BoundsCPIP,
}

impl AnalysisMode {
    /// Returns whether the provided [`Predicate`] (which became true at `decision_level`) should
    /// be processed further.
    ///
    /// If false is returned, then the provided [`Predicate`] is added directly to the nogood.
    pub(crate) fn predicate_should_be_processed(
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
    pub(crate) fn should_continue_resolving(
        &self,
        predicate_id_generator: &PredicateIdGenerator,
        working_nogood: &WorkingNogood,
    ) -> bool {
        match self {
            AnalysisMode::OneUIP => {
                // We wait until there is only a single element from the current decision level
                // left.
                working_nogood.num_current_checkpoint() > 1
            }
            AnalysisMode::AllDecision => {
                // We wait until there are only decisions left.
                working_nogood.num_current_checkpoint() > 0
            }
            AnalysisMode::CPIP => {
                // We wait until there are only elements over a single variable left.
                working_nogood.num_unique_variables_current_checkpoint() > 1
            }
            AnalysisMode::BoundsCPIP => {
                working_nogood.num_unique_variables_current_checkpoint() > 1
                    || !self.stops_with(
                        working_nogood
                            .predicate_ids_current_checkpoint()
                            .map(|predicate_id| predicate_id_generator.get_predicate(predicate_id)),
                    )
            }
        }
    }

    /// Returns whether `element` can be replaced with `new_predicate` (which are over the same
    /// variable) in the [`WorkingNogood`].
    ///
    /// A replacement is not possible if `new_predicate` would be the next element to be resolved
    /// upon, since resolving upon it could lead to `element` being added again (leading to a
    /// loop). For example, replacing `[x >= 5] /\ [x != 5]` with `[x >= 6]` and then resolving
    /// upon `[x >= 6]` would add its reason `[x >= 5] /\ [x != 5]` back to the nogood.
    ///
    /// The exception is CPIP-based analysis which stops right after the replacement; then
    /// `new_predicate` is never resolved upon, so the replacement is always possible.
    pub(crate) fn can_replace(
        &self,
        element: Predicate,
        new_predicate: Predicate,
        context: &ConflictAnalysisContext,
        predicate_id_generator: &PredicateIdGenerator,
        working_nogood: &WorkingNogood,
    ) -> bool {
        // We first calculate the value in the heap of the new_predicate, and retrieve the
        // predicate which is currently the next to be resolved upon (i.e., the maximum in the
        // heap).
        let heap_value = get_heap_value(new_predicate, context);
        let max = working_nogood.peek_max_current_checkpoint();

        // Then we check whether `new_predicate` would *not* be the next predicate to be resolved
        // upon after the replacement; this is the case if one of the following holds:
        // 1. It has a lower value than the current maximum in the heap; another predicate is
        //    resolved upon first.
        // 2. There are no predicates from the current checkpoint; after the replacement,
        //    `new_predicate` is the only predicate from the current checkpoint (i.e., the asserting
        //    predicate when using 1UIP or CPIP-based analysis).
        // 3. The only predicate from the current checkpoint is `element` itself; similar to the
        //    previous case, `new_predicate` then replaces it as the only predicate from the current
        //    checkpoint.
        let not_next_resolved = heap_value < max.map(|(_, value)| value).unwrap_or_default()
            || working_nogood.num_current_checkpoint() == 0
            || (working_nogood.num_current_checkpoint() == 1
                && max.is_some_and(|(predicate_id, _)| {
                    predicate_id_generator.get_predicate(predicate_id) == element
                }));
        pumpkin_assert_moderate!(
            element.get_domain() == new_predicate.get_domain(),
            "Expected {element} and {new_predicate} to be over the same variable"
        );

        match self {
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {
                // The stopping criteria of these analysis modes do not depend on the variables of
                // the predicates; so, it only depends on whether `new_predicate` would be the next
                // predicate to be resolved upon.
                not_next_resolved
            }
            AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                // If `new_predicate` would not be the next predicate to be resolved upon, then we
                // can replace `element`.
                if not_next_resolved {
                    return true;
                }

                // Otherwise, we can only replace `element` if the analysis stops right after the
                // replacement.
                //
                // CPIP-based analysis only stops if all predicates from the current checkpoint
                // are over a single variable. Since `element` and `new_predicate` are over the
                // same variable, this can only hold after the replacement if all predicates from
                // the current checkpoint are already over the variable of `new_predicate` (note
                // that `element` might be from a previous checkpoint).
                if !working_nogood
                    .unique_variables_current_checkpoint()
                    .all(|domain| domain == new_predicate.get_domain())
                {
                    return false;
                }

                pumpkin_assert_moderate!(
                    working_nogood
                        .predicate_ids_current_checkpoint()
                        .all(|predicate_id| predicate_id_generator
                            .get_predicate(predicate_id)
                            .get_domain()
                            == new_predicate.get_domain()),
                    "Expected the predicates from the current checkpoint to be over the same variable as {new_predicate}"
                );

                // Finally, we check whether the stopping criterion of the analysis mode holds for
                // the predicates from the current checkpoint after the replacement; i.e., without
                // `element` and with `new_predicate`.
                //
                // Note that adding `new_predicate` could still lead to further predicates over the
                // same variable being removed by the minimiser, which is not taken into account.
                let predicates_after_replacement = working_nogood
                    .predicate_ids_current_checkpoint()
                    .map(|predicate_id| predicate_id_generator.get_predicate(predicate_id))
                    .filter(|&predicate| predicate != element)
                    .chain([new_predicate]);
                self.stops_with(predicates_after_replacement)
            }
        }
    }

    /// Returns whether CPIP-based analysis stops if `predicates` are the predicates from the
    /// current checkpoint in the nogood, assuming that they are over a single variable.
    ///
    /// Should only be called for CPIP-based analysis modes.
    fn stops_with(&self, predicates: impl IntoIterator<Item = Predicate>) -> bool {
        match self {
            AnalysisMode::BoundsCPIP => {
                // We wait until extended nogood propagation can propagate a bound.
                //
                // We calculate the number of predicate types from the current decision level (note
                // that they are necessarily over a single variable) to determine when bound
                // propagation can take place.
                let (mut lower_bounds, mut upper_bounds, mut equalities) = (0, 0, 0);
                let mut domain = None;
                for predicate in predicates {
                    pumpkin_assert_moderate!(
                        *domain.get_or_insert(predicate.get_domain()) == predicate.get_domain(),
                        "Expected the predicates to be over a single variable"
                    );
                    match predicate.get_predicate_type() {
                        PredicateType::LowerBound => lower_bounds += 1,
                        PredicateType::NotEqual => {}
                        PredicateType::Equal => equalities += 1,
                        PredicateType::UpperBound => upper_bounds += 1,
                    }
                }
                // We can propagate bounds in the following situations:
                // - There is a lower-bound present but no upper-bound OR there is an upper-bound
                //   present but no lower-bound
                // - There are only holes present
                // - There is an equality present (would necessarily lead to a single predicate due
                //   to semantic minimisation)
                (lower_bounds > 0 && upper_bounds == 0)
                    || (lower_bounds == 0 && upper_bounds > 0)
                    || (lower_bounds == 0 && upper_bounds == 0)
                    || equalities > 0
            }
            // CPIP stops when there are only elements over a single variable left.
            AnalysisMode::CPIP => true,
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {
                unreachable!("Only CPIP-based analysis modes can stop based on the predicates")
            }
        }
    }

    /// Whether the analysis mode learns CPIP nogoods.
    pub(crate) fn uses_cpip(&self) -> bool {
        matches!(self, AnalysisMode::CPIP | AnalysisMode::BoundsCPIP)
    }

    /// Called whenever a [`Predicate`] is added to the nogood by the resolution resolver.
    ///
    /// A helper is passed which contains how many times a [`DomainId`] appears in the current
    /// nogood.
    pub(crate) fn predicate_added_to_nogood(
        &self,
        predicate: Predicate,
        unique_variable_helper: &mut HashMap<DomainId, u32>,
    ) {
        match self {
            AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                // We get the current count for the domain, or insert it if it does not exist
                let entry = unique_variable_helper
                    .entry(predicate.get_domain())
                    .or_default();

                *entry += 1
            }
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {}
        }
    }

    /// Called whenever a [`Predicate`] is removed from the nogood by the resolution resolver.
    ///
    /// A helper is passed which contains how many times a [`DomainId`] appears in the current
    /// nogood.
    pub(crate) fn predicate_removed_from_nogood(
        &self,
        predicate: Predicate,
        unique_variable_helper: &mut HashMap<DomainId, u32>,
    ) {
        match self {
            AnalysisMode::CPIP | AnalysisMode::BoundsCPIP => {
                // First, we find the entry
                let entry = unique_variable_helper.entry(predicate.get_domain());

                match entry {
                    Entry::Occupied(mut occupied_entry) => {
                        let value = occupied_entry.get_mut();

                        pumpkin_assert_simple!(*value > 0);

                        if *value == 1 {
                            // We remove the entry in its entirety if the count ever reaches 0.
                            let _ = occupied_entry.remove();
                        } else {
                            // Otherwise, we simply reduce the count by 1.
                            *value -= 1;
                        }
                    }
                    Entry::Vacant(_) => {
                        panic!("When removing a predicate from a nogood, it should exist.")
                    }
                }
            }
            AnalysisMode::OneUIP | AnalysisMode::AllDecision => {}
        }
    }
}
