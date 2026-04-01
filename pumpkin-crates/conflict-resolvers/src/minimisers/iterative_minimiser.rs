use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::DomainId;

#[derive(Debug, Clone, Default)]
pub(crate) struct IterativeMinimiser {
    domains: KeyedVec<DomainId, IterativeDomain>,
    replacement_with_equals: bool,
}

/// The result of processing a predicate, indicating its redundancy.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ProcessingResult {
    /// The predicate to process was redundant.
    Redundant,
    /// The predicate to process was not redundant, and it replaced
    /// [`ProcessingResult::ReplacedPresent::removed`].
    ///
    /// e.g., [x >= 5] can replace [x >= 2].
    ReplacedPresent { removed: Predicate },
    /// The predicate to process was replaced with
    /// [`ProcessingResult::PossiblyReplacedWithNew::new_predicate`] and it also removed
    /// [`ProcessingResult::PossiblyReplacedWithNew::removed`].
    ///
    /// Note that it is not always possible to replace with `new_predicate` (since it can lead to
    /// infinite loops), so it is not guaranteed that `new_predicate` is added.
    ///
    /// e.g., if [x >= 5] is in the nogood, and the predicate [x <= 5] is added, then [x >= 5] is
    /// removed and replaced with [x == 5] (and [x <= 5] is not added).
    PossiblyReplacedWithNew {
        removed: Predicate,
        new_predicate: Predicate,
    },
    /// The predicate was found to be not redundant.
    NotRedundant,
}

/// A concise domain representation used by the [`IterativeMinimiser`].
///
/// Note that this does not contain elements which are part of the intial domain, but only part of
/// the nogood.
#[derive(Debug, Clone)]
struct IterativeDomain {
    /// The updates that have occurred to the lower-bound.
    lower_bound_updates: Vec<i32>,
    /// The updates that have occurred to the upper-bound.
    upper_bound_updates: Vec<i32>,
    /// A mapping of a hole in the domain to whether it caused a lower-bound or upper-bound update
    /// respectively.
    holes: HashMap<i32, (bool, bool)>,

    /// An easy way to access the lower-bound.
    lower_bound: i32,
    /// An easy way to access the upper-bound.
    upper_bound: i32,
}

impl Default for IterativeDomain {
    fn default() -> Self {
        Self {
            lower_bound_updates: Default::default(),
            upper_bound_updates: Default::default(),
            holes: Default::default(),
            lower_bound: i32::MIN,
            upper_bound: i32::MAX,
        }
    }
}

impl IterativeDomain {
    /// Applies the given predicate to the state.
    ///
    /// If `replacement` is true, then it indicates that this predicate was added due to a
    /// [`ProcessingResult::PossiblyReplacedWithNew`] event. This influences how the updates to the
    /// bounds are processed.
    pub(crate) fn apply_predicate(&mut self, predicate: Predicate, replacement: bool) {
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                // We update the lower-bound if it larger.
                if predicate.get_right_hand_side() > self.lower_bound {
                    self.lower_bound = predicate.get_right_hand_side();
                    self.lower_bound_updates
                        .push(predicate.get_right_hand_side());
                }
            }
            PredicateType::UpperBound => {
                // We update the upper-bound if it is smaller.
                if predicate.get_right_hand_side() < self.upper_bound {
                    self.upper_bound = predicate.get_right_hand_side();
                    self.upper_bound_updates
                        .push(predicate.get_right_hand_side());
                }
            }
            PredicateType::NotEqual => {
                let mut lb_caused_update = false;
                let mut ub_caused_update = false;

                // We update the lower-bound if it coincides with the right-hand side of the not
                // equals predicate.
                //
                // TODO: Could move bound based on other holes?
                if predicate.get_right_hand_side() == self.lower_bound {
                    lb_caused_update = true;
                    self.lower_bound = predicate.get_right_hand_side() + 1;
                    self.lower_bound_updates
                        .push(predicate.get_right_hand_side() + 1);
                }

                // We update the upper-bound if it coincides with the right-hand side of the not
                // equals predicate.
                //
                // TODO: Could move bound based on other holes?
                if predicate.get_right_hand_side() == self.upper_bound {
                    ub_caused_update = true;
                    self.upper_bound = predicate.get_right_hand_side() - 1;
                    self.upper_bound_updates
                        .push(predicate.get_right_hand_side() - 1);
                }

                // We also insert it into the existing holes
                let _ = self.holes.insert(
                    predicate.get_right_hand_side(),
                    (lb_caused_update, ub_caused_update),
                );
            }
            PredicateType::Equal => {
                // We pop the previous bound if this equality predicate is a replacement of a
                // previous predicate.
                if replacement {
                    if self.lower_bound == predicate.get_right_hand_side() {
                        let lb_u = self.lower_bound_updates.pop();
                        assert_eq!(lb_u, Some(predicate.get_right_hand_side()));
                    } else if self.upper_bound == predicate.get_right_hand_side() {
                        let ub_u = self.upper_bound_updates.pop();
                        assert_eq!(ub_u, Some(predicate.get_right_hand_side()));
                    }
                }

                // We push fresh bound variables due to this predicate.
                self.lower_bound_updates
                    .push(predicate.get_right_hand_side());
                self.upper_bound_updates
                    .push(predicate.get_right_hand_side());

                // And update the lower- and upper-bound.
                self.upper_bound = predicate.get_right_hand_side();
                self.lower_bound = predicate.get_right_hand_side();
            }
        }
    }

    /// Removes the provided predicate from the domain.
    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                // We find the update corresponding to the predicate and remove it.
                let position = self
                    .lower_bound_updates
                    .iter()
                    .position(|value| *value == predicate.get_right_hand_side())
                    .expect("Expected removed lower-bound to be present");

                let _ = self.lower_bound_updates.remove(position);

                // We update the lower-bound to its value or the minimum possible lower-bound.
                self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);

                // Now we need to check whether the lower-bound was caused by an update of a hole.
                //
                // If this is the case, then we need to revert the lower-bound to its value before
                // it was applied.
                if self
                    .holes
                    .get(&predicate.get_right_hand_side())
                    .map(|(lb_updated, _)| *lb_updated)
                    .unwrap_or_default()
                // There is a hole which has the same rhs as the removed predicate, and it was responsible for updating the lower-bound.
                    && let Some(position) = self
                        .lower_bound_updates
                        .iter()
                        .position(|value| *value == predicate.get_right_hand_side() + 1)
                // There exists a lower-bound update which represents the update due to the
                // predicate and a hole in the domain.
                {
                    // We remove the lower-bound which is now not true anymore.
                    let _ = self.lower_bound_updates.remove(position);
                    // And update the lower-bound.
                    self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);
                }
            }
            PredicateType::UpperBound => {
                // We find the update corresponding to the predicate and remove it.
                let position = self
                    .upper_bound_updates
                    .iter()
                    .position(|value| *value == predicate.get_right_hand_side())
                    .unwrap_or_else(|| {
                        panic!(
                            "Expected removed upper-bound {} to be present\n{:?}",
                            predicate.get_right_hand_side(),
                            self.upper_bound_updates
                        )
                    });
                let _ = self.upper_bound_updates.remove(position);

                // We update the lower-bound to its value or the minimum possible lower-bound.
                self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MAX);

                // Now we need to check whether the upper-bound was caused by an update of a hole.
                //
                // If this is the case, then we need to revert the upper-bound to its value before
                // it was applied.
                if self
                    .holes
                    .get(&predicate.get_right_hand_side())
                    .map(|(_, ub_updated)| *ub_updated)
                    .unwrap_or_default()
                // There is a hole which has the same rhs as the removed predicate, and it was responsible for updating the lower-bound.
                    && let Some(position) = self
                        .upper_bound_updates
                        .iter()
                        .position(|value| *value == predicate.get_right_hand_side() - 1)
                // There exists a lower-bound update which represents the update due to the
                // predicate and a hole in the domain.
                {
                    // We remove the upper-bound which is now not true anymore.
                    let _ = self.upper_bound_updates.remove(position);
                    // And update the upper-bound
                    self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MAX);
                }
            }
            PredicateType::NotEqual => {
                let (lb_caused_update, ub_caused_update) = self
                    .holes
                    .get(&predicate.get_right_hand_side())
                    .expect("Expected removed not equals to be present");

                // We remove the updated lower-bound if it is present
                if *lb_caused_update
                    && let Some(position) = self
                        .lower_bound_updates
                        .iter()
                        .position(|value| *value == predicate.get_right_hand_side() + 1)
                {
                    let _ = self.lower_bound_updates.remove(position);
                    self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);
                }

                // We remove the updated upper-bound if it is present
                if *ub_caused_update
                    && let Some(position) = self
                        .upper_bound_updates
                        .iter()
                        .position(|value| *value == predicate.get_right_hand_side() - 1)
                {
                    let _ = self.upper_bound_updates.remove(position);
                    self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MAX);
                }
            }
            PredicateType::Equal => {
                // We remove the last update and reset the bounds
                assert_eq!(self.lower_bound, self.upper_bound);
                assert_eq!(self.lower_bound, predicate.get_right_hand_side());

                let popped_lb = self.lower_bound_updates.pop();
                assert_eq!(popped_lb, Some(predicate.get_right_hand_side()));

                let popped_ub = self.upper_bound_updates.pop();
                assert_eq!(popped_ub, Some(predicate.get_right_hand_side()));

                self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);
                self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MAX);
            }
        }
    }
}

impl IterativeMinimiser {
    /// The [`IterativeMinimiser`] indicated (via [`ProcessingResult::PossiblyReplacedWithNew`])
    /// that a new predicate could be inserted; this method indicates that acutally could not/did
    /// not happen.
    pub(crate) fn did_not_replace(&mut self) {
        self.replacement_with_equals = false;
    }

    /// Removes the given predicate from the nogood.
    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        self.domains[domain].remove_predicate(predicate);
    }

    /// Applies the given predicate from the nogood.
    pub(crate) fn apply_predicate(&mut self, predicate: Predicate) {
        // println!(
        //     "\t\tApplying ({}) {predicate:?}",
        //     self.replacement_with_equals
        // );
        let domain = predicate.get_domain();
        self.domains.accomodate(domain, IterativeDomain::default());
        self.domains[domain].apply_predicate(predicate, self.replacement_with_equals);
    }

    /// Processes the predicate, indicating via [`ProcessingResult`] what can happen to it.
    pub(crate) fn process_predicate(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) -> ProcessingResult {
        let domain = predicate.get_domain();
        self.domains.accomodate(domain, IterativeDomain::default());

        let lower_bound = self.domains[domain].lower_bound;
        let upper_bound = self.domains[domain].upper_bound;

        // First, we log initial bounds which could be responsible for removing a predicate from
        // the nogood.
        //
        // TODO: Should only log when relevant.
        let lower_bound_is_initial_bound =
            context.is_initial_bound(predicate!(domain >= lower_bound));
        let upper_bound_is_initial_bound =
            context.is_initial_bound(predicate!(domain <= upper_bound));
        if lower_bound_is_initial_bound {
            context.explain_root_assignment(predicate!(domain >= lower_bound));
        }
        if upper_bound_is_initial_bound {
            context.explain_root_assignment(predicate!(domain <= upper_bound));
        }
        for hole in self.domains[domain].holes.keys() {
            if context.get_checkpoint_for_predicate(predicate!(domain != *hole)) == Some(0) {
                context.explain_root_assignment(predicate!(domain != *hole));
            }
        }

        // If the domain is assigned, then the added predicate is redundant.
        //
        // Encompasses the rules:
        // - [x = v], [x != v'] => [x = v]
        // - [x = v], [x <= v'] => [x = v]
        // - [x = v], [x >= v'] => [x = v]
        if lower_bound == upper_bound {
            return ProcessingResult::Redundant;
        }

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if predicate.get_right_hand_side() == upper_bound {
                    // [x <= v], [x >= v] => [x = v]
                    if upper_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain <= upper_bound));
                    }
                    self.replacement_with_equals = true;
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain == upper_bound),
                    }
                } else if predicate.get_right_hand_side() > lower_bound {
                    // [x >= v], [x >= v'] => [x >= v'] if v' > v
                    if lower_bound != i32::MIN {
                        ProcessingResult::ReplacedPresent {
                            removed: predicate!(domain >= lower_bound),
                        }
                    } else {
                        ProcessingResult::NotRedundant
                    }
                } else {
                    // [x >= v], [x >= v'] => [x >= v] if v > v'
                    if lower_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain >= lower_bound));
                    }
                    ProcessingResult::Redundant
                }
            }
            PredicateType::UpperBound => {
                // [x >= v], [x <= v] => [x = v]
                if predicate.get_right_hand_side() == lower_bound {
                    if lower_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain >= lower_bound));
                    }
                    self.replacement_with_equals = true;
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain == lower_bound),
                    }
                } else if predicate.get_right_hand_side() < upper_bound {
                    // [x <= v], [x <= v'] => [x <= v'] if v' < v
                    if upper_bound != i32::MAX {
                        ProcessingResult::ReplacedPresent {
                            removed: predicate!(domain <= upper_bound),
                        }
                    } else {
                        ProcessingResult::NotRedundant
                    }
                } else {
                    // [x <= v], [x <= v'] => [x <= v] if v < v'
                    if upper_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain <= upper_bound));
                    }
                    ProcessingResult::Redundant
                }
            }
            PredicateType::NotEqual => {
                if predicate.get_right_hand_side() == upper_bound {
                    // [x <= v], [x != v] => [x <= v - 1]
                    if upper_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain <= upper_bound));
                    }
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain <= upper_bound - 1),
                    }
                } else if predicate.get_right_hand_side() > upper_bound {
                    // [x <= v], [x != v'] => [x <= v] where v' > v
                    if upper_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain <= upper_bound));
                    }
                    ProcessingResult::Redundant
                } else if predicate.get_right_hand_side() == lower_bound {
                    // [x >= v], [x != v] => [x <= v + 1]
                    if lower_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain >= lower_bound));
                    }
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain >= lower_bound + 1),
                    }
                } else if predicate.get_right_hand_side() < lower_bound {
                    // [x >= v], [x != v'] => [x >= v] where v' < v
                    if lower_bound_is_initial_bound {
                        context.explain_root_assignment(predicate!(domain >= lower_bound));
                    }
                    ProcessingResult::Redundant
                } else {
                    ProcessingResult::NotRedundant
                }
            }
            PredicateType::Equal => ProcessingResult::NotRedundant,
        }
    }
}
