use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::DomainId;

/// A minimiser which iteratively applies rewrite rules based on the semantic meaning of predicates
/// *during* conflict analysis.
///
/// The implementation is heavily inspired by \[1\].
///
/// There are a couple of limitations to note:
/// - Currently, whenever a hole is created at a lower- or upper-bound, the bound is only moved by 1
///   (even if there are multiple holes which could cause a higher lower-bound). This simplifies a
///   lot of the logic.
/// - Currently, it is unclear how the rules are applied in \[1\]. This means that certain rules are
///   invalid (for, as of yet, unknown reasons), for example, removing all other elements concerning
///   `x` when `[x = v]` is added leads to wrong solutions.
///   - This leads to another slightly nuanced point; which is that when an element is removed from
///     the nogood, it could be that there are other bounds which should be restored.
///
///     For example, it could be that the nogood contains (among others) the predicates `[x >= v]`
///     and `[x = v + 1]`. In this case, once `[x = v + 1]` gets removed, the lower-bound should be
///     moved back to `v`.
///
/// ## Developer Notes
/// - The predicates from the previous decision level should also be added to the
///   [`IterativeMinimiser`]; this is due to the fact that they are not guaranteed to be
///   semantically minimised away.
///
///   Imagine the situation where we have `[x >= v]` from a previous
///   decision level and `[x >= v']` from the current decision level (where `v' > v`). If we then
///   do not process `[x >= v]`, then it would get added to the nogood directly rather than removed
///   due to redundancy. If we now resolve on [`x >= v'`] and the nogood becomes asserting, then
///   there are no other elements over `x` and the predicate from the previous decision level does
///   not get removed.
///
/// # Bibliography
/// \[1\] T. Feydy, A. Schutt, and P. Stuckey, ‘Semantic learning for lazy clause generation’, in
/// TRICS workshop, held alongside CP, 2013.
#[derive(Debug, Clone, Default)]
pub(crate) struct IterativeMinimiser {
    /// Keeps track of the domains induced by the current working nogood.
    state: VariableState<Predicate>,
    domains: KeyedVec<DomainId, Vec<Predicate>>,
}

/// The result of processing a predicate, indicating its redundancy.
#[derive(Debug, Clone)]
pub(crate) enum ProcessingResult {
    /// The predicate to process was redundant.
    Redundant,
    /// The predicate to process was not redundant, and it replaced
    /// [`ProcessingResult::ReplacedPresent::removed`].
    ///
    /// e.g., [x >= 5] can replace [x >= 2].
    ReplacedPresent { removed: Vec<Predicate> },
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

impl IterativeMinimiser {
    /// Removes the given predicate from the nogood.
    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        if let Some(to_remove_position) = self.domains[domain]
            .iter()
            .position(|element| *element == predicate)
        {
            let _ = self.domains[domain].remove(to_remove_position);
        }
    }

    /// Applies the given predicate from the nogood.
    pub(crate) fn apply_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        self.domains.accomodate(domain, Default::default());
        self.domains[domain].push(predicate)
    }

    /// Explains the lower-bound in the proof log.
    fn explain_lower_bound_in_proof(
        &self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) {
        if context.is_proof_logging_inferences() {
            let domain = predicate.get_domain();

            if let IntExt::Int(lower_bound) = self.state.lower_bound(&domain)
                && context.get_checkpoint_for_predicate(predicate!(domain >= lower_bound))
                    == Some(0)
            {
                context.explain_root_assignment(predicate!(domain >= lower_bound));
            }

            for hole in self.state.holes(&domain) {
                if context.get_checkpoint_for_predicate(predicate!(domain != hole)) == Some(0) {
                    context.explain_root_assignment(predicate!(domain != hole));
                }
            }
        }
    }

    fn explain_upper_bound_in_proof(
        &self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) {
        if context.is_proof_logging_inferences() {
            let domain = predicate.get_domain();

            if let IntExt::Int(upper_bound) = self.state.upper_bound(&domain)
                && context.get_checkpoint_for_predicate(predicate!(domain <= upper_bound))
                    == Some(0)
            {
                context.explain_root_assignment(predicate!(domain <= upper_bound));
            }

            for hole in self.state.holes(&domain) {
                if context.get_checkpoint_for_predicate(predicate!(domain != hole)) == Some(0) {
                    context.explain_root_assignment(predicate!(domain != hole));
                }
            }
        }
    }

    /// Processes the predicate, indicating via [`ProcessingResult`] what can happen to it.
    pub(crate) fn process_predicate(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) -> ProcessingResult {
        let domain = predicate.get_domain();
        if domain.index() >= self.domains.len() || self.domains[domain].is_empty() {
            return ProcessingResult::NotRedundant;
        }
        self.domains.accomodate(domain, Default::default());

        self.state.reset_domain(domain);
        for predicate in self.domains[predicate.get_domain()].iter() {
            let consistent = self.state.apply(predicate);
            assert!(consistent)
        }

        let lower_bound = self
            .state
            .lower_bound(&predicate.get_domain())
            .try_into()
            .unwrap_or(i32::MIN);
        let upper_bound = self
            .state
            .upper_bound(&predicate.get_domain())
            .try_into()
            .unwrap_or(i32::MAX);

        // If the domain is assigned, then the added predicate is redundant.
        //
        // Encompasses the rules:
        // - [x = v], [x != v'] => [x = v]
        // - [x = v], [x <= v'] => [x = v]
        // - [x = v], [x >= v'] => [x = v]
        if lower_bound == upper_bound {
            self.explain_lower_bound_in_proof(predicate, context);
            self.explain_upper_bound_in_proof(predicate, context);
            return ProcessingResult::Redundant;
        }

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if predicate.get_right_hand_side() == upper_bound {
                    self.explain_upper_bound_in_proof(predicate, context);
                    // [x <= v], [x >= v] => [x = v]
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain == upper_bound),
                    }
                } else if predicate.get_right_hand_side() > lower_bound {
                    // [x >= v], [x >= v'] => [x >= v'] if v' > v
                    let to_remove = self.domains[predicate.get_domain()]
                        .iter()
                        .filter(|element| {
                            element.is_lower_bound_predicate()
                                || (element.is_not_equal_predicate()
                                    && element.get_right_hand_side()
                                        < predicate.get_right_hand_side())
                        })
                        .copied()
                        .collect::<Vec<_>>();

                    if !to_remove.is_empty() {
                        ProcessingResult::ReplacedPresent { removed: to_remove }
                    } else {
                        ProcessingResult::NotRedundant
                    }
                } else {
                    // [x >= v], [x >= v'] => [x >= v] if v > v'
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                }
            }
            PredicateType::UpperBound => {
                // [x >= v], [x <= v] => [x = v]
                if predicate.get_right_hand_side() == lower_bound {
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain == lower_bound),
                    }
                } else if predicate.get_right_hand_side() < upper_bound {
                    // [x <= v], [x <= v'] => [x <= v'] if v' < v
                    let to_remove = self.domains[predicate.get_domain()]
                        .iter()
                        .filter(|element| {
                            element.is_upper_bound_predicate()
                                || (element.is_not_equal_predicate()
                                    && element.get_right_hand_side()
                                        > predicate.get_right_hand_side())
                        })
                        .copied()
                        .collect::<Vec<_>>();
                    if !to_remove.is_empty() {
                        ProcessingResult::ReplacedPresent { removed: to_remove }
                    } else {
                        ProcessingResult::NotRedundant
                    }
                } else {
                    // [x <= v], [x <= v'] => [x <= v] if v < v'
                    self.explain_upper_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                }
            }
            PredicateType::NotEqual => {
                if predicate.get_right_hand_side() == upper_bound {
                    // [x <= v], [x != v] => [x <= v - 1]
                    self.explain_upper_bound_in_proof(predicate, context);
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain <= upper_bound - 1),
                    }
                } else if predicate.get_right_hand_side() > upper_bound {
                    // [x <= v], [x != v'] => [x <= v] where v' > v
                    self.explain_upper_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                } else if predicate.get_right_hand_side() == lower_bound {
                    // [x >= v], [x != v] => [x <= v + 1]
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain >= lower_bound + 1),
                    }
                } else if predicate.get_right_hand_side() < lower_bound {
                    // [x >= v], [x != v'] => [x >= v] where v' < v
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                } else {
                    ProcessingResult::NotRedundant
                }
            }
            PredicateType::Equal => ProcessingResult::NotRedundant,
        }
    }
}
