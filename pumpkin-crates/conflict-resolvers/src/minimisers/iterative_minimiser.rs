#[cfg(doc)]
use std::collections::BTreeSet;

use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashMap;
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::statistics::Statistic;
use pumpkin_core::statistics::StatisticLogger;
use pumpkin_core::variables::DomainId;

/// A minimiser which iteratively applies rewrite rules based on the semantic meaning of predicates
/// *during* conflict analysis.
///
/// The implementation is heavily inspired by \[1\].
///
/// The current implementation is inefficient, and recalculates current induced domain of a
/// variable each time a predicate is added to the nogood.
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
    state: IterativeDomain,
    /// Keeps track of the predicates for each [`DomainId`] in the current nogood.
    domains: HashMap<DomainId, Vec<Predicate>>,
    statistics: IterativeMinimiserStatistics,
}

/// A simple representation of a domain.
///
/// Differs from `VariableState` by not allowing infinity as bounds, and using a [`Vec`] instead
/// of a [`BTreeSet`] for storing the holes (to improve efficiency).
#[derive(Clone, Debug)]
struct IterativeDomain {
    lb: i32,
    ub: i32,
    holes: Vec<i32>,
}

impl Default for IterativeDomain {
    fn default() -> Self {
        Self {
            lb: i32::MIN,
            ub: i32::MAX,
            holes: Default::default(),
        }
    }
}

impl IterativeDomain {
    /// Resets the [`IterativeDomain`] to the maximum bounds, and removes the holes.
    fn reset(&mut self, domain_id: DomainId, context: &mut ConflictAnalysisContext) {
        self.lb = context.initial_lower_bound(domain_id);
        self.ub = context.initial_upper_bound(domain_id);
        self.holes = context.initial_holes(domain_id);
    }

    /// Tightens the lower-bound to `lb`.
    ///
    /// Note that this can lead to a lower-bound which is larger than `lb` due to holes in the
    /// domain.
    fn tighten_lower_bound(&mut self, mut lb: i32) {
        if self.lb >= lb {
            return;
        }

        while self.holes.contains(&lb) {
            lb += 1;
        }

        self.lb = lb;
    }

    /// Tightens the upper-bound to `ub`.
    ///
    /// Note that this can lead to a upper-bound which is smaller than `ub` due to holes in the
    /// domain.
    fn tighten_upper_bound(&mut self, mut ub: i32) {
        if self.ub <= ub {
            return;
        }

        while self.holes.contains(&ub) {
            ub -= 1;
        }

        self.ub = ub;
    }

    /// Applies the provided [`Predicate`] to the [`IterativeDomain`].
    fn apply(&mut self, predicate: &Predicate) -> bool {
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => self.tighten_lower_bound(predicate.get_right_hand_side()),
            PredicateType::UpperBound => self.tighten_upper_bound(predicate.get_right_hand_side()),
            PredicateType::NotEqual => {
                if predicate.get_right_hand_side() == self.lb {
                    self.tighten_lower_bound(self.lb + 1);
                }

                if predicate.get_right_hand_side() == self.ub {
                    self.tighten_upper_bound(self.ub - 1);
                }

                if predicate.get_right_hand_side() > self.lb
                    && predicate.get_right_hand_side() < self.ub
                {
                    self.holes.push(predicate.get_right_hand_side());
                }
            }
            PredicateType::Equal => {
                self.tighten_lower_bound(predicate.get_right_hand_side());
                self.tighten_upper_bound(predicate.get_right_hand_side());
            }
        }

        self.lb <= self.ub
    }
}

create_statistics_struct!(IterativeMinimiserStatistics {
    /// The number of non-redundant predicates encountered.
    num_non_redundant: usize,
    /// The number of redundant predicates encountered.
    num_redundant: usize,
    /// The number of predicates removed by a bound.
    num_removed_by_bound: usize,
    /// The number of predicates removed by a hole.
    num_removed_by_hole: usize,
    /// The number of predicates removed by an equality.
    num_removed_by_equality: usize,
    /// The number of predicates removed because the domain is fixed.
    num_removed_by_fixed_domain: usize,
    /// The number of predicates removed because an equality was created.
    num_removed_by_creating_equality: usize,
});

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
    /// [`ProcessingResult::PossiblyReplacedWithNew::new_predicate`], it also possibly removed
    /// [`ProcessingResult::PossiblyReplacedWithNew::potentially_removed`] (if it exists), and it
    /// removed [`ProcessingResult::PossiblyReplacedWithNew::removed`].
    ///
    /// Note that it is not always possible to replace with `new_predicate` (since it can lead to
    /// infinite loops), so it is not guaranteed that `new_predicate` is added. The final field is
    /// necessary to ensure that the predicates are correctly removed in case `new_predicate` is
    /// **not** added.
    ///
    /// e.g., if [x >= 5] is in the nogood, and the predicate [x <= 5] is added, then [x >= 5] is
    /// removed and replaced with [x == 5] (and [x <= 5] is not added).
    PossiblyReplacedWithNew {
        potentially_removed: Predicate,
        new_predicate: Predicate,
        removed: Vec<Predicate>,
    },
    /// The predicate was found to be not redundant.
    NotRedundant,
}

impl IterativeMinimiser {
    /// Clears the structures.
    pub(crate) fn clear(&mut self) {
        self.domains.clear();
    }

    pub(crate) fn log_statistics(&self, statistic_logger: StatisticLogger) {
        let statistic_logger = statistic_logger.attach_to_prefix("IterativeMinimiser");
        self.statistics.log(statistic_logger);
    }

    /// Removes the given predicate from the nogood.
    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        if let Some(to_remove_position) = self.domains[&domain]
            .iter()
            .position(|element| *element == predicate)
        {
            let _ = self
                .domains
                .get_mut(&domain)
                .unwrap()
                .swap_remove(to_remove_position);
        }
    }

    /// Applies the given predicate from the nogood.
    pub(crate) fn apply_predicate(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) {
        let domain = predicate.get_domain();

        let entry = self.domains.entry(domain).or_insert_with(|| {
            context.explain_initial_domain(domain);
            Default::default()
        });
        entry.push(predicate);
    }

    /// Processes the predicate, indicating via [`ProcessingResult`] what can happen to it.
    pub(crate) fn process_predicate(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) -> ProcessingResult {
        let domain = predicate.get_domain();
        let Some(predicates) = self.domains.get(&domain) else {
            return ProcessingResult::NotRedundant;
        };

        if predicates.is_empty() {
            return ProcessingResult::NotRedundant;
        }

        self.state.reset(domain, context);

        for predicate in predicates.iter() {
            let consistent = self.state.apply(predicate);
            assert!(consistent)
        }

        let lower_bound = self.state.lb;
        let upper_bound = self.state.ub;

        // If the domain is assigned, then the added predicate is redundant.
        //
        // Encompasses the rules:
        // - [x = v], [x != v'] => [x = v]
        // - [x = v], [x <= v'] => [x = v]
        // - [x = v], [x >= v'] => [x = v]
        if lower_bound == upper_bound {
            self.statistics.num_removed_by_fixed_domain += 1;

            return ProcessingResult::Redundant;
        }

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if predicate.get_right_hand_side() == upper_bound {
                    self.statistics.num_removed_by_creating_equality += 1;
                    // [x <= v], [x >= v] => [x = v]
                    let to_remove = predicates
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
                        self.statistics.num_removed_by_bound += 1;
                    }

                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain == upper_bound),
                        removed: to_remove,
                    }
                } else if predicate.get_right_hand_side() > lower_bound {
                    if self.state.holes.contains(&predicate.get_right_hand_side()) {
                        // [x >= v], [x != v] => [x <= v + 1]
                        self.statistics.num_removed_by_bound += 1;
                        let to_remove = predicates
                            .iter()
                            .filter(|element| {
                                element.is_lower_bound_predicate()
                                    || (element.is_not_equal_predicate()
                                        && element.get_right_hand_side()
                                            < predicate.get_right_hand_side())
                            })
                            .copied()
                            .collect::<Vec<_>>();

                        ProcessingResult::PossiblyReplacedWithNew {
                            potentially_removed: predicate!(
                                domain != predicate.get_right_hand_side()
                            ),
                            new_predicate: predicate!(
                                domain >= predicate.get_right_hand_side() + 1
                            ),
                            removed: to_remove,
                        }
                    } else {
                        // [x >= v], [x >= v'] => [x >= v'] if v' > v
                        let to_remove = predicates
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
                            self.statistics.num_removed_by_bound += 1;
                            ProcessingResult::ReplacedPresent { removed: to_remove }
                        } else {
                            self.statistics.num_non_redundant += 1;
                            ProcessingResult::NotRedundant
                        }
                    }
                } else {
                    self.statistics.num_redundant += 1;
                    // [x >= v], [x >= v'] => [x >= v] if v > v'
                    ProcessingResult::Redundant
                }
            }
            PredicateType::UpperBound => {
                // [x >= v], [x <= v] => [x = v]
                if predicate.get_right_hand_side() == lower_bound {
                    self.statistics.num_removed_by_creating_equality += 1;

                    let to_remove = predicates
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
                        self.statistics.num_removed_by_bound += 1;
                    }

                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain == lower_bound),
                        removed: to_remove,
                    }
                } else if predicate.get_right_hand_side() < upper_bound {
                    if self.state.holes.contains(&predicate.get_right_hand_side()) {
                        // [x <= v], [x != v] => [x <= v - 1]
                        self.statistics.num_removed_by_bound += 1;
                        let to_remove = predicates
                            .iter()
                            .filter(|element| {
                                element.is_upper_bound_predicate()
                                    || (element.is_not_equal_predicate()
                                        && element.get_right_hand_side()
                                            > predicate.get_right_hand_side())
                            })
                            .copied()
                            .collect::<Vec<_>>();

                        ProcessingResult::PossiblyReplacedWithNew {
                            potentially_removed: predicate!(
                                domain != predicate.get_right_hand_side()
                            ),
                            new_predicate: predicate!(
                                domain <= predicate.get_right_hand_side() - 1
                            ),
                            removed: to_remove,
                        }
                    } else {
                        // [x <= v], [x <= v'] => [x <= v'] if v' < v
                        let to_remove = predicates
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
                            self.statistics.num_removed_by_bound += 1;
                            ProcessingResult::ReplacedPresent { removed: to_remove }
                        } else {
                            self.statistics.num_non_redundant += 1;
                            ProcessingResult::NotRedundant
                        }
                    }
                } else {
                    self.statistics.num_redundant += 1;
                    // [x <= v], [x <= v'] => [x <= v] if v < v'
                    ProcessingResult::Redundant
                }
            }
            PredicateType::NotEqual => {
                if predicate.get_right_hand_side() == upper_bound {
                    self.statistics.num_removed_by_hole += 1;
                    // [x <= v], [x != v] => [x <= v - 1]
                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain <= upper_bound - 1),
                        removed: vec![],
                    }
                } else if predicate.get_right_hand_side() > upper_bound {
                    self.statistics.num_redundant += 1;
                    // [x <= v], [x != v'] => [x <= v] where v' > v
                    ProcessingResult::Redundant
                } else if predicate.get_right_hand_side() == lower_bound {
                    self.statistics.num_removed_by_hole += 1;
                    // [x >= v], [x != v] => [x <= v + 1]
                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain >= lower_bound + 1),
                        removed: vec![],
                    }
                } else if predicate.get_right_hand_side() < lower_bound {
                    self.statistics.num_redundant += 1;
                    // [x >= v], [x != v'] => [x >= v] where v' < v
                    ProcessingResult::Redundant
                } else if self.state.holes.contains(&predicate.get_right_hand_side()) {
                    self.statistics.num_redundant += 1;
                    ProcessingResult::Redundant
                } else {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                }
            }
            PredicateType::Equal => {
                if predicates.is_empty() {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                } else {
                    self.statistics.num_removed_by_equality += 1;
                    // [x ⊗ v], [x = v] => [x = v]
                    ProcessingResult::ReplacedPresent {
                        removed: predicates.clone(),
                    }
                }
            }
        }
    }
}
