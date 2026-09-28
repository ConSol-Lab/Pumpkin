#[cfg(doc)]
use std::collections::BTreeSet;

use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::asserts::pumpkin_assert_simple;
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
/// The implementation is incremental; it relies on the invariant that the rewrite rules ensure that
/// for each variable, the (non-root) nogood contains at most one lower-bound predicate, at most one
/// upper-bound predicate (where an equality predicate counts as both), and distinct not-equals
/// predicates. Root-level predicates are not guaranteed to adhere to this invariant, and they are
/// never removed; they are therefore folded into a separate domain which is only tightened.
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
    /// Keeps track of the predicates for each [`DomainId`] in the current nogood.
    domains: HashMap<DomainId, DomainPredicates>,
    statistics: IterativeMinimiserStatistics,
}

/// The predicates in the current nogood over a single [`DomainId`].
#[derive(Clone, Debug, Default)]
struct DomainPredicates {
    /// The initial domain tightened by the root-level predicates; it is never loosened during
    /// conflict analysis.
    root: Option<IterativeDomain>,
    /// Whether a root-level predicate has been applied.
    ///
    /// Note that [`DomainPredicates::root`] cannot be used for this since it is also initialised
    /// when processing a predicate.
    has_root_predicate: bool,
    /// The non-root lower-bound predicate (either a lower-bound or an equality predicate).
    lower_bound: Option<Predicate>,
    /// The non-root upper-bound predicate (either an upper-bound or an equality predicate).
    upper_bound: Option<Predicate>,
    /// The (distinct) non-root not-equals predicates.
    not_equals: Vec<Predicate>,
}

impl DomainPredicates {
    /// Returns whether the provided value is a hole in the domain.
    fn is_hole(&self, root: &IterativeDomain, value: i32) -> bool {
        root.holes.contains(&value)
            || self
                .not_equals
                .iter()
                .any(|predicate| predicate.get_right_hand_side() == value)
    }

    /// Returns the non-root predicates.
    fn non_root_predicates(&self) -> Vec<Predicate> {
        // An equality predicate is stored in both slots, so we only return it once.
        let upper_bound = self
            .upper_bound
            .filter(|&element| self.lower_bound != Some(element));

        self.lower_bound
            .into_iter()
            .chain(upper_bound)
            .chain(self.not_equals.iter().copied())
            .collect()
    }
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

impl IterativeDomain {
    /// Creates the [`IterativeDomain`] corresponding to the initial domain of `domain_id`.
    fn initial(domain_id: DomainId, context: &ConflictAnalysisContext) -> Self {
        Self {
            lb: context.initial_lower_bound(domain_id),
            ub: context.initial_upper_bound(domain_id),
            holes: context.initial_holes(domain_id),
        }
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
        let Some(entry) = self.domains.get_mut(&predicate.get_domain()) else {
            return;
        };

        if entry.lower_bound == Some(predicate) {
            entry.lower_bound = None;
        }
        if entry.upper_bound == Some(predicate) {
            entry.upper_bound = None;
        }
        if let Some(to_remove_position) = entry
            .not_equals
            .iter()
            .position(|element| *element == predicate)
        {
            let _ = entry.not_equals.swap_remove(to_remove_position);
        }
    }

    /// Applies the given (non-root) predicate from the nogood.
    pub(crate) fn apply_predicate(&mut self, predicate: Predicate) {
        let entry = self.domains.entry(predicate.get_domain()).or_default();

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                pumpkin_assert_simple!(entry.lower_bound.is_none());
                entry.lower_bound = Some(predicate);
            }
            PredicateType::UpperBound => {
                pumpkin_assert_simple!(entry.upper_bound.is_none());
                entry.upper_bound = Some(predicate);
            }
            PredicateType::NotEqual => {
                pumpkin_assert_moderate!(!entry.not_equals.contains(&predicate));
                entry.not_equals.push(predicate);
            }
            PredicateType::Equal => {
                pumpkin_assert_simple!(entry.lower_bound.is_none() && entry.upper_bound.is_none());
                entry.lower_bound = Some(predicate);
                entry.upper_bound = Some(predicate);
            }
        }
    }

    /// Applies the given root-level predicate from the nogood.
    pub(crate) fn apply_root_predicate(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) {
        let domain = predicate.get_domain();
        let entry = self.domains.entry(domain).or_default();

        entry.has_root_predicate = true;
        let consistent = entry
            .root
            .get_or_insert_with(|| IterativeDomain::initial(domain, context))
            .apply(&predicate);
        assert!(consistent);
    }

    /// Processes the predicate, indicating via [`ProcessingResult`] what can happen to it.
    pub(crate) fn process_predicate(
        &mut self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) -> ProcessingResult {
        let domain = predicate.get_domain();
        let Some(entry) = self.domains.get_mut(&domain) else {
            return ProcessingResult::NotRedundant;
        };

        if entry.lower_bound.is_none()
            && entry.upper_bound.is_none()
            && entry.not_equals.is_empty()
            && !entry.has_root_predicate
        {
            return ProcessingResult::NotRedundant;
        }

        let _ = entry
            .root
            .get_or_insert_with(|| IterativeDomain::initial(domain, context));

        // Note that the initial domain needs to be explained each time since the deduction checker
        // requires the facts to be logged after the inferences which make use of them; this is a
        // no-op when not logging a proof.
        context.explain_initial_domain(domain);

        let entry = &self.domains[&domain];
        let root = entry.root.as_ref().unwrap();

        let (lower_bound, upper_bound) = calculate_bounds(entry, root);

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
                    let to_remove = lower_bound_to_remove(entry, predicate);

                    if !to_remove.is_empty() {
                        self.statistics.num_removed_by_bound += 1;
                    }

                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain == upper_bound),
                        removed: to_remove,
                    }
                } else if predicate.get_right_hand_side() > lower_bound {
                    if entry.is_hole(root, predicate.get_right_hand_side()) {
                        // [x >= v], [x != v] => [x <= v + 1]
                        self.statistics.num_removed_by_bound += 1;
                        let to_remove = lower_bound_to_remove(entry, predicate);

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
                        let to_remove = lower_bound_to_remove(entry, predicate);

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

                    let to_remove = upper_bound_to_remove(entry, predicate);

                    if !to_remove.is_empty() {
                        self.statistics.num_removed_by_bound += 1;
                    }

                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain == lower_bound),
                        removed: to_remove,
                    }
                } else if predicate.get_right_hand_side() < upper_bound {
                    if entry.is_hole(root, predicate.get_right_hand_side()) {
                        // [x <= v], [x != v] => [x <= v - 1]
                        self.statistics.num_removed_by_bound += 1;
                        let to_remove = upper_bound_to_remove(entry, predicate);

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
                        let to_remove = upper_bound_to_remove(entry, predicate);
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
                } else if entry.is_hole(root, predicate.get_right_hand_side()) {
                    self.statistics.num_redundant += 1;
                    ProcessingResult::Redundant
                } else {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                }
            }
            PredicateType::Equal => {
                let removed = entry.non_root_predicates();
                if removed.is_empty() {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                } else {
                    self.statistics.num_removed_by_equality += 1;
                    // [x ⊗ v], [x = v] => [x = v]
                    ProcessingResult::ReplacedPresent { removed }
                }
            }
        }
    }
}

/// Calculates the upper-bound and lower-bound, based on the provided [`DomainPredicates`]
fn calculate_bounds(entry: &DomainPredicates, root: &IterativeDomain) -> (i32, i32) {
    let mut lower_bound = entry.lower_bound.map_or(root.lb, |element| {
        root.lb.max(element.get_right_hand_side())
    });
    while entry.is_hole(root, lower_bound) {
        lower_bound += 1;
    }

    let mut upper_bound = entry.upper_bound.map_or(root.ub, |element| {
        root.ub.min(element.get_right_hand_side())
    });
    while entry.is_hole(root, upper_bound) {
        upper_bound -= 1;
    }

    assert!(lower_bound <= upper_bound);

    (lower_bound, upper_bound)
}

/// Returns the predicates which are removed when tightening the lower-bound using `predicate`.
fn lower_bound_to_remove(entry: &DomainPredicates, predicate: Predicate) -> Vec<Predicate> {
    entry
        .lower_bound
        .iter()
        .chain(
            entry
                .not_equals
                .iter()
                .filter(|element| element.get_right_hand_side() < predicate.get_right_hand_side()),
        )
        .copied()
        .collect()
}

/// Returns the predicates which are removed when tightening the upper-bound using `predicate`.
fn upper_bound_to_remove(entry: &DomainPredicates, predicate: Predicate) -> Vec<Predicate> {
    entry
        .upper_bound
        .iter()
        .chain(
            entry
                .not_equals
                .iter()
                .filter(|element| element.get_right_hand_side() > predicate.get_right_hand_side()),
        )
        .copied()
        .collect()
}
