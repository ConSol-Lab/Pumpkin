use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::HashSet;
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

/// The domain induced by the predicates in the current nogood over a single [`DomainId`].
#[derive(Clone, Debug)]
struct DomainPredicates {
    /// The lower-bound of the initial domain tightened by the root-level predicates.
    root_lower_bound: i32,
    /// The upper-bound of the initial domain tightened by the root-level predicates.
    root_upper_bound: i32,
    /// The holes of the initial domain and the root-level not-equals predicates.
    root_holes: HashSet<i32>,
    /// The root-level predicates which have been applied.
    ///
    /// These are stored so that they can be explained again each time that they are used; see
    /// [`IterativeMinimiser::process_predicate`].
    root_predicates: Vec<Predicate>,
    has_root_predicates: bool,
    /// The non-root lower-bound (or equality) predicate.
    lower_bound: Option<Predicate>,
    /// The non-root upper-bound (or equality) predicate.
    upper_bound: Option<Predicate>,
    /// The right-hand sides of the non-root not-equals predicates.
    not_equals: HashSet<i32>,
}

impl DomainPredicates {
    fn new(domain: DomainId, context: &ConflictAnalysisContext) -> Self {
        Self {
            root_lower_bound: context.initial_lower_bound(domain),
            root_upper_bound: context.initial_upper_bound(domain),
            root_holes: context.initial_holes(domain).into_iter().collect(),
            root_predicates: Vec::new(),
            has_root_predicates: false,
            lower_bound: None,
            upper_bound: None,
            not_equals: HashSet::default(),
        }
    }

    fn is_empty(&self) -> bool {
        self.lower_bound.is_none()
            && self.upper_bound.is_none()
            && self.not_equals.is_empty()
            && !self.has_root_predicates
    }

    fn is_hole(&self, value: i32) -> bool {
        self.root_holes.contains(&value) || self.not_equals.contains(&value)
    }

    /// Returns the lower-bound and upper-bound of the induced domain.
    fn bounds(&self) -> (i32, i32) {
        let mut lower_bound = self.lower_bound.map_or(self.root_lower_bound, |lb| {
            lb.get_right_hand_side().max(self.root_lower_bound)
        });
        while self.is_hole(lower_bound) {
            lower_bound += 1;
        }

        let mut upper_bound = self.upper_bound.map_or(self.root_upper_bound, |ub| {
            ub.get_right_hand_side().min(self.root_upper_bound)
        });
        while self.is_hole(upper_bound) {
            upper_bound -= 1;
        }

        assert!(lower_bound <= upper_bound);
        (lower_bound, upper_bound)
    }

    /// Returns the non-root predicates.
    ///
    /// Note that the domain is not fixed when this is called, so an equality predicate cannot be
    /// present.
    fn non_root_predicates(&self, domain: DomainId) -> Vec<Predicate> {
        self.lower_bound
            .into_iter()
            .chain(self.upper_bound)
            .chain(
                self.not_equals
                    .iter()
                    .map(|&hole| predicate!(domain != hole)),
            )
            .collect()
    }

    /// Returns the non-root lower-bound predicate and the non-root not-equals predicates below
    /// `value`.
    ///
    /// Note that the domain is not fixed when this is called, so an equality predicate cannot be
    /// present.
    fn lower_bound_and_not_equals_below(&self, domain: DomainId, value: i32) -> Vec<Predicate> {
        self.lower_bound
            .into_iter()
            .chain(
                self.not_equals
                    .iter()
                    .filter(|&&hole| hole < value)
                    .map(|&hole| predicate!(domain != hole)),
            )
            .collect()
    }

    /// Returns the non-root upper-bound predicate and the non-root not-equals predicates above
    /// `value`.
    ///
    /// Note that the domain is not fixed when this is called, so an equality predicate cannot be
    /// present.
    fn upper_bound_and_not_equals_above(&self, domain: DomainId, value: i32) -> Vec<Predicate> {
        self.upper_bound
            .into_iter()
            .chain(
                self.not_equals
                    .iter()
                    .filter(|&&hole| hole > value)
                    .map(|&hole| predicate!(domain != hole)),
            )
            .collect()
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

        if predicate.is_not_equal_predicate() {
            let _ = entry.not_equals.remove(&predicate.get_right_hand_side());
            return;
        }

        if entry.lower_bound == Some(predicate) {
            entry.lower_bound = None;
        }
        if entry.upper_bound == Some(predicate) {
            entry.upper_bound = None;
        }
    }

    /// Applies the given (non-root) predicate from the nogood.
    pub(crate) fn apply_predicate(
        &mut self,
        predicate: Predicate,
        context: &ConflictAnalysisContext,
    ) {
        let domain = predicate.get_domain();
        let entry = self
            .domains
            .entry(domain)
            .or_insert_with(|| DomainPredicates::new(domain, context));

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
                let newly_inserted = entry.not_equals.insert(predicate.get_right_hand_side());
                pumpkin_assert_simple!(newly_inserted);
            }
            PredicateType::Equal => {
                pumpkin_assert_simple!(entry.lower_bound.is_none() && entry.upper_bound.is_none());
                entry.lower_bound = Some(predicate);
                entry.upper_bound = Some(predicate);
            }
        }
    }

    /// Applies the given root-level predicate from the nogood.
    ///
    /// Root-level predicates are never removed, and are not guaranteed to adhere to the invariant
    /// described in [`IterativeMinimiser`]; they are therefore folded into the root domain.
    pub(crate) fn apply_root_predicate(
        &mut self,
        predicate: Predicate,
        context: &ConflictAnalysisContext,
    ) {
        let domain = predicate.get_domain();
        let entry = self
            .domains
            .entry(domain)
            .or_insert_with(|| DomainPredicates::new(domain, context));

        entry.has_root_predicates = true;
        if context.is_proof_logging_inferences() {
            entry.root_predicates.push(predicate);
        }

        let value = predicate.get_right_hand_side();
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => entry.root_lower_bound = entry.root_lower_bound.max(value),
            PredicateType::UpperBound => entry.root_upper_bound = entry.root_upper_bound.min(value),
            PredicateType::NotEqual => {
                let _ = entry.root_holes.insert(value);
            }
            PredicateType::Equal => {
                entry.root_lower_bound = entry.root_lower_bound.max(value);
                entry.root_upper_bound = entry.root_upper_bound.min(value);
            }
        }
    }

    /// Logs the root-level inferences to the proof.
    fn log_root_inferences(
        context: &mut ConflictAnalysisContext<'_>,
        domain: DomainId,
        entry: &DomainPredicates,
    ) {
        // Note that the initial domain and the root-level predicates need to be explained each time
        // since the deduction checker requires the facts to be logged after the inferences which
        // make use of them; this is a no-op when not logging a proof.
        if context.is_proof_logging_inferences() {
            context.explain_initial_domain(domain);
            for &root_predicate in entry.root_predicates.iter() {
                context.explain_root_assignment(root_predicate);
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
        let Some(entry) = self.domains.get(&domain).filter(|entry| !entry.is_empty()) else {
            return ProcessingResult::NotRedundant;
        };

        Self::log_root_inferences(context, domain, entry);

        let (lower_bound, upper_bound) = entry.bounds();

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
                    let to_remove = entry
                        .lower_bound_and_not_equals_below(domain, predicate.get_right_hand_side());

                    if !to_remove.is_empty() {
                        self.statistics.num_removed_by_bound += 1;
                    }

                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain == upper_bound),
                        removed: to_remove,
                    }
                } else if predicate.get_right_hand_side() > lower_bound {
                    if entry.is_hole(predicate.get_right_hand_side()) {
                        // [x >= v], [x != v] => [x <= v + 1]
                        self.statistics.num_removed_by_bound += 1;
                        let to_remove = entry.lower_bound_and_not_equals_below(
                            domain,
                            predicate.get_right_hand_side(),
                        );

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
                        let to_remove = entry.lower_bound_and_not_equals_below(
                            domain,
                            predicate.get_right_hand_side(),
                        );

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

                    let to_remove = entry
                        .upper_bound_and_not_equals_above(domain, predicate.get_right_hand_side());

                    if !to_remove.is_empty() {
                        self.statistics.num_removed_by_bound += 1;
                    }

                    ProcessingResult::PossiblyReplacedWithNew {
                        potentially_removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain == lower_bound),
                        removed: to_remove,
                    }
                } else if predicate.get_right_hand_side() < upper_bound {
                    if entry.is_hole(predicate.get_right_hand_side()) {
                        // [x <= v], [x != v] => [x <= v - 1]
                        self.statistics.num_removed_by_bound += 1;
                        let to_remove = entry.upper_bound_and_not_equals_above(
                            domain,
                            predicate.get_right_hand_side(),
                        );

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
                        let to_remove = entry.upper_bound_and_not_equals_above(
                            domain,
                            predicate.get_right_hand_side(),
                        );
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
                } else if entry.is_hole(predicate.get_right_hand_side()) {
                    self.statistics.num_redundant += 1;
                    ProcessingResult::Redundant
                } else {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                }
            }
            PredicateType::Equal => {
                let removed = entry.non_root_predicates(domain);
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
