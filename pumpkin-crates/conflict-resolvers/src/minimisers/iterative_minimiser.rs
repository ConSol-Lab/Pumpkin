use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashSet;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::StorageKey;
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
    state: KeyedVec<DomainId, IterativeDomain>,
    /// Keeps track of the predicates for each [`DomainId`] in the current nogood.
    domains: KeyedVec<DomainId, Vec<Predicate>>,
    /// For proof logging; keeps track of the predicates which are true at checkpoint 0.
    root_predicates: KeyedVec<DomainId, Vec<Predicate>>,
    statistics: IterativeMinimiserStatistics,
}

#[derive(Clone, Debug)]
struct IterativeDomain {
    lb: i32,
    ub: i32,
    holes: HashSet<i32>,
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
    fn reset(&mut self) {
        self.lb = i32::MIN;
        self.ub = i32::MAX;
        self.holes.clear()
    }

    fn tighten_lower_bound(&mut self, mut lb: i32) {
        if self.lb >= lb {
            return;
        }

        while self.holes.contains(&lb) {
            lb += 1;
        }

        self.lb = lb;
    }

    fn tighten_upper_bound(&mut self, mut ub: i32) {
        if self.ub <= ub {
            return;
        }

        while self.holes.contains(&ub) {
            ub -= 1;
        }

        self.ub = ub;
    }

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
                    let _ = self.holes.insert(predicate.get_right_hand_side());
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
    num_non_redundant: usize,
    num_redundant: usize,
    num_removed_by_bound: usize,
    num_removed_by_hole: usize,
    num_removed_by_equality: usize,
    num_removed_by_fixed_domain: usize,
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
    /// Clears the structures.
    pub(crate) fn clear(&mut self) {
        self.domains.clear();
        self.root_predicates.clear();
        self.state = Default::default();
    }

    pub(crate) fn log_statistics(&self, statistic_logger: StatisticLogger) {
        let statistic_logger = statistic_logger.attach_to_prefix("IterativeMinimiser");
        self.statistics.log(statistic_logger);
    }

    /// Removes the given predicate from the nogood.
    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        while let Some(to_remove_position) = self.domains[domain]
            .iter()
            .position(|element| *element == predicate)
        {
            let _ = self.domains[domain].remove(to_remove_position);
        }
    }

    /// Applies the given predicate from the nogood.
    pub(crate) fn apply_predicate(
        &mut self,
        predicate: Predicate,
        context: &ConflictAnalysisContext,
    ) {
        // println!("APPLYING {predicate:?}");
        let domain = predicate.get_domain();
        self.domains.accomodate(domain, Default::default());
        self.domains[domain].push(predicate);

        if context.is_proof_logging_inferences()
            && context.get_checkpoint_for_predicate(predicate) == Some(0)
        {
            self.root_predicates.accomodate(domain, Default::default());
            self.root_predicates[domain].push(predicate)
        }
    }

    /// Explains the lower-bound in the proof log.
    fn explain_lower_bound_in_proof(
        &self,
        predicate: Predicate,
        context: &mut ConflictAnalysisContext,
    ) {
        if context.is_proof_logging_inferences() {
            let domain = predicate.get_domain();

            if domain.index() >= self.root_predicates.len() {
                return;
            }

            for root_predicate in self.root_predicates[domain]
                .iter()
                .filter(|root_predicate| {
                    root_predicate.is_lower_bound_predicate()
                        || root_predicate.is_not_equal_predicate()
                })
            {
                context.explain_root_assignment(*root_predicate);
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

            if domain.index() >= self.root_predicates.len() {
                return;
            }

            for root_predicate in self.root_predicates[domain]
                .iter()
                .filter(|root_predicate| {
                    root_predicate.is_upper_bound_predicate()
                        || root_predicate.is_not_equal_predicate()
                })
            {
                context.explain_root_assignment(*root_predicate);
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

        self.state.accomodate(domain, Default::default());
        self.state[domain].reset();
        for predicate in self.domains[predicate.get_domain()].iter() {
            let consistent = self.state[domain].apply(predicate);
            assert!(consistent)
        }

        let lower_bound = self.state[domain].lb;
        let upper_bound = self.state[domain].ub;

        // If the domain is assigned, then the added predicate is redundant.
        //
        // Encompasses the rules:
        // - [x = v], [x != v'] => [x = v]
        // - [x = v], [x <= v'] => [x = v]
        // - [x = v], [x >= v'] => [x = v]
        if lower_bound == upper_bound {
            self.statistics.num_removed_by_fixed_domain += 1;

            self.explain_lower_bound_in_proof(predicate, context);
            self.explain_upper_bound_in_proof(predicate, context);
            return ProcessingResult::Redundant;
        }

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if predicate.get_right_hand_side() == upper_bound {
                    self.statistics.num_removed_by_creating_equality += 1;
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
                        self.statistics.num_removed_by_bound += 1;
                        ProcessingResult::ReplacedPresent { removed: to_remove }
                    } else {
                        self.statistics.num_non_redundant += 1;
                        ProcessingResult::NotRedundant
                    }
                } else {
                    self.statistics.num_redundant += 1;
                    // [x >= v], [x >= v'] => [x >= v] if v > v'
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                }
            }
            PredicateType::UpperBound => {
                // [x >= v], [x <= v] => [x = v]
                if predicate.get_right_hand_side() == lower_bound {
                    self.statistics.num_removed_by_creating_equality += 1;
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
                        self.statistics.num_removed_by_bound += 1;
                        ProcessingResult::ReplacedPresent { removed: to_remove }
                    } else {
                        self.statistics.num_non_redundant += 1;
                        ProcessingResult::NotRedundant
                    }
                } else {
                    self.statistics.num_redundant += 1;
                    // [x <= v], [x <= v'] => [x <= v] if v < v'
                    self.explain_upper_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                }
            }
            PredicateType::NotEqual => {
                if predicate.get_right_hand_side() == upper_bound {
                    self.statistics.num_removed_by_hole += 1;
                    // [x <= v], [x != v] => [x <= v - 1]
                    self.explain_upper_bound_in_proof(predicate, context);
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain <= upper_bound - 1),
                    }
                } else if predicate.get_right_hand_side() > upper_bound {
                    self.statistics.num_redundant += 1;
                    // [x <= v], [x != v'] => [x <= v] where v' > v
                    self.explain_upper_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                } else if predicate.get_right_hand_side() == lower_bound {
                    self.statistics.num_removed_by_hole += 1;
                    // [x >= v], [x != v] => [x <= v + 1]
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::PossiblyReplacedWithNew {
                        removed: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain >= lower_bound + 1),
                    }
                } else if predicate.get_right_hand_side() < lower_bound {
                    self.statistics.num_redundant += 1;
                    // [x >= v], [x != v'] => [x >= v] where v' < v
                    self.explain_lower_bound_in_proof(predicate, context);
                    ProcessingResult::Redundant
                } else {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                }
            }
            PredicateType::Equal => {
                if self.domains[predicate.get_domain()].is_empty() {
                    self.statistics.num_non_redundant += 1;
                    ProcessingResult::NotRedundant
                } else {
                    self.statistics.num_removed_by_equality += 1;
                    // [x ⊗ v], [x = v] => [x = v]
                    ProcessingResult::ReplacedPresent {
                        removed: self.domains[predicate.get_domain()].clone(),
                    }
                }
            }
        }
    }
}
