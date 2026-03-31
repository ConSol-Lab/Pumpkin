use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::HashSet;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::variables::DomainId;

pub(crate) struct IterativeMinimiser {
    domains: KeyedVec<DomainId, IterativeDomain>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ProcessingResult {
    Redundant,
    ReplacedPresent {
        removed: Predicate,
    },
    ReplacedWithNew {
        previous: Predicate,
        new_predicate: Predicate,
    },
    NotRedundant,
}

#[derive(Debug, Clone)]
struct IterativeDomain {
    lower_bound_updates: Vec<i32>,
    upper_bound_updates: Vec<i32>,
    holes: HashMap<i32, (bool, bool)>,

    lower_bound: i32,
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
    pub(crate) fn apply_predicate(&mut self, predicate: Predicate) {
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if predicate.get_right_hand_side() > self.lower_bound {
                    self.lower_bound = predicate.get_right_hand_side();
                    self.lower_bound_updates
                        .push(predicate.get_right_hand_side());
                }
            }
            PredicateType::UpperBound => {
                if predicate.get_right_hand_side() < self.upper_bound {
                    self.upper_bound = predicate.get_right_hand_side();
                    self.upper_bound_updates
                        .push(predicate.get_right_hand_side());
                }
            }
            PredicateType::NotEqual => {
                let mut lb_caused_update = false;
                let mut ub_caused_update = false;
                if predicate.get_right_hand_side() == self.lower_bound {
                    lb_caused_update = true;
                    self.lower_bound = predicate.get_right_hand_side() + 1;
                    self.lower_bound_updates
                        .push(predicate.get_right_hand_side() + 1);
                }
                if predicate.get_right_hand_side() == self.upper_bound {
                    ub_caused_update = true;
                    self.upper_bound = predicate.get_right_hand_side() - 1;
                    self.upper_bound_updates
                        .push(predicate.get_right_hand_side() - 1);
                }

                let _ = self.holes.insert(
                    predicate.get_right_hand_side(),
                    (lb_caused_update, ub_caused_update),
                );
            }
            PredicateType::Equal => {
                self.upper_bound = predicate.get_right_hand_side();
                self.upper_bound_updates
                    .push(predicate.get_right_hand_side());

                self.lower_bound = predicate.get_right_hand_side();
                self.lower_bound_updates
                    .push(predicate.get_right_hand_side());
            }
        }
    }

    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                let position = self
                    .lower_bound_updates
                    .iter()
                    .rev()
                    .position(|value| *value == predicate.get_right_hand_side())
                    .expect("Expected removed lower-bound to be present");

                let _ = self.lower_bound_updates.remove(position);
                self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);
            }
            PredicateType::UpperBound => {
                let position = self
                    .upper_bound_updates
                    .iter()
                    .rev()
                    .position(|value| *value == predicate.get_right_hand_side())
                    .expect("Expected removed lower-bound to be present");

                let _ = self.upper_bound_updates.remove(position);
                self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MIN);
            }
            PredicateType::NotEqual => {
                let (lb_caused_update, ub_caused_update) = self
                    .holes
                    .get(&predicate.get_right_hand_side())
                    .expect("Expected removed not equals to be present");

                if *lb_caused_update {
                    let position = self
                        .lower_bound_updates
                        .iter()
                        .rev()
                        .position(|value| *value == predicate.get_right_hand_side() + 1)
                        .expect("Expected removed lower-bound to be present");

                    let _ = self.lower_bound_updates.remove(position);
                    self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);
                }

                if *ub_caused_update {
                    let position = self
                        .upper_bound_updates
                        .iter()
                        .rev()
                        .position(|value| *value == predicate.get_right_hand_side() - 1)
                        .expect("Expected removed lower-bound to be present");

                    let _ = self.upper_bound_updates.remove(position);
                    self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MIN);
                }
            }
            PredicateType::Equal => {
                assert_eq!(self.lower_bound, self.upper_bound);
                assert_eq!(self.lower_bound, predicate.get_right_hand_side());

                let _ = self.lower_bound_updates.pop();
                let _ = self.upper_bound_updates.pop();
                self.lower_bound = self.lower_bound_updates.last().copied().unwrap_or(i32::MIN);
                self.upper_bound = self.upper_bound_updates.last().copied().unwrap_or(i32::MIN);
            }
        }
    }
}

impl IterativeMinimiser {
    pub(crate) fn remove_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        self.domains[domain].remove_predicate(predicate);
    }

    pub(crate) fn apply_predicate(&mut self, predicate: Predicate) {
        let domain = predicate.get_domain();
        self.domains[domain].apply_predicate(predicate);
    }

    pub(crate) fn process_predicate(&mut self, predicate: Predicate) -> ProcessingResult {
        let domain = predicate.get_domain();
        self.domains.accomodate(domain, IterativeDomain::default());

        let lower_bound = self.domains[domain].lower_bound;
        let upper_bound = self.domains[domain].upper_bound;

        if lower_bound == upper_bound {
            return ProcessingResult::Redundant;
        }

        match predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                if predicate.get_right_hand_side() == upper_bound {
                    return ProcessingResult::ReplacedWithNew {
                        previous: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain == upper_bound),
                    };
                } else if predicate.get_right_hand_side() > lower_bound {
                    return ProcessingResult::ReplacedPresent {
                        removed: predicate!(domain >= lower_bound),
                    };
                } else {
                    return ProcessingResult::Redundant;
                }
            }
            PredicateType::UpperBound => {
                if predicate.get_right_hand_side() == lower_bound {
                    return ProcessingResult::ReplacedWithNew {
                        previous: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain == lower_bound),
                    };
                } else if predicate.get_right_hand_side() < upper_bound {
                    return ProcessingResult::ReplacedPresent {
                        removed: predicate!(domain <= upper_bound),
                    };
                } else {
                    return ProcessingResult::Redundant;
                }
            }
            PredicateType::NotEqual => {
                if predicate.get_right_hand_side() == upper_bound {
                    return ProcessingResult::ReplacedWithNew {
                        previous: predicate!(domain <= upper_bound),
                        new_predicate: predicate!(domain <= upper_bound - 1),
                    };
                } else if predicate.get_right_hand_side() > upper_bound {
                    return ProcessingResult::Redundant;
                } else if predicate.get_right_hand_side() == lower_bound {
                    return ProcessingResult::ReplacedWithNew {
                        previous: predicate!(domain >= lower_bound),
                        new_predicate: predicate!(domain >= lower_bound + 1),
                    };
                } else if predicate.get_right_hand_side() < lower_bound {
                    return ProcessingResult::Redundant;
                } else {
                    return ProcessingResult::NotRedundant;
                }
            }
            PredicateType::Equal => ProcessingResult::NotRedundant,
        }
    }
}
