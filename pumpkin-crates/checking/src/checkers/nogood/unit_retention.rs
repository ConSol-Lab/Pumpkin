use super::NogoodChecker;
use super::truth_value;
use crate::AtomicConstraint;
use crate::DomainView;
use crate::RetentionCheck;
use crate::RetentionChecker;

impl<Atomic: AtomicConstraint> RetentionChecker<Atomic> for NogoodChecker<Atomic> {
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        let untrue_predicate_count = self
            .nogood
            .iter()
            .filter(|atomic| truth_value(*atomic, state) != Some(true))
            .count();

        // If at least two predicates are not true, or any predicate is false,
        // then the domains are unit-propagation consistent.
        let is_consistent = untrue_predicate_count >= 2
            || self
                .nogood
                .iter()
                .any(|atomic| truth_value(atomic, state) == Some(false));

        if is_consistent {
            RetentionCheck::NothingToPropagate
        } else {
            log::error!(
                "The nogood {:?} is not unit-propagation consistent; truth values: {:?}",
                self.nogood,
                self.nogood
                    .iter()
                    .map(|atomic| (atomic, truth_value(atomic, state)))
                    .collect::<Vec<_>>()
            );
            RetentionCheck::PropagationMissed
        }
    }
}
