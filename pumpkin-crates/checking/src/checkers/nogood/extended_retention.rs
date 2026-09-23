use std::collections::BTreeSet;
use std::fmt::Debug;

use super::ExtendedNogoodChecker;
use super::truth_value;
use crate::AtomicConstraint;
use crate::Comparison;
use crate::RetentionChecker;
use crate::VariableState;

impl<Atomic> RetentionChecker<Atomic> for ExtendedNogoodChecker<Atomic>
where
    Atomic: AtomicConstraint,
    Atomic::Identifier: Debug,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        // 1. Determine the variables with a predicate which is not true; if there are none then the
        //    nogood is conflicting
        let free_domains = self
            .nogood
            .iter()
            .filter(|atomic| truth_value(*atomic, state) != Some(true))
            .map(|atomic| atomic.identifier())
            .collect::<Vec<_>>();
        if free_domains.is_empty() {
            log::error!(
                "The nogood {:?} holds; it should have been reported as a conflict",
                self.nogood
            );
            return false;
        }

        // 2. If predicates over at least two variables are not true then nothing can be propagated
        let free_domain = &free_domains[0];
        if free_domains.iter().any(|domain| domain != free_domain) {
            return true;
        }

        // 3. Determine the values of the remaining variable which satisfy all of its predicates
        let mut lower = state.lower_bound(free_domain).as_int().unwrap_or_else(|| {
            panic!("{free_domain:?} is not bounded below in the retention check")
        });
        let mut upper = state.upper_bound(free_domain).as_int().unwrap_or_else(|| {
            panic!("{free_domain:?} is not bounded above in the retention check")
        });
        let mut excluded: BTreeSet<i32> = state.holes(free_domain).collect();
        for atomic in self
            .nogood
            .iter()
            .filter(|atomic| atomic.identifier() == *free_domain)
        {
            let value = atomic.value();
            match atomic.comparison() {
                Comparison::GreaterEqual => lower = lower.max(value),
                Comparison::LessEqual => upper = upper.min(value),
                Comparison::NotEqual => {
                    let _ = excluded.insert(value);
                }
                Comparison::Equal => {
                    lower = lower.max(value);
                    upper = upper.min(value);
                }
            }
        }

        // 4. Assert that none of these values remain in the domain
        let num_values = (i64::from(upper) - i64::from(lower) + 1).max(0);
        let num_excluded = excluded
            .iter()
            .filter(|&&value| lower <= value && value <= upper)
            .count() as i64;
        let no_value_allowed = num_excluded == num_values;

        if !no_value_allowed {
            log::error!(
                "The values of {free_domain:?} in [{lower}, {upper}] could be removed by the nogood {:?}",
                self.nogood
            );
        }

        no_value_allowed
    }
}
