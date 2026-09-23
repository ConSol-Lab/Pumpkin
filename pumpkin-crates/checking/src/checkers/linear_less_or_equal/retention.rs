use super::LinearLessOrEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionChecker;
use crate::VariableState;
use crate::checkers::retention_checker::scope_lower_bound;
use crate::checkers::retention_checker::scope_upper_bound;

impl<Var, Atomic> RetentionChecker<Atomic> for LinearLessOrEqualChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        // 1. Check if the constraint is conflicting
        let bound = i64::from(self.bound);
        let lower_bound_sum = self
            .terms
            .iter()
            .map(|term| i64::from(scope_lower_bound(term, state)))
            .sum::<i64>();

        if lower_bound_sum > bound {
            log::error!(
                "The lower bounds of {:?} exceed the bound {} of the linear inequality",
                self.terms,
                self.bound
            );
            return false;
        }

        // 2. Assert that it is possible to assign the greatest value in the domain for each
        //    variable
        //  We do this by effectively assigning the greatest value to the variable whilst keeping
        //  the other variables at their lower bound.
        self.terms.iter().all(|term| {
            let greatest = bound - (lower_bound_sum - i64::from(scope_lower_bound(term, state)));
            let is_tight = i64::from(scope_upper_bound(term, state)) <= greatest;

            if !is_tight {
                log::error!(
                    "The upper bound of {term:?} could be lowered to {greatest} by the linear inequality {:?} <= {}",
                    self.terms,
                    self.bound
                );
            }

            is_tight
        })
    }
}
