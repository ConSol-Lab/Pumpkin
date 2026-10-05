use std::fmt::Debug;

use super::HypercubeLinearChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

impl<Atomic, Var> InferenceChecker<Atomic> for HypercubeLinearChecker<Atomic, Var>
where
    Atomic: AtomicConstraint + Clone + Debug,
    Var: CheckerVariable<Atomic>,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        let hypercube_satisfied = self.hypercube.iter().all(|atomic| state.is_true(atomic));

        let term_sum = self
            .terms
            .iter()
            .map(|term| IntExt::<i64>::from(term.induced_lower_bound(&state)))
            .sum::<IntExt<i64>>();

        let linear_slack = i64::from(self.bound) - term_sum;
        let linear_conflicting = linear_slack < 0;

        hypercube_satisfied && linear_conflicting
    }
}
