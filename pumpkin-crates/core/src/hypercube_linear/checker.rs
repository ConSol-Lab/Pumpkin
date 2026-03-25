use std::fmt::Debug;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

#[derive(Debug, Clone)]
pub struct HypercubeLinearChecker<Atomic, Var> {
    pub hypercube: Vec<Atomic>,
    pub terms: Vec<Var>,
    pub bound: i32,
}

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
