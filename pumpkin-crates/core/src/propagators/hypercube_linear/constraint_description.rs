use pumpkin_checking::CheckerVariable;
use pumpkin_checking::DomainView;
use pumpkin_checking::IntExt;

use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;
use crate::propagators::hypercube_linear::Hypercube;
use crate::propagators::hypercube_linear::LinearInequality;

crate::scoped_struct! {
/// The description of the hypercube linear constraint: when every predicate of the hypercube
/// holds, the linear inequality holds.
#[derive(Clone, Debug)]
pub struct HypercubeLinearDescription {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
}
}

impl ConstraintDescription for HypercubeLinearDescription {
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let bound = i64::from(self.linear.bound());
        let highest_sum = self
            .linear
            .terms()
            .map(|term| IntExt::<i64>::from(term.induced_upper_bound(domains)))
            .sum::<IntExt<i64>>();
        let lowest_sum = self
            .linear
            .terms()
            .map(|term| IntExt::<i64>::from(term.induced_lower_bound(domains)))
            .sum::<IntExt<i64>>();

        let is_hypercube_violated = self
            .hypercube
            .iter_predicates()
            .any(|predicate| domains.is_true(&!predicate));
        let is_hypercube_satisfied = self
            .hypercube
            .iter_predicates()
            .all(|predicate| domains.is_true(&predicate));

        if is_hypercube_violated || highest_sum <= bound {
            SolutionCheck::ConstraintSatisfied
        } else if is_hypercube_satisfied && lowest_sum > bound {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::Unknown
        }
    }
}
