use std::borrow::Cow;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::HypercubeLinearChecker;

use super::HypercubeLinearDescription;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;

/// The rule of the hypercube linear constraint.
#[derive(Clone, Copy, Debug)]
pub struct HypercubeLinearRule;

impl ConflictRule for HypercubeLinearRule {
    type Description = HypercubeLinearDescription;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("hypercube_linear")
    }

    fn create_conflict_checker(
        constraint_description: &HypercubeLinearDescription,
    ) -> impl ConflictChecker<Predicate> + 'static {
        HypercubeLinearChecker {
            hypercube: constraint_description.hypercube.iter_predicates().collect(),
            terms: constraint_description.linear.terms().collect(),
            bound: constraint_description.linear.bound(),
        }
    }
}
