use std::borrow::Cow;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::HypercubeLinearChecker;

use super::HypercubeLinearDescription;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;
use crate::variables::AffineView;
use crate::variables::DomainId;

/// The rule of the hypercube linear constraint.
#[derive(Clone, Copy, Debug)]
pub struct HypercubeLinearRule;

impl ConflictRule for HypercubeLinearRule {
    type Description = HypercubeLinearDescription;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("hypercube_linear")
    }

    fn create_inference_checker(
        description: &HypercubeLinearDescription,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &HypercubeLinearDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}

impl HypercubeLinearRule {
    fn checker(
        description: &HypercubeLinearDescription,
    ) -> HypercubeLinearChecker<Predicate, AffineView<DomainId>> {
        HypercubeLinearChecker {
            hypercube: description.hypercube.iter_predicates().collect(),
            terms: description.linear.terms().collect(),
            bound: description.linear.bound(),
        }
    }
}
