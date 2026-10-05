use std::borrow::Cow;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::HypercubeLinearChecker;

use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::containers::KeyGenerator;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;
use crate::propagation::ConstraintDescription;
use crate::propagation::LocalId;
use crate::propagation::MissingRetentionChecker;
use crate::propagators::hypercube_linear::Hypercube;
use crate::propagators::hypercube_linear::LinearInequality;

/// The description of the hypercube linear constraint: when every predicate of the hypercube
/// holds, the linear inequality holds.
#[derive(Clone, Debug)]
pub struct HypercubeLinearDescription {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
}

impl ConstraintDescription for HypercubeLinearDescription {
    fn scope(&self) -> Scope {
        let mut scope = Scope::default();
        let mut local_ids = KeyGenerator::<LocalId>::default();

        for predicate in self.hypercube.iter_predicates() {
            scope.add_domain(local_ids.next_key(), predicate.get_domain());
        }

        for term in self.linear.terms() {
            term.add_to_scope(&mut scope, local_ids.next_key());
        }

        scope
    }
}

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
        HypercubeLinearChecker {
            hypercube: description.hypercube.iter_predicates().collect(),
            terms: description.linear.terms().collect(),
            bound: description.linear.bound(),
        }
    }

    fn create_retention_checker(
        _: &HypercubeLinearDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        MissingRetentionChecker::todo("the hypercube linear rule")
    }
}
