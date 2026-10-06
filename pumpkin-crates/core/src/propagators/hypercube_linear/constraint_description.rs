use pumpkin_checking::CheckerVariable;
use pumpkin_checking::DomainView;

use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::containers::KeyGenerator;
use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::LocalId;
use crate::propagation::SolutionCheck;
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

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let mut is_hypercube_satisfied = true;
        for predicate in self.hypercube.iter_predicates() {
            if domains.fixed_value(&predicate.get_domain()).is_none() {
                return SolutionCheck::UnfixedVariable;
            }
            is_hypercube_satisfied &= domains.is_true(&predicate);
        }

        let Some(sum) = self
            .linear
            .terms()
            .map(|term| term.induced_fixed_value(domains).map(i64::from))
            .sum::<Option<i64>>()
        else {
            return SolutionCheck::UnfixedVariable;
        };

        if !is_hypercube_satisfied || sum <= i64::from(self.linear.bound()) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
