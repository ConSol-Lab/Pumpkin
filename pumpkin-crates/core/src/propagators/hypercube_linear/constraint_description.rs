use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::containers::KeyGenerator;
use crate::propagation::ConstraintDescription;
use crate::propagation::LocalId;
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
