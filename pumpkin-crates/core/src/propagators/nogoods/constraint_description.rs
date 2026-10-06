use pumpkin_checking::DomainView;

use crate::checkers::Scope;
use crate::containers::HashSet;
use crate::containers::KeyGenerator;
use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;
use crate::variables::DomainId;

/// The description of a nogood: the atomic constraints that cannot all hold at once.
#[derive(Clone, Debug)]
pub struct NogoodDescription {
    pub nogood: Box<[Predicate]>,
}

impl ConstraintDescription for NogoodDescription {
    fn scope(&self) -> Scope {
        let mut scope = Scope::default();
        let mut seen: HashSet<DomainId> = HashSet::default();
        let mut id_generator = KeyGenerator::default();

        for predicate in self.nogood.iter() {
            let domain = predicate.get_domain();
            if seen.insert(domain) {
                scope.add_domain(id_generator.next_key(), domain);
            }
        }

        scope
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        if self
            .nogood
            .iter()
            .any(|predicate| domains.fixed_value(&predicate.get_domain()).is_none())
        {
            return SolutionCheck::UnfixedVariable;
        }

        if self
            .nogood
            .iter()
            .all(|predicate| domains.is_true(predicate))
        {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::ConstraintSatisfied
        }
    }
}
