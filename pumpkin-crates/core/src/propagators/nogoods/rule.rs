use std::borrow::Cow;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ExtendedNogoodChecker;
use pumpkin_checking::checkers::NogoodChecker;

use crate::checkers::Scope;
use crate::containers::HashSet;
use crate::containers::KeyGenerator;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;
use crate::propagation::ConstraintDescription;
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
}

/// The rule of a nogood under unit propagation.
#[derive(Clone, Copy, Debug)]
pub struct UnitNogoodRule;

impl ConflictRule for UnitNogoodRule {
    type Description = NogoodDescription;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed(NogoodChecker::<Predicate>::RULE_NAME)
    }

    fn create_inference_checker(
        description: &NogoodDescription,
    ) -> impl InferenceChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: description.nogood.clone(),
        }
    }

    fn create_retention_checker(
        description: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: description.nogood.clone(),
        }
    }
}

/// The rule of a nogood under extended nogood propagation.
///
/// Its inferences are those of [`UnitNogoodRule`], under the same name; only its retention
/// checker is stronger, since extended propagation removes more values.
#[derive(Clone, Copy, Debug)]
pub struct ExtendedNogoodRule;

impl ConflictRule for ExtendedNogoodRule {
    type Description = NogoodDescription;

    fn name() -> Cow<'static, str> {
        UnitNogoodRule::name()
    }

    fn create_inference_checker(
        description: &NogoodDescription,
    ) -> impl InferenceChecker<Predicate> + 'static {
        UnitNogoodRule::create_inference_checker(description)
    }

    fn create_retention_checker(
        description: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        ExtendedNogoodChecker {
            nogood: description.nogood.clone(),
        }
    }
}
