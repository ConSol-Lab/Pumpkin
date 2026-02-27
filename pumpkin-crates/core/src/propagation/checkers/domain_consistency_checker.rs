use pumpkin_checking::InferenceChecker;

use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::checkers::ConsistencyChecker;
use crate::propagation::checkers::Scope;
use crate::propagation::checkers::WitnessGenerator;

#[derive(Clone, Debug)]
pub struct DomainConsistencyChecker<C> {
    witness_generator: C,
}

impl<C> DomainConsistencyChecker<C> {
    pub fn new(witness_generator: C) -> Self {
        DomainConsistencyChecker { witness_generator }
    }
}

impl<C> ConsistencyChecker for DomainConsistencyChecker<C>
where
    C: WitnessGenerator + InferenceChecker<Predicate> + Clone,
{
    fn check_consistency(&self, domains: Domains<'_>, scope: &Scope) -> bool {
        super::assert_consistency(
            domains,
            scope,
            &self.witness_generator,
            super::Consistency::Domain,
        );

        true
    }
}
