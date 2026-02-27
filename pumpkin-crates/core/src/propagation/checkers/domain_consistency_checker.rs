use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::containers::HashMap;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;
use crate::propagation::checkers::ConsistencyChecker;
use crate::propagation::checkers::Scope;
use crate::propagation::checkers::Witness;
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

impl<C: InferenceChecker<Predicate>> DomainConsistencyChecker<C> {
    /// Validate that the witness is a valid solution.
    fn validate_witness(&self, witness: &Witness) -> bool {
        let premises = witness
            .iter()
            .map(|(domain, value)| predicate![domain == value])
            .collect::<Vec<_>>();

        let state = VariableState::prepare_for_conflict_check(premises.clone(), None)
            .expect("the witness is consistent by construction");

        let state_is_conflicting = self.witness_generator.check(state, &premises, None);

        !state_is_conflicting
    }
}

impl<C> ConsistencyChecker for DomainConsistencyChecker<C>
where
    C: WitnessGenerator + InferenceChecker<Predicate> + Clone,
{
    fn check_consistency(&self, domains: Domains<'_>, scope: &Scope) -> bool {
        // A propagator is domain consistent if all values for all variables participate in
        // at least one solution to the constraint.

        let mut supported_values = HashMap::new();

        for (local_id, domain_id) in scope.iter() {
            let supported_values_in_domain: Vec<_> =
                std::mem::take(supported_values.entry(domain_id).or_default());

            for value in domains.iterate_domain(&domain_id) {
                if supported_values_in_domain.contains(&value) {
                    // If the value is already supported, no need to generate a witness
                    // for it.
                    continue;
                }

                let witness = self.witness_generator.support(local_id, value.into());

                assert_eq!(
                    Some(value),
                    witness.value_for(domain_id),
                    "the witness for a variable assignment must contain that variable assignment"
                );

                assert!(
                    self.validate_witness(&witness),
                    "witness should satisfy the constraint"
                );

                // Add all the assigned variables as supported values as well.
                for (witness_domain_id, witness_value) in witness.iter() {
                    let supports = supported_values.entry(witness_domain_id).or_default();
                    supports.push(witness_value);
                }
            }

            let _ = supported_values
                .insert(domain_id, supported_values_in_domain)
                .expect("a element was inserted at the beginning of the loop");
        }

        true
    }
}
