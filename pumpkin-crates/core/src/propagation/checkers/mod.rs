mod bound_consistency_checker;
mod consistency_checker;
mod domain_consistency_checker;
mod scope;
mod variable;
mod witness;
mod witness_generator;

pub use bound_consistency_checker::*;
pub use consistency_checker::*;
pub use domain_consistency_checker::*;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
pub use scope::*;
pub use variable::*;
pub use witness::*;
pub use witness_generator::*;

use crate::containers::HashMap;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::LocalId;
use crate::propagation::ReadDomains;
use crate::variables::DomainId;

#[deprecated = "only here to aid refactoring"]
#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
pub struct DefaultChecker;

#[allow(deprecated, reason = "only here to aid refactoring")]
impl ConsistencyChecker for DefaultChecker {
    fn check_consistency(&self, _: Domains<'_>, _: &Scope) -> bool {
        true
    }
}

enum Consistency {
    Bounds,
    Domain,
}

fn assert_consistency<PropagationChecker>(
    domains: Domains<'_>,
    scope: &Scope,
    witness_generator: &PropagationChecker,
    consistency: Consistency,
) where
    PropagationChecker: WitnessGenerator + InferenceChecker<Predicate>,
{
    let mut supported_values: HashMap<DomainId, Vec<i32>> = HashMap::default();

    for (local_id, domain_id) in scope.iter() {
        let _ = supported_values.entry(domain_id).or_insert(vec![]);

        match consistency {
            Consistency::Domain => {
                // A propagator is domain consistent if all values for all variables participate in
                // at least one solution to the constraint.
                for value in domains.iterate_domain(&domain_id) {
                    support_value(
                        &domains,
                        witness_generator,
                        local_id,
                        domain_id,
                        value,
                        &mut supported_values,
                    );
                }
            }
            Consistency::Bounds => {
                for value in [
                    domains.lower_bound(&domain_id),
                    domains.upper_bound(&domain_id),
                ] {
                    support_value(
                        &domains,
                        witness_generator,
                        local_id,
                        domain_id,
                        value,
                        &mut supported_values,
                    );
                }
            }
        }
    }
}

fn support_value<PropagationChecker>(
    domains: &Domains<'_>,
    witness_generator: &PropagationChecker,
    local_id: LocalId,
    domain_id: DomainId,
    value: i32,
    supported_values: &mut HashMap<DomainId, Vec<i32>>,
) where
    PropagationChecker: WitnessGenerator + InferenceChecker<Predicate>,
{
    if supported_values[&domain_id].contains(&value) {
        // If the value is already supported, no need to generate a witness
        // for it.
        return;
    }

    let witness = witness_generator.support(domains, local_id, value.into());

    assert_eq!(
        Some(value),
        witness.value_for(domain_id),
        "the witness for a variable assignment must contain that variable assignment"
    );

    assert!(
        validate_witness(witness_generator, &witness),
        "witness should satisfy the constraint"
    );

    // Add all the assigned variables as supported values as well.
    for (witness_domain_id, witness_value) in witness.iter() {
        assert!(
            domains.contains(&witness_domain_id, witness_value),
            "witness uses values outside domain"
        );

        let supports = supported_values.entry(witness_domain_id).or_default();
        supports.push(witness_value);
    }
}

fn validate_witness(checker: &impl InferenceChecker<Predicate>, witness: &Witness) -> bool {
    let premises = witness
        .iter()
        .map(|(domain, value)| predicate![domain == value])
        .collect::<Vec<_>>();

    let state = VariableState::prepare_for_conflict_check(premises.clone(), None)
        .expect("the witness is consistent by construction");

    let state_is_conflicting = checker.check(state, &premises, None);

    !state_is_conflicting
}
