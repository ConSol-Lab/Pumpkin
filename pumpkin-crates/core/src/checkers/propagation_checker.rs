use pumpkin_checking::BoxedChecker;
use pumpkin_checking::VariableState;

use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;
use crate::variables::DomainId;

/// Tests whether an inference is correct given the solver state.
///
/// An inference is correct when:
/// 1. All premises are satisfied.
/// 2. The conjunction of the premises and negation of the consequent is consistent.
/// 3. The consequent is logically entailed given the inference code.
#[derive(Clone, Debug)]
pub struct PropagationChecker {
    inference_checker: BoxedChecker<Predicate>,
}

impl PropagationChecker {
    /// Create a new propagation checker given an inference checker and inference code.
    pub fn new(inference_checker: BoxedChecker<Predicate>) -> PropagationChecker {
        PropagationChecker { inference_checker }
    }

    /// Run the propagation checker for the given inference.
    pub fn check(
        &self,
        premises: &[Predicate],
        consequent: Option<Predicate>,
        domains: Domains<'_>,
    ) -> Result<(), InvalidInference> {
        let premises_satisfied = premises
            .iter()
            .all(|&premise| domains.evaluate_predicate(premise) == Some(true));

        if !premises_satisfied {
            return Err(InvalidInference::UnsatisfiedPremises);
        }

        let variable_state =
            VariableState::prepare_for_conflict_check(premises.iter().copied(), consequent)
                .map_err(InvalidInference::InconsistentPredicates)?;

        if self
            .inference_checker
            .check(variable_state, premises, consequent.as_ref())
        {
            Ok(())
        } else {
            Err(InvalidInference::Unsound)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InvalidInference {
    /// Not all premises are true given the current state.
    UnsatisfiedPremises,
    /// The predicates that make up the inference are trivially inconsistent.
    InconsistentPredicates(DomainId),
    /// Cannot establish that the inference is sound.
    Unsound,
}
