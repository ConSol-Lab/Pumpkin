use crate::checkers::ConflictCheckerStore;
use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// An inference to check: the premises imply the consequent or, without a consequent, are a
/// conflict.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Inference<'a> {
    pub(crate) premises: &'a [Predicate],
    pub(crate) consequent: Option<Predicate>,
    /// The trail position at which the consequent was applied; `None` when it was not applied,
    /// such as for a propagation that empties a domain.
    pub(crate) consequent_position: Option<usize>,
    pub(crate) inference_code: InferenceCode,
}

/// Check that `inference` is valid in the solver state and that its rule accepts it.
///
/// Panics when a check fails, or when the rule has no conflict checker although it is checked.
pub(crate) fn check_inference(
    assignments: &Assignments,
    conflict_checkers: &ConflictCheckerStore,
    inference: Inference<'_>,
) {
    if let Err(invalid_inference) = check_timing(assignments, inference) {
        panic!(
            "the inference {:?} -> {:?} with inference code {:?} is invalid in the solver state: \
             {invalid_inference:?}",
            inference.premises, inference.consequent, inference.inference_code
        );
    }

    let inference_code = inference.inference_code;
    let checkers = conflict_checkers.for_inference_code(&inference_code);
    if checkers.len() == 0 {
        // An inference made without a rule, as tests do, can only be checked when a test adds a
        // checker for it.
        let rule = inference_code.rule();
        assert!(
            conflict_checkers.is_rule_unchecked(rule) || rule.is_unknown(),
            "missing checker for inference code {inference_code:?}"
        );
        return;
    }

    let results = checkers
        .map(|checker| checker.check_inference(inference.premises, inference.consequent.as_ref()))
        .collect::<Vec<_>>();

    assert!(
        results.iter().any(Result::is_ok),
        "checker for inference code {inference_code:?} fails on inference {:?} -> {:?}: \
         {results:?}",
        inference.premises,
        inference.consequent
    );
}

/// Check that every premise is true, and for an applied consequent, that each premise was true
/// before the consequent was applied and that the consequent is true.
fn check_timing(
    assignments: &Assignments,
    inference: Inference<'_>,
) -> Result<(), InvalidInference> {
    for &premise in inference.premises {
        if assignments.evaluate_predicate(premise) != Some(true) {
            return Err(InvalidInference::UnsatisfiedPremise(premise));
        }

        if let Some(consequent_position) = inference.consequent_position
            && assignments
                .get_trail_position(&premise)
                .is_some_and(|premise_position| premise_position >= consequent_position)
        {
            return Err(InvalidInference::PremiseAfterConsequent(premise));
        }
    }

    if let (Some(consequent), Some(_)) = (inference.consequent, inference.consequent_position)
        && assignments.evaluate_predicate(consequent) != Some(true)
    {
        return Err(InvalidInference::ConsequentNotApplied(consequent));
    }

    Ok(())
}

/// Why an inference is invalid in the solver state, independent of its rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InvalidInference {
    /// The premise is not true.
    UnsatisfiedPremise(Predicate),
    /// The premise became true only at or after the propagation it explains.
    PremiseAfterConsequent(Predicate),
    /// The propagated predicate is not true after the propagation.
    ConsequentNotApplied(Predicate),
}
