//! The runtime checks of [`State`]. They are a debugging tool:
//! - the conflict checks under `check-propagations` and the retention checks under
//!   `check-consistency`, which run when [`State::propagate_to_fixed_point`] returns;
//! - the solution check under `check-solutions`, which runs when the solver finds a solution.

#[cfg(feature = "check-consistency")]
use crate::checkers::RetentionCoverage;
#[cfg(feature = "check-consistency")]
use crate::checkers::RetentionFailure;
#[cfg(feature = "check-solutions")]
use crate::checkers::SolutionFailure;
#[cfg(feature = "check-propagations")]
use crate::checkers::is_rule_checked;
#[cfg(feature = "check-propagations")]
use crate::predicates::Predicate;
#[cfg(feature = "check-propagations")]
use crate::proof::InferenceCode;
#[cfg(feature = "check-consistency")]
use crate::propagation::Domains;
#[cfg(feature = "check-propagations")]
use crate::propagation::ExplanationContext;
#[cfg(feature = "check-solutions")]
use crate::propagation::SolutionCheck;
#[cfg(feature = "check-propagations")]
use crate::state::Conflict;
use crate::state::State;

#[cfg(feature = "check-propagations")]
impl State {
    /// Check every propagation on the trail from `start_index`, and the conflict when
    /// propagation ended in one that a propagator reported.
    ///
    /// Panics when a check fails.
    pub(super) fn check_inferences(&mut self, start_index: usize, result: &Result<(), Conflict>) {
        let mut reason = vec![];

        for trail_index in start_index..self.assignments.num_trail_entries() {
            let entry = self.assignments.get_trail_entry(trail_index);

            // A decision has no reason, and is not an inference.
            let Some(reason_reference) = entry.reason else {
                continue;
            };

            reason.clear();
            let inference_code = self.reason_store.get_or_compute(
                reason_reference,
                ExplanationContext::without_working_nogood(
                    &self.assignments,
                    trail_index,
                    &mut self.notification_engine,
                ),
                &mut self.propagators,
                &mut reason,
            );

            if let Err(invalid_inference) =
                self.check_timing(&reason, Some((entry.predicate, trail_index)))
            {
                panic!(
                    "the inference {reason:?} -> {} with inference code {inference_code:?} is \
                     invalid in the solver state: {invalid_inference:?}",
                    entry.predicate
                );
            }

            self.run_checker(&reason, Some(entry.predicate), inference_code);
        }

        if let Err(Conflict::Propagator(conflict)) = result {
            let premises = conflict.conjunction.iter().copied().collect::<Vec<_>>();

            if let Err(invalid_inference) = self.check_timing(&premises, None) {
                panic!(
                    "the conflict {premises:?} with inference code {:?} is invalid in the solver \
                     state: {invalid_inference:?}",
                    conflict.inference_code
                );
            }

            self.run_checker(&premises, None, conflict.inference_code);
        }
    }

    /// Check that every premise is true, and for a propagation, that each premise was true
    /// before the propagation at the given trail index and that the propagated predicate is
    /// true.
    fn check_timing(
        &self,
        premises: &[Predicate],
        propagation: Option<(Predicate, usize)>,
    ) -> Result<(), InvalidInference> {
        for &premise in premises {
            if self.assignments.evaluate_predicate(premise) != Some(true) {
                return Err(InvalidInference::UnsatisfiedPremise(premise));
            }

            if let Some((_, trail_index)) = propagation
                && self
                    .assignments
                    .get_trail_position(&premise)
                    .is_some_and(|premise_position| premise_position >= trail_index)
            {
                return Err(InvalidInference::PremiseAfterConsequent(premise));
            }
        }

        if let Some((consequent, _)) = propagation
            && self.assignments.evaluate_predicate(consequent) != Some(true)
        {
            return Err(InvalidInference::ConsequentNotApplied(consequent));
        }

        Ok(())
    }

    /// Run the conflict checkers of `inference_code` on the inference.
    ///
    /// Panics when the rule has no checker, or when none of its checkers accepts the inference.
    fn run_checker(
        &self,
        premises: &[Predicate],
        consequent: Option<Predicate>,
        inference_code: InferenceCode,
    ) {
        if !is_rule_checked(self.rule_name(inference_code)) {
            return;
        }

        let checkers = self
            .rule_checkers
            .conflict_checkers
            .for_inference_code(&inference_code);
        assert!(
            checkers.len() > 0,
            "missing checker for inference code {inference_code:?}"
        );

        let results = checkers
            .map(|checker| checker.check_inference(premises, consequent.as_ref()))
            .collect::<Vec<_>>();

        assert!(
            results.iter().any(Result::is_ok),
            "checker for inference code {inference_code:?} fails on inference {premises:?} -> {consequent:?}: {results:?}"
        );
    }
}

/// Why an inference is invalid in the solver state, independent of its rule.
#[cfg(feature = "check-propagations")]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InvalidInference {
    /// The premise is not true.
    UnsatisfiedPremise(Predicate),
    /// The premise became true only at or after the propagation it explains.
    PremiseAfterConsequent(Predicate),
    /// The propagated predicate is not true after the propagation.
    ConsequentNotApplied(Predicate),
}

#[cfg(feature = "check-consistency")]
impl State {
    /// Ask every retention checker watching a domain that changed since `start_index` whether its
    /// propagator has anything left to propagate.
    ///
    /// The other checkers were asked when their domains last changed.
    /// This panics if a propagator reports that it has not finished.
    pub(super) fn run_retention_checkers(&mut self, start_index: usize) {
        for index in start_index..self.assignments.num_trail_entries() {
            let domain = self
                .assignments
                .get_trail_entry(index)
                .predicate
                .get_domain();
            self.rule_checkers
                .retention_checkers
                .on_domain_event(domain);
        }

        let coverage = if cfg!(feature = "check-consistency-all") {
            RetentionCoverage::All
        } else {
            RetentionCoverage::Notified
        };
        let outcome = self.rule_checkers.retention_checkers.run(
            coverage,
            Domains::new(&self.assignments, &mut self.trailed_values),
        );

        if let Err(failure) = outcome {
            self.report_retention_failure(&failure);
        }
    }

    /// Panics, naming the rule and the propagator that is not finished and the variables it
    /// watches.
    fn report_retention_failure(&self, failure: &RetentionFailure) -> ! {
        let rule = self.rule_name(failure.inference_code);
        let propagator = self.propagators[failure.propagator].name();
        let variables = failure
            .variables
            .iter()
            .map(|&domain| match self.variable_names.get_int_name(domain) {
                Some(name) => name.to_owned(),
                None => format!("{domain:?}"),
            })
            .collect::<Vec<_>>()
            .join(", ");

        panic!(
            "Propagation reported a fixed point, but the retention checker of the rule '{rule}' \
             of the propagator '{propagator}' reports that it still has something to propagate \
             over {variables}. \
             The checker describes what it expected in a message logged at the error level, \
             which is only visible when a logger is installed."
        )
    }
}

#[cfg(feature = "check-solutions")]
impl State {
    /// Check that the assignment satisfies the description of every constraint.
    ///
    /// Panics, naming the rule and the propagator of the first constraint it does not satisfy.
    pub(crate) fn check_solution(&self) {
        if let Err(failure) = self
            .rule_checkers
            .solution_checkers
            .check(&self.assignments)
        {
            self.report_solution_failure(&failure);
        }
    }

    fn report_solution_failure(&self, failure: &SolutionFailure) -> ! {
        let rule = self.rule_name(failure.inference_code);
        let propagator = self.propagators[failure.propagator].name();

        match failure.outcome {
            SolutionCheck::UnfixedVariable => panic!(
                "The solver reported a solution, but a variable of the constraint of the rule \
                 '{rule}' of the propagator '{propagator}' is not fixed."
            ),
            SolutionCheck::ConstraintSatisfied => {
                unreachable!("a satisfied constraint is not a failure")
            }
            SolutionCheck::ConstraintViolated => panic!(
                "The solver reported a solution that violates the constraint of the rule '{rule}' \
                 of the propagator '{propagator}'."
            ),
        }
    }
}
