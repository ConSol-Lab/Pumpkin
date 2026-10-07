//! The runtime checks of [`State`]. They are a debugging tool:
//! - the inference checks under `check-inferences` or `check-inferences-proof`, which run when a
//!   propagation is posted, when a propagator reports a conflict, or when conflict analysis
//!   computes a reason;
//! - the retention checks under `check-retention`, which run after a propagator call in which the
//!   propagator did not enqueue itself again, and at a fixpoint;
//! - the solution check under `check-solutions`, which runs when the solver finds a solution.

#[cfg(feature = "inference-checkers")]
use crate::checkers::Inference;
#[cfg(feature = "check-retention")]
use crate::checkers::RetentionCoverage;
#[cfg(feature = "check-retention")]
use crate::checkers::RetentionFailure;
#[cfg(feature = "check-solutions")]
use crate::checkers::SolutionFailure;
#[cfg(feature = "inference-checkers")]
use crate::checkers::check_inference;
#[cfg(feature = "inference-checkers")]
use crate::engine::reason::ReasonRef;
#[cfg(feature = "check-retention")]
use crate::propagation::Domains;
#[cfg(feature = "check-retention")]
use crate::propagation::PropagatorId;
#[cfg(feature = "check-solutions")]
use crate::propagation::SolutionCheck;
#[cfg(feature = "inference-checkers")]
use crate::state::PropagatorConflict;
use crate::state::State;

#[cfg(feature = "inference-checkers")]
impl State {
    /// Check an inference whose reason conflict analysis computed: every such inference under
    /// `check-inferences-proof`, and only those with a lazy reason under `check-inferences`, since
    /// the others were checked when they were posted.
    pub(crate) fn check_explained_inference(
        &self,
        reason_reference: ReasonRef,
        inference: Inference<'_>,
    ) {
        let is_lazy = self.reason_store.get_lazy_code(reason_reference).is_some();

        if cfg!(feature = "check-inferences-proof") || is_lazy {
            check_inference(
                &self.assignments,
                &self.rule_checkers.conflict_checkers,
                inference,
            );
        }
    }

    /// Check a conflict that a propagator reported: under `check-inferences` when the propagator
    /// returns it, under `check-inferences-proof` when conflict analysis starts from it.
    pub(crate) fn check_reported_conflict(&self, conflict: &PropagatorConflict) {
        check_inference(
            &self.assignments,
            &self.rule_checkers.conflict_checkers,
            Inference {
                premises: conflict.conjunction.as_slice(),
                consequent: None,
                consequent_position: None,
                inference_code: conflict.inference_code,
            },
        );
    }
}

#[cfg(feature = "check-retention")]
impl State {
    /// Make the retention checkers that watch a domain changed since `start_index` pending.
    pub(super) fn notify_retention_checkers(&mut self, start_index: usize) {
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
    }

    /// Run the pending retention checkers of `propagator`, which did not enqueue itself again
    /// after it was called and so reports that it is at a fixpoint.
    ///
    /// Panics if one of them reports that the propagator has something left to propagate.
    pub(super) fn run_retention_checkers_of(&mut self, propagator: PropagatorId) {
        let outcome = self.rule_checkers.retention_checkers.run_pending_of(
            propagator,
            Domains::new(&self.assignments, &mut self.trailed_values),
        );

        if let Err(failure) = outcome {
            self.report_retention_failure(&failure, RetentionMoment::AfterCall);
        }
    }

    /// Run the retention checkers that are still pending when propagation reaches a fixpoint:
    /// those of the propagators that were not called after their domains changed. Under
    /// `check-retention-all`, run every retention checker.
    ///
    /// Panics if one of them reports that its propagator has something left to propagate.
    pub(super) fn run_retention_checkers_at_fixpoint(&mut self) {
        let coverage = if cfg!(feature = "check-retention-all") {
            RetentionCoverage::All
        } else {
            RetentionCoverage::Notified
        };
        let outcome = self.rule_checkers.retention_checkers.run(
            coverage,
            Domains::new(&self.assignments, &mut self.trailed_values),
        );

        if let Err(failure) = outcome {
            self.report_retention_failure(&failure, RetentionMoment::AtFixpoint);
        }
    }

    /// Panics, naming the rule and the propagator that is not finished and the variables the
    /// checker watches.
    fn report_retention_failure(&self, failure: &RetentionFailure, moment: RetentionMoment) -> ! {
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

        let failure = match moment {
            RetentionMoment::AfterCall => format!(
                "The propagator '{propagator}' did not enqueue itself again after it was called, \
                 but the retention checker of its rule '{rule}' reports that it still has \
                 something to propagate over {variables}."
            ),
            RetentionMoment::AtFixpoint => format!(
                "Propagation reached a fixed point, but the retention checker of the rule \
                 '{rule}' of the propagator '{propagator}' reports that it still has something \
                 to propagate over {variables}. The propagator was not called after these \
                 domains changed: check the events it registers for and its notify."
            ),
        };

        panic!(
            "{failure} The checker describes what it expected in a message logged at the error \
             level, which is only visible when a logger is installed."
        )
    }
}

/// When a retention checker found that its propagator is not finished.
#[cfg(feature = "check-retention")]
#[derive(Clone, Copy, Debug)]
enum RetentionMoment {
    /// After a call of the propagator in which it did not enqueue itself again.
    AfterCall,
    /// When propagation reached a fixpoint.
    AtFixpoint,
}

#[cfg(feature = "check-solutions")]
impl State {
    /// Check that the assignment satisfies the description of every constraint.
    ///
    /// Panics, naming the rule and the propagator of the first constraint it does not satisfy.
    pub fn check_solution(&self) {
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
            SolutionCheck::ConstraintViolated => panic!(
                "The solver reported a solution that violates the constraint of the rule '{rule}' \
                 of the propagator '{propagator}'."
            ),
            SolutionCheck::Unknown => panic!(
                "The solver reported a solution in which the constraint of the rule '{rule}' of \
                 the propagator '{propagator}' depends on variables that are not fixed."
            ),
            SolutionCheck::ConstraintSatisfied => {
                unreachable!("a satisfied constraint is not a failure")
            }
        }
    }
}
