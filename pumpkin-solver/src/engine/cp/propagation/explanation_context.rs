use crate::engine::Assignments;

/// The context that is available when lazily explaining propagations.
///
/// See [`pumpkin_solver::engine::propagation::Propagator`] for more information.
pub(crate) struct ExplanationContext<'a> {
    assignments: &'a Assignments,
}

impl<'a> ExplanationContext<'a> {
    pub(crate) fn new(assignments: &'a Assignments) -> Self {
        ExplanationContext { assignments }
    }

    /// Get the underlying assignments.
    #[deprecated = "using the assignments directly is not ideal, and we should develop this context API further instead"]
    pub(crate) fn assignments(&self) -> &'a Assignments {
        self.assignments
    }
}
