use crate::engine::Assignments;

/// [`PropagationContext`] is passed to propagators during propagation.
/// It may be queried to retrieve information about the current variable domains such as the
/// lower-bound of a particular variable, or used to apply changes to the domain of a variable
/// e.g. set `[x >= 5]`.
///
///
/// Note that the [`PropagationContext`] is the only point of communication beween
/// the propagations and the solver during propagation.
#[derive(Clone, Copy, Debug)]
pub struct PropagationContext<'a> {
    pub(crate) assignments: &'a Assignments,
}

impl<'a> PropagationContext<'a> {
    pub(crate) fn new(assignments: &'a Assignments) -> Self {
        PropagationContext { assignments }
    }
}
