use crate::engine::Assignments;
use crate::propagation::HasAssignments;
#[cfg(doc)]
use crate::propagation::ReadDomains;

/// Provides access to domain information to propagators.
///
/// Implements [`ReadDomains`] to expose information about the current variable domains such as the
/// lower-bound of a particular variable.
#[derive(Clone, Copy, Debug)]
pub struct Domains<'a> {
    pub(crate) assignments: &'a Assignments,
}

impl<'a> Domains<'a> {
    pub(crate) fn new(assignments: &'a Assignments) -> Self {
        Domains { assignments }
    }
}

impl HasAssignments for Domains<'_> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }
}
