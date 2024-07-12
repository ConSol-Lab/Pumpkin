use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::variables::DomainId;
use AssignmentsInteger;

/// A trait which specifies the common behaviours of [`Solution`] and [`SolutionReference`].
pub trait ProblemSolution: HasAssignments {
    /// Returns the number of defined [`DomainId`]s.
    fn num_domains(&self) -> usize {
        self.assignments_integer().num_domains() as usize
    }

    fn get_integer_value(&self, domain: DomainId) -> i32 {
        self.assignments_integer()
            .get_assigned_value(domain)
            .expect("Expected retrieved integer variable from solution to be assigned")
    }
}

/// A solution which keeps reference to its inner structures.
#[derive(Debug, Copy, Clone)]
pub struct SolutionReference<'a> {
    assignments_integer: &'a AssignmentsInteger,
}

impl<'a> SolutionReference<'a> {
    pub fn new(assignments_integer: &'a AssignmentsInteger) -> SolutionReference<'a> {
        SolutionReference {
            assignments_integer,
        }
    }
}

impl<'a> ProblemSolution for SolutionReference<'a> {}

/// A solution which takes ownership of its inner structures.
#[derive(Debug)]
pub struct Solution {
    assignments_integer: AssignmentsInteger,
}

impl Solution {
    pub fn new(assignments_integer: AssignmentsInteger) -> Self {
        Self {
            assignments_integer,
        }
    }

    pub(crate) fn as_reference(&self) -> SolutionReference<'_> {
        SolutionReference {
            assignments_integer: &self.assignments_integer,
        }
    }
}

impl ProblemSolution for Solution {}

impl<'a> From<SolutionReference<'a>> for Solution {
    fn from(value: SolutionReference) -> Self {
        Self {
            assignments_integer: value.assignments_integer.clone(),
        }
    }
}

impl<'a> HasAssignments for SolutionReference<'a> {
    fn assignments_integer(&self) -> &'a AssignmentsInteger {
        self.assignments_integer
    }
}

impl HasAssignments for Solution {
    fn assignments_integer(&self) -> &AssignmentsInteger {
        &self.assignments_integer
    }
}
