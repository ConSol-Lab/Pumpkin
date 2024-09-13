use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::variables::DomainGeneratorIterator;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::engine::Assignments;
use crate::predicates::Predicate;

/// A trait which specifies the common behaviours of [`Solution`] and [`SolutionReference`].
pub trait ProblemSolution: HasAssignments {
    /// Returns the number of defined [`DomainId`]s.
    fn num_domains(&self) -> usize {
        self.assignments().num_domains() as usize
    }

    fn get_integer_value(&self, domain: DomainId) -> i32 {
        self.assignments()
            .get_assigned_value(domain)
            .expect("Expected retrieved integer variable from solution to be assigned")
    }

    fn get_literal_value(&self, literal: Literal) -> bool {
        self.assignments()
            .evaluate_predicate(literal.get_true_predicate())
            .expect("Expected to retrieve concrete truth value from solution to be assigned.")
    }
}

/// A solution which keeps reference to its inner structures.
#[derive(Debug, Copy, Clone)]
pub struct SolutionReference<'a> {
    assignments: &'a Assignments,
}

impl<'a> SolutionReference<'a> {
    pub fn new(assignments: &'a Assignments) -> SolutionReference<'a> {
        SolutionReference { assignments }
    }
}

impl<'a> ProblemSolution for SolutionReference<'a> {}

/// A solution which takes ownership of its inner structures.
#[derive(Debug, Default)]
pub struct Solution {
    assignments: Assignments,
}

impl Solution {
    pub fn new(assignments: Assignments) -> Self {
        Self { assignments }
    }

    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.assignments.get_domains()
        // todo: Should we skip the first element as it could be the always true domain id?
    }

    pub fn as_reference(&self) -> SolutionReference<'_> {
        SolutionReference {
            assignments: &self.assignments,
        }
    }

    pub fn contains_domain_id(&self, domain_id: DomainId) -> bool {
        domain_id.id < self.assignments.num_domains()
    }

    pub fn is_predicate_satisfied(&self, predicate: Predicate) -> bool {
        self.assignments.is_predicate_satisfied(predicate)
    }
}

impl ProblemSolution for Solution {}

impl<'a> From<SolutionReference<'a>> for Solution {
    fn from(value: SolutionReference) -> Self {
        Self {
            assignments: value.assignments.clone(),
        }
    }
}

impl<'a> HasAssignments for SolutionReference<'a> {
    fn assignments(&self) -> &'a Assignments {
        self.assignments
    }
}

impl HasAssignments for Solution {
    fn assignments(&self) -> &Assignments {
        &self.assignments
    }
}
