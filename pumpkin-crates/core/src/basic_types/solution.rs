use crate::engine::Assignments;
use crate::engine::variables::DomainGeneratorIterator;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::propagation::HasAssignments;
use crate::variables::IntegerVariable;

/// A trait which specifies the common behaviours of [`Solution`] and [`SolutionReference`].
pub trait ProblemSolution {
    /// Returns the number of defined [`DomainId`]s.
    fn num_domains(&self) -> usize;

    fn get_integer_value<Var: IntegerVariable>(&self, var: Var) -> i32;

    fn get_literal_value(&self, literal: Literal) -> bool;
}

impl<T: HasAssignments> ProblemSolution for T {
    /// Returns the number of defined [`DomainId`]s.
    fn num_domains(&self) -> usize {
        self.assignments().num_domains() as usize
    }

    fn get_integer_value<Var: IntegerVariable>(&self, var: Var) -> i32 {
        self.assignments()
            .get_assigned_value(&var)
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
    pub(crate) fn new(assignments: &'a Assignments) -> SolutionReference<'a> {
        SolutionReference { assignments }
    }

    pub fn get_domains(&self) -> DomainGeneratorIterator {
        self.assignments.get_domains()
        // todo: Should we skip the first element as it could be the always true domain id?
    }
}

/// A solution which takes ownership of its inner structures.
///
/// Implements [`ProblemSolution`].
#[derive(Clone, Debug, Default)]
pub struct Solution {
    assignments: Assignments,
}

impl Solution {
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
        domain_id.id() < self.assignments.num_domains()
    }
}

impl From<Assignments> for Solution {
    fn from(value: Assignments) -> Self {
        Self { assignments: value }
    }
}

impl From<SolutionReference<'_>> for Solution {
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

    fn trailed_values(&self) -> &crate::engine::TrailedValues {
        unimplemented!("Currently, this information cannot be retrieved using this structure")
    }

    fn trailed_values_mut(&mut self) -> &mut crate::engine::TrailedValues {
        unimplemented!("Currently, this information cannot be retrieved using this structure")
    }
}

impl HasAssignments for Solution {
    fn assignments(&self) -> &Assignments {
        &self.assignments
    }

    fn trailed_values(&self) -> &crate::engine::TrailedValues {
        unimplemented!("Currently, this information cannot be retrieved using this structure")
    }

    fn trailed_values_mut(&mut self) -> &mut crate::engine::TrailedValues {
        unimplemented!("Currently, this information cannot be retrieved using this structure")
    }
}
