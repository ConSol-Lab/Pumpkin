use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

/// A trait which specifies the common behaviours of [`Solution`] and [`SolutionReference`].
pub trait ProblemSolution: HasAssignments {
    /// Returns the number of defined [`PropositionalVariable`]s
    fn num_propositional_variables(&self) -> usize {
        self.assignments_propositional()
            .num_propositional_variables() as usize
    }

    /// Returns the number of domains.
    fn num_domains(&self) -> usize {
        self.assignments_integer().num_domains() as usize
    }

    /// Returns the assigned boolean value of the provided [`PropositionalVariable`].
    fn get_propositional_variable_value(
        &self,
        propositional_variable: PropositionalVariable,
    ) -> bool {
        pumpkin_assert_moderate!(
            self.assignments_propositional()
                .is_variable_assigned(propositional_variable),
            "Expected retrieved propositional variable from solution to be assigned"
        );
        self.assignments_propositional()
            .is_variable_assigned_true(propositional_variable)
    }

    /// Returns the assigned boolean value of the provided [`Literal`].
    fn get_literal_value(&self, literal: Literal) -> bool {
        pumpkin_assert_moderate!(
            self.assignments_propositional()
                .is_literal_assigned(literal),
            "Expected retrieved literal from solution to be assigned"
        );
        self.assignments_propositional()
            .is_literal_assigned_true(literal)
    }

    /// Returns the assigned integer value of the provided variable.
    fn get_integer_value(&self, variable: impl IntegerVariable) -> i32 {
        let lower_bound = variable.lower_bound(self.assignments_integer());
        let upper_bound = variable.upper_bound(self.assignments_integer());

        pumpkin_assert_moderate!(
            lower_bound == upper_bound,
            "Expected retrieved integer variable from solution to be assigned"
        );

        lower_bound
    }
}

/// A solution which keeps reference to its inner structures.
#[derive(Debug, Copy, Clone)]
pub struct SolutionReference<'a> {
    assignments_propositional: &'a AssignmentsPropositional,
    assignments_integer: &'a AssignmentsInteger,
}

impl<'a> SolutionReference<'a> {
    pub fn new(
        assignments_propositional: &'a AssignmentsPropositional,
        assignments_integer: &'a AssignmentsInteger,
    ) -> SolutionReference<'a> {
        SolutionReference {
            assignments_propositional,
            assignments_integer,
        }
    }

    pub fn get_propostional_variables(&self) -> impl Iterator<Item = PropositionalVariable> {
        self.assignments_propositional.get_propositional_variables()
    }
}

impl ProblemSolution for SolutionReference<'_> {}

/// A solution which takes ownership of its inner structures.
#[derive(Clone, Debug, Default)]
pub struct Solution {
    assignments_propositional: AssignmentsPropositional,
    assignments_integer: AssignmentsInteger,
}

impl Solution {
    pub fn new(
        assignments_propositional: AssignmentsPropositional,
        assignments_integer: AssignmentsInteger,
    ) -> Self {
        Self {
            assignments_propositional,
            assignments_integer,
        }
    }

    pub fn as_reference(&self) -> SolutionReference<'_> {
        SolutionReference {
            assignments_propositional: &self.assignments_propositional,
            assignments_integer: &self.assignments_integer,
        }
    }
}

impl ProblemSolution for Solution {}

impl From<SolutionReference<'_>> for Solution {
    fn from(value: SolutionReference) -> Self {
        Self {
            assignments_propositional: value.assignments_propositional.clone(),
            assignments_integer: value.assignments_integer.clone(),
        }
    }
}

impl<'a> HasAssignments for SolutionReference<'a> {
    fn assignments_integer(&self) -> &'a AssignmentsInteger {
        self.assignments_integer
    }

    fn assignments_propositional(&self) -> &'a AssignmentsPropositional {
        self.assignments_propositional
    }
}

impl HasAssignments for Solution {
    fn assignments_integer(&self) -> &AssignmentsInteger {
        &self.assignments_integer
    }

    fn assignments_propositional(&self) -> &AssignmentsPropositional {
        &self.assignments_propositional
    }
}
