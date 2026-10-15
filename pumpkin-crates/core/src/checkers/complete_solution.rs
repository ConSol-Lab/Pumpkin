use crate::engine::Assignments;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::ConstraintDescription;
use crate::state::State;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

/// An assignment in which every domain has exactly one value: the input of
/// [`ConstraintDescription::check_solution`].
///
/// It can only be created by a check that every domain is fixed, such as
/// [`State::complete_solution`], so the values it returns always exist.
#[derive(Clone, Copy, Debug)]
pub struct CompleteSolution<'assignments> {
    assignments: &'assignments Assignments,
}

/// A domain that does not have exactly one value, so the assignment is not a complete solution.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnfixedDomain(pub DomainId);

impl<'assignments> CompleteSolution<'assignments> {
    /// Fails with the first domain that does not have exactly one value.
    pub(crate) fn try_new(assignments: &'assignments Assignments) -> Result<Self, UnfixedDomain> {
        match assignments
            .get_domains()
            .find(|domain| !assignments.is_domain_assigned(domain))
        {
            Some(domain) => Err(UnfixedDomain(domain)),
            None => Ok(CompleteSolution { assignments }),
        }
    }

    pub fn integer_value<Var: IntegerVariable>(&self, variable: &Var) -> i32 {
        variable.lower_bound(self.assignments)
    }

    pub fn literal_value(&self, literal: Literal) -> bool {
        self.predicate_holds(literal.get_true_predicate())
    }

    pub fn predicate_holds(&self, predicate: Predicate) -> bool {
        self.assignments
            .evaluate_predicate(predicate)
            .expect("every domain of a complete solution is fixed")
    }
}

impl State {
    /// The current assignment as a [`CompleteSolution`], or the first domain that is not fixed.
    pub fn complete_solution(&self) -> Result<CompleteSolution<'_>, UnfixedDomain> {
        CompleteSolution::try_new(&self.assignments)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate;

    #[test]
    fn an_assignment_with_an_unfixed_domain_is_not_a_complete_solution() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");

        assert_eq!(state.complete_solution().err(), Some(UnfixedDomain(y)));
    }

    #[test]
    fn a_complete_solution_gives_the_values_of_the_variables() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let literal = state.new_literal(None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");
        let _ = state
            .post(literal.get_false_predicate())
            .expect("the literal is unassigned");

        let solution = state.complete_solution().expect("every variable is fixed");

        assert_eq!(solution.integer_value(&x), 3);
        assert!(!solution.literal_value(literal));
        assert!(solution.predicate_holds(predicate![x >= 2]));
        assert!(!solution.predicate_holds(predicate![x >= 4]));
    }
}
