use crate::checkers::CompleteSolution;
use crate::checkers::ScopeItem;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;
use crate::variables::Literal;

/// The description of a constraint that only has to hold when the reification literal is true.
#[derive(Clone, Debug, ScopeItem)]
pub struct HalfReifiedDescription<Description> {
    /// The description of the constraint that is reified.
    pub inner: Description,
    pub reification_literal: Literal,
}

impl<Description: ConstraintDescription> ConstraintDescription
    for HalfReifiedDescription<Description>
{
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        if solution.literal_value(self.reification_literal) {
            self.inner.check_solution(solution)
        } else {
            SolutionCheck::ConstraintSatisfied
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::propagators::nogoods::NogoodDescription;
    use crate::state::State;

    #[test]
    fn a_nogood_is_violated_only_when_all_its_predicates_hold() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");
        let _ = state
            .post(predicate![y == 1])
            .expect("the value is in the domain");
        let solution = state.complete_solution().expect("every variable is fixed");

        let nogood = |predicates: &[Predicate]| NogoodDescription {
            nogood: Box::from(predicates),
        };

        assert_eq!(
            nogood(&[predicate![x == 4], predicate![y == 1]]).check_solution(&solution),
            SolutionCheck::ConstraintSatisfied
        );
        assert_eq!(
            nogood(&[predicate![x == 3], predicate![y == 1]]).check_solution(&solution),
            SolutionCheck::ConstraintViolated
        );
    }

    #[test]
    fn the_inner_constraint_does_not_have_to_hold_when_the_literal_is_false() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let literal = state.new_literal(None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");
        let _ = state
            .post(literal.get_false_predicate())
            .expect("the literal is unassigned");

        let description = HalfReifiedDescription {
            inner: NogoodDescription {
                nogood: Box::from([predicate![x == 3]]),
            },
            reification_literal: literal,
        };

        assert_eq!(
            description
                .check_solution(&state.complete_solution().expect("every variable is fixed")),
            SolutionCheck::ConstraintSatisfied
        );
    }

    #[test]
    fn the_inner_constraint_has_to_hold_when_the_literal_is_true() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let literal = state.new_literal(None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");
        let _ = state
            .post(literal.get_true_predicate())
            .expect("the literal is unassigned");

        let description = HalfReifiedDescription {
            inner: NogoodDescription {
                nogood: Box::from([predicate![x == 3]]),
            },
            reification_literal: literal,
        };

        assert_eq!(
            description
                .check_solution(&state.complete_solution().expect("every variable is fixed")),
            SolutionCheck::ConstraintViolated
        );
    }
}
