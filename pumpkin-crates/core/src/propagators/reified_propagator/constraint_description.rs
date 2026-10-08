use pumpkin_checking::CheckerVariable;
use pumpkin_checking::DomainView;

use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;
use crate::variables::Literal;

crate::scoped_struct! {
/// The description of a constraint that only has to hold when the reification literal is true.
#[derive(Clone, Debug)]
pub struct HalfReifiedDescription<Description> {
    /// The description of the constraint that is reified.
    pub inner: Description,
    pub reification_literal: Literal,
}
}

impl<Description: ConstraintDescription> ConstraintDescription
    for HalfReifiedDescription<Description>
{
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        match self.reification_literal.induced_fixed_value(domains) {
            Some(1) => self.inner.check_solution(domains),
            Some(_) => SolutionCheck::ConstraintSatisfied,
            // The half reification holds whatever the literal is when the inner constraint holds.
            None => match self.inner.check_solution(domains) {
                SolutionCheck::ConstraintSatisfied => SolutionCheck::ConstraintSatisfied,
                SolutionCheck::ConstraintViolated | SolutionCheck::Unknown => {
                    SolutionCheck::Unknown
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate;
    use crate::propagators::nogoods::NogoodDescription;
    use crate::state::State;

    #[test]
    fn a_nogood_with_a_false_predicate_holds_whatever_the_unfixed_variables() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");

        let nogood = |predicates: &[Predicate]| NogoodDescription {
            nogood: Box::from(predicates),
        };

        assert_eq!(
            nogood(&[predicate![x == 4], predicate![y == 1]]).check_solution(&state.assignments),
            SolutionCheck::ConstraintSatisfied
        );
        assert_eq!(
            nogood(&[predicate![x == 3], predicate![y == 1]]).check_solution(&state.assignments),
            SolutionCheck::Unknown
        );
        assert_eq!(
            nogood(&[predicate![x == 3]]).check_solution(&state.assignments),
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
            description.check_solution(&state.assignments),
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
            description.check_solution(&state.assignments),
            SolutionCheck::ConstraintViolated
        );
    }
}
