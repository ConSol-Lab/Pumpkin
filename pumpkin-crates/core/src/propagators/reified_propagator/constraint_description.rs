use pumpkin_checking::CheckerVariable;
use pumpkin_checking::DomainView;

use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
use crate::propagation::LocalId;
use crate::propagation::SolutionCheck;
use crate::variables::Literal;

/// The description of a constraint that only has to hold when the reification literal is true.
#[derive(Clone, Debug)]
pub struct HalfReifiedDescription<Description> {
    /// The description of the constraint that is reified.
    pub inner: Description,
    pub reification_literal: Literal,
}

impl<Description: ConstraintDescription> ConstraintDescription
    for HalfReifiedDescription<Description>
{
    fn scope(&self) -> Scope {
        // Whether the inner constraint has to hold depends on the reification literal, so the
        // literal is part of the scope, under a local id after those of the inner constraint.
        let mut scope = self.inner.scope();
        let literal_id = scope
            .domains()
            .map(|(local_id, _)| local_id.successor())
            .max()
            .unwrap_or(LocalId::from(0));
        self.reification_literal
            .add_to_scope(&mut scope, literal_id);
        scope
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        match self.reification_literal.induced_fixed_value(domains) {
            None => SolutionCheck::UnfixedVariable,
            Some(1) => self.inner.check_solution(domains),
            Some(_) => SolutionCheck::ConstraintSatisfied,
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
