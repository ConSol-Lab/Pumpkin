use std::rc::Rc;

use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the linear disequality `∑ terms_i != rhs`.
#[derive(Clone, Debug)]
pub struct LinearNotEqualDescription<Var> {
    /// The terms of the sum
    pub terms: Rc<[Var]>,
    /// The right-hand side of the sum
    pub rhs: i32,
}

impl<Var: IntegerVariable> ConstraintDescription for LinearNotEqualDescription<Var> {
    fn scope(&self) -> Scope {
        Scope::from_variables(self.terms.iter())
    }

    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let Some(sum) = self
            .terms
            .iter()
            .map(|term| term.induced_fixed_value(domains).map(i64::from))
            .sum::<Option<i64>>()
        else {
            return SolutionCheck::UnfixedVariable;
        };

        if sum == i64::from(self.rhs) {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::ConstraintSatisfied
        }
    }
}
