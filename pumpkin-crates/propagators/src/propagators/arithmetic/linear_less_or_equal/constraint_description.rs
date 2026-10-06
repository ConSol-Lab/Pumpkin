use pumpkin_checking::DomainView;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

/// The description of the linear inequality `∑ terms_i <= bound`.
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualDescription<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}

impl<Var: IntegerVariable> ConstraintDescription for LinearLessOrEqualDescription<Var> {
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

        if sum <= i64::from(self.bound) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}
