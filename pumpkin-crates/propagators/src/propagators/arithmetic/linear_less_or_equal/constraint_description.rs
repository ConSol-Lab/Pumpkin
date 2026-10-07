use pumpkin_checking::DomainView;
use pumpkin_checking::IntExt;
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
        let bound = i64::from(self.bound);
        let highest_sum = self
            .terms
            .iter()
            .map(|term| IntExt::<i64>::from(term.induced_upper_bound(domains)))
            .sum::<IntExt<i64>>();
        let lowest_sum = self
            .terms
            .iter()
            .map(|term| IntExt::<i64>::from(term.induced_lower_bound(domains)))
            .sum::<IntExt<i64>>();

        if highest_sum <= bound {
            SolutionCheck::ConstraintSatisfied
        } else if lowest_sum > bound {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::Unknown
        }
    }
}
