use pumpkin_checking::DomainView;
use pumpkin_checking::IntExt;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the linear inequality `∑ terms_i <= bound`.
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualDescription<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}
}

impl<Var: IntegerVariable> ConstraintDescription for LinearLessOrEqualDescription<Var> {
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        let bound = i64::from(self.bound);

        let (mut highest_sum, mut lowest_sum) = (IntExt::Int(0_i64), IntExt::Int(0_i64));
        for term in self.terms.iter() {
            highest_sum = highest_sum + IntExt::<i64>::from(term.induced_upper_bound(domains));
            lowest_sum = lowest_sum + IntExt::<i64>::from(term.induced_lower_bound(domains));
        }

        if highest_sum <= bound {
            SolutionCheck::ConstraintSatisfied
        } else if lowest_sum > bound {
            SolutionCheck::ConstraintViolated
        } else {
            SolutionCheck::Unknown
        }
    }
}
