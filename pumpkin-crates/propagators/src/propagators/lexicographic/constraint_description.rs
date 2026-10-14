use pumpkin_checking::DomainView;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the constraint that `x` is lexicographically at most `y`.
#[derive(Clone, Debug)]
pub struct LexLessOrEqualDescription<Var> {
    pub x: Box<[Var]>,
    pub y: Box<[Var]>,
}
}

impl<Var: IntegerVariable> ConstraintDescription for LexLessOrEqualDescription<Var> {
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        for (x, y) in self.x.iter().zip(self.y.iter()) {
            let (Some(x), Some(y)) = (
                x.induced_fixed_value(domains),
                y.induced_fixed_value(domains),
            ) else {
                return SolutionCheck::Unknown;
            };
            if x < y {
                return SolutionCheck::ConstraintSatisfied;
            }
            if x > y {
                return SolutionCheck::ConstraintViolated;
            }
        }
        SolutionCheck::ConstraintSatisfied
    }
}
