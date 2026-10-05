use std::borrow::Cow;
use std::marker::PhantomData;
use std::rc::Rc;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::LinearNotEqualChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
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
}

/// The rule of the linear disequality.
#[derive(Clone, Copy, Debug)]
pub struct LinearNotEqualRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> LinearNotEqualRule<Var> {
    fn checker(description: &LinearNotEqualDescription<Var>) -> LinearNotEqualChecker<Var> {
        LinearNotEqualChecker {
            terms: description.terms.as_ref().into(),
            bound: description.rhs,
        }
    }
}

impl<Var: IntegerVariable + 'static> ConflictRule for LinearNotEqualRule<Var> {
    type Description = LinearNotEqualDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed(LinearNotEqualChecker::<Var>::RULE_NAME)
    }

    fn create_inference_checker(
        description: &LinearNotEqualDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &LinearNotEqualDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
