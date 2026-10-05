use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::MaximumChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `rhs = max(array)`.
#[derive(Clone, Debug)]
pub struct MaximumDescription<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> ConstraintDescription
    for MaximumDescription<ElementVar, Rhs>
{
    fn scope(&self) -> Scope {
        let mut scope = Scope::from_variables(self.array.iter());
        self.rhs
            .add_to_scope(&mut scope, LocalId::from(self.array.len() as u32));
        scope
    }
}

/// The rule of the maximum constraint.
#[derive(Clone, Copy, Debug)]
pub struct MaximumRule<ElementVar, Rhs>(PhantomData<(ElementVar, Rhs)>);

impl<ElementVar, Rhs> MaximumRule<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    fn checker(
        description: &MaximumDescription<ElementVar, Rhs>,
    ) -> MaximumChecker<ElementVar, Rhs> {
        MaximumChecker {
            array: description.array.clone(),
            rhs: description.rhs.clone(),
        }
    }
}

impl<ElementVar, Rhs> ConflictRule for MaximumRule<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    type Description = MaximumDescription<ElementVar, Rhs>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("maximum")
    }

    fn create_inference_checker(
        description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
