use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ElementChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::ElementDescription;

/// The rule of the element constraint.
#[derive(Clone, Copy, Debug)]
pub struct ElementRule<VX, VI, VE>(PhantomData<(VX, VI, VE)>);

impl<VX, VI, VE> ConflictRule for ElementRule<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    type Description = ElementDescription<VX, VI, VE>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("element")
    }

    fn create_inference_checker(
        constraint_description: &ElementDescription<VX, VI, VE>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &ElementDescription<VX, VI, VE>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}

impl<VX, VI, VE> ElementRule<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    fn checker(
        constraint_description: &ElementDescription<VX, VI, VE>,
    ) -> ElementChecker<VX, VI, VE> {
        ElementChecker::new(
            constraint_description.array.clone(),
            constraint_description.index.clone(),
            constraint_description.rhs.clone(),
        )
    }
}
