use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::ElementChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::ElementDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct ElementRule<VX, VI, VE>(PhantomData<(VX, VI, VE)>);

impl<VX: IntegerVariable + 'static, VI: IntegerVariable + 'static, VE: IntegerVariable + 'static>
    ConflictRule for ElementRule<VX, VI, VE>
{
    type Description = ElementDescription<VX, VI, VE>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("element")
    }

    fn create_conflict_checker(
        constraint_description: &ElementDescription<VX, VI, VE>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        ElementChecker::new(
            constraint_description.array.clone(),
            constraint_description.index.clone(),
            constraint_description.rhs.clone(),
        )
    }
}
