use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ElementChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::MissingRetentionChecker;
use pumpkin_core::variables::IntegerVariable;

use super::ID_INDEX;
use super::ID_RHS;
use super::ID_X_OFFSET;

/// The description of the constraint `array[index] = rhs`.
#[derive(Clone, Debug)]
pub struct ElementDescription<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
}

impl<VX, VI, VE> ConstraintDescription for ElementDescription<VX, VI, VE>
where
    VX: IntegerVariable,
    VI: IntegerVariable,
    VE: IntegerVariable,
{
    fn scope(&self) -> Scope {
        let mut scope = Scope::default();
        for (i, x_i) in self.array.iter().enumerate() {
            x_i.add_to_scope(&mut scope, LocalId::from(i as u32 + ID_X_OFFSET));
        }
        self.index.add_to_scope(&mut scope, ID_INDEX);
        self.rhs.add_to_scope(&mut scope, ID_RHS);
        scope
    }
}

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
        Cow::Borrowed(ElementChecker::<VX, VI, VE>::RULE_NAME)
    }

    fn create_inference_checker(
        description: &ElementDescription<VX, VI, VE>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        ElementChecker::new(
            description.array.clone(),
            description.index.clone(),
            description.rhs.clone(),
        )
    }

    fn create_retention_checker(
        _: &ElementDescription<VX, VI, VE>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        MissingRetentionChecker::todo("the element rule")
    }
}
