use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::variables::IntegerVariable;

use super::IntegerMultiplicationDescription;
use super::IntegerMultiplicationRule;
use super::propagator::IntegerMultiplicationPropagator;

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

/// The [`PropagatorConstructor`] for [`IntegerMultiplicationPropagator`].
///
/// Creates the propagator for `a * b = c`.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationConstructor<VA, VB, VC> {
    pub constraint_description: IntegerMultiplicationDescription<VA, VB, VC>,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB, VC> PropagatorConstructor for IntegerMultiplicationConstructor<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type PropagatorImpl = IntegerMultiplicationPropagator<VA, VB, VC>;
    type Rule = IntegerMultiplicationRule<VA, VB, VC>;

    fn constraint_description(&self) -> IntegerMultiplicationDescription<VA, VB, VC> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        _: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let IntegerMultiplicationConstructor {
            constraint_description,
            constraint_tag: _,
        } = self;
        let IntegerMultiplicationDescription { a, b, c } = constraint_description;

        let registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ANY_INT, ID_A)
            .add(&b, DomainEvents::ANY_INT, ID_B)
            .add(&c, DomainEvents::ANY_INT, ID_C)
            .build();

        let propagator = IntegerMultiplicationPropagator::new(a, b, c, inference_code);

        PropagatorSpec {
            registration,
            propagator,
        }
    }
}
