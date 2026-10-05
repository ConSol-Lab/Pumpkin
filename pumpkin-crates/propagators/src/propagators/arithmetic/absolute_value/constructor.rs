use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::variables::IntegerVariable;

use super::AbsoluteValueDescription;
use super::AbsoluteValuePropagator;
use super::AbsoluteValueRule;

#[derive(Clone, Debug)]
pub struct AbsoluteValueArgs<VA, VB> {
    pub constraint_description: AbsoluteValueDescription<VA, VB>,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB> PropagatorConstructor for AbsoluteValueArgs<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    type PropagatorImpl = AbsoluteValuePropagator<VA, VB>;
    type Rule = AbsoluteValueRule<VA, VB>;

    fn constraint_description(&self) -> AbsoluteValueDescription<VA, VB> {
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
        let AbsoluteValueDescription { signed, absolute } = self.constraint_description;

        let registration = EventsToRegister::builder()
            .add(&signed, DomainEvents::BOUNDS, LocalId::from(0))
            .add(&absolute, DomainEvents::BOUNDS, LocalId::from(1))
            .build();

        let propagator = AbsoluteValuePropagator {
            signed,
            absolute,
            inference_code,
        };

        PropagatorSpec {
            registration,
            propagator,
        }
    }
}
