use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ConstructedPropagator;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::MaximumDescription;
use crate::arithmetic::MaximumPropagator;
use crate::arithmetic::MaximumRule;

/// The [`PropagatorConstructor`] for the [`MaximumPropagator`].
#[derive(Clone, Debug)]
pub struct MaximumArgs<ElementVar, Rhs> {
    pub constraint_description: MaximumDescription<ElementVar, Rhs>,
    pub constraint_tag: ConstraintTag,
}

impl<ElementVar, Rhs> PropagatorConstructor for MaximumArgs<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    type PropagatorImpl = MaximumPropagator<ElementVar, Rhs>;
    type Rule = MaximumRule<ElementVar, Rhs>;

    fn constraint_description(&self) -> MaximumDescription<ElementVar, Rhs> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        _: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> ConstructedPropagator<Self::PropagatorImpl> {
        let MaximumDescription { array, rhs } = self.constraint_description;

        let mut events_to_register = EventsToRegister::builder();
        for (idx, var) in array.iter().enumerate() {
            events_to_register =
                events_to_register.add(var, DomainEvents::BOUNDS, LocalId::from(idx as u32));
        }

        let rhs_local_id = LocalId::from(array.len() as u32);
        events_to_register = events_to_register.add(&rhs, DomainEvents::BOUNDS, rhs_local_id);

        let propagator = MaximumPropagator {
            array,
            rhs,
            inference_code,
        };

        ConstructedPropagator {
            events_to_register: events_to_register.build(),
            propagator,
        }
    }
}
