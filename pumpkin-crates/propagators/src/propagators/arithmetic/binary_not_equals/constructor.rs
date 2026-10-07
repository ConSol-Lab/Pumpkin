use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorWithEvents;
use pumpkin_core::variables::IntegerVariable;

use super::BinaryNotEqualsDescription;
use super::BinaryNotEqualsPropagator;
use super::BinaryNotEqualsRule;

/// The [`PropagatorConstructor`] for the [`BinaryNotEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsPropagatorArgs<AVar, BVar> {
    pub constraint_description: BinaryNotEqualsDescription<AVar, BVar>,
    pub constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryNotEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryNotEqualsPropagator<AVar, BVar>;
    type Rule = BinaryNotEqualsRule<AVar, BVar>;

    fn constraint_description(&self) -> BinaryNotEqualsDescription<AVar, BVar> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        _: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorWithEvents<Self::PropagatorImpl> {
        let BinaryNotEqualsDescription { a, b } = self.constraint_description;

        // We only care about the case where one of the two is assigned
        let events_to_register = EventsToRegister::builder()
            .add(&a, DomainEvents::ASSIGN, LocalId::from(0))
            .add(&b, DomainEvents::ASSIGN, LocalId::from(1))
            .build();

        let propagator = BinaryNotEqualsPropagator {
            a,
            b,

            inference_code,
        };

        PropagatorWithEvents {
            events_to_register,
            propagator,
        }
    }
}
