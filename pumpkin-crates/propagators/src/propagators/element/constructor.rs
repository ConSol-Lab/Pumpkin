use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::variables::IntegerVariable;

use super::ElementDescription;
use super::ElementPropagator;
use super::ElementRule;
use super::ID_INDEX;
use super::ID_RHS;
use super::ID_X_OFFSET;

#[derive(Clone, Debug)]
pub struct ElementArgs<VX, VI, VE> {
    pub constraint_description: ElementDescription<VX, VI, VE>,
    pub constraint_tag: ConstraintTag,
}

impl<VX, VI, VE> PropagatorConstructor for ElementArgs<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    type PropagatorImpl = ElementPropagator<VX, VI, VE>;
    type Rule = ElementRule<VX, VI, VE>;

    fn constraint_description(&self) -> ElementDescription<VX, VI, VE> {
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
        let ElementDescription { array, index, rhs } = self.constraint_description;

        let mut registration = EventsToRegister::builder();
        for (i, x_i) in array.iter().enumerate() {
            registration = registration.add(
                x_i,
                DomainEvents::ANY_INT,
                LocalId::from(i as u32 + ID_X_OFFSET),
            );
        }

        registration = registration.add(&index, DomainEvents::ANY_INT, ID_INDEX);
        registration = registration.add(&rhs, DomainEvents::ANY_INT, ID_RHS);

        let propagator = ElementPropagator {
            array,
            index,
            rhs,
            inference_code,
            rhs_reason_buffer: vec![],
        };

        PropagatorSpec {
            registration: registration.build(),
            propagator,
        }
    }
}
