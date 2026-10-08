use pumpkin_core::declare_inference_label;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::AbsoluteValueChecker;
use super::AbsoluteValueDescription;
use super::AbsoluteValuePropagator;

declare_inference_label!(AbsoluteValue);

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

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let AbsoluteValueArgs {
            constraint_description,
            constraint_tag,
        } = self;
        let AbsoluteValueDescription { signed, absolute } = constraint_description;

        let registration = EventsToRegister::builder()
            .add(&signed, DomainEvents::BOUNDS, LocalId::from(0))
            .add(&absolute, DomainEvents::BOUNDS, LocalId::from(1))
            .build();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_conflict_checker(
            constraint_tag,
            AbsoluteValue,
            AbsoluteValueChecker {
                signed: signed.clone(),
                absolute: absolute.clone(),
            },
        );

        let propagator = AbsoluteValuePropagator {
            signed,
            absolute,
            inference_code,
        };

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator,
        }
    }
}
