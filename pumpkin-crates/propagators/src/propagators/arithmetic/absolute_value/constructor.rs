use pumpkin_checking::checkers::AbsoluteValueChecker;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::AbsoluteValue;
use super::AbsoluteValuePropagator;

#[derive(Clone, Debug)]
pub struct AbsoluteValueArgs<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
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
            signed,
            absolute,
            constraint_tag,
        } = self;

        let registration = EventsToRegister::builder()
            .add(&signed, DomainEvents::BOUNDS, LocalId::from(0))
            .add(&absolute, DomainEvents::BOUNDS, LocalId::from(1))
            .build();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_rule(
            ((LocalId::from(0), &signed), (LocalId::from(1), &absolute)),
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
