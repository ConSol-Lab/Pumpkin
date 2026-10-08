use pumpkin_checking::checkers::MaximumChecker;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::Maximum;
use super::MaximumDescription;
use super::MaximumPropagator;

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

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let MaximumArgs {
            constraint_description,
            constraint_tag,
        } = self;
        let MaximumDescription { array, rhs } = constraint_description;

        let mut registration = EventsToRegister::builder();
        for (idx, var) in array.iter().enumerate() {
            registration = registration.add(var, DomainEvents::BOUNDS, LocalId::from(idx as u32));
        }

        registration = registration.add(
            &rhs,
            DomainEvents::BOUNDS,
            LocalId::from(array.len() as u32),
        );

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_conflict_checker(
            constraint_tag,
            Maximum,
            MaximumChecker {
                array: array.clone(),
                rhs: rhs.clone(),
            },
        );

        let propagator = MaximumPropagator {
            array,
            rhs,
            inference_code,
        };

        PropagatorSpec {
            registration: registration.build(),
            checkers: checkers.build(),
            propagator,
        }
    }
}
