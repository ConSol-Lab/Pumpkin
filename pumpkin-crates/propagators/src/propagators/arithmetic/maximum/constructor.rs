use pumpkin_checking::checkers::MaximumChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::MaximumPropagator;
use crate::arithmetic::maximum::Maximum;

/// The [`PropagatorConstructor`] for the [`MaximumPropagator`].
#[derive(Clone, Debug)]
pub struct MaximumArgs<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
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
            array,
            rhs,
            constraint_tag,
        } = self;

        let mut registration = EventsToRegister::builder();
        for (idx, var) in array.iter().enumerate() {
            registration = registration.add(var, DomainEvents::BOUNDS, LocalId::from(idx as u32));
        }

        let rhs_local_id = LocalId::from(array.len() as u32);
        registration = registration.add(&rhs, DomainEvents::BOUNDS, rhs_local_id);

        let mut scope = Scope::from_variables(array.iter());
        rhs.add_to_scope(&mut scope, rhs_local_id);

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_rule(
            scope,
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
