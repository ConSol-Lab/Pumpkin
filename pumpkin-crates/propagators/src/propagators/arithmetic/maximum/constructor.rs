use pumpkin_core::checkers::Scope;
use pumpkin_core::checkers::StrongConsistency;
use pumpkin_core::checkers::StrongRetentionChecker;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::MaximumChecker;
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

        let mut scope = Scope::default();
        let mut registration = EventsToRegister::builder();
        for (idx, var) in array.iter().enumerate() {
            let local_id = LocalId::from(idx as u32);
            registration = registration.add(var, DomainEvents::BOUNDS, local_id);
            var.add_to_scope(&mut scope, local_id);
        }

        let rhs_local_id = LocalId::from(array.len() as u32);
        registration = registration.add(&rhs, DomainEvents::BOUNDS, rhs_local_id);
        rhs.add_to_scope(&mut scope, rhs_local_id);

        let checker = MaximumChecker {
            array: array.clone(),
            rhs: rhs.clone(),
        };

        let mut checkers = RuntimeCheckers::builder();
        let inference_code =
            checkers.add_inference_checker(constraint_tag, Maximum, checker.clone());
        checkers.add_consistency_checker(
            scope,
            StrongRetentionChecker::new(StrongConsistency::Bounds, checker),
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
