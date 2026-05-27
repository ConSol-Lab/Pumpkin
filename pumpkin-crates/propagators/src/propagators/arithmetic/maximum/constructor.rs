use pumpkin_core::checkers::Scope;
use pumpkin_core::checkers::StrongConsistency;
use pumpkin_core::checkers::StrongRetentionChecker;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::MaximumChecker;
use crate::arithmetic::MaximumPropagator;
use crate::arithmetic::maximum::Maximum;

#[derive(Clone, Debug)]
pub struct MaximumConstructor<ElementVar, Rhs> {
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

    fn create(
        self,
        context: PropagatorConstructorContext,
    ) -> (EventRegistration, Self::PropagatorImpl) {
        let MaximumArgs {
            array,
            rhs,
            constraint_tag,
        } = self;

        let mut registration = EventRegistration::builder();
        for (idx, var) in array.iter().enumerate() {
            registration = registration.add(var, DomainEvents::BOUNDS, LocalId::from(idx as u32));
        }

        registration = registration.add(
            &rhs,
            DomainEvents::BOUNDS,
            LocalId::from(array.len() as u32),
        );

        let inference_code = InferenceCode::new(constraint_tag, Maximum);

        let mut scope = Scope::default();

        for (idx, var) in array.iter().enumerate() {
            let local_id = LocalId::from(idx as u32);
            context.register(var.clone(), DomainEvents::BOUNDS, local_id);
            var.add_to_scope(&mut scope, local_id);
        }

        let rhs_local_id = LocalId::from(array.len() as u32);
        context.register(rhs.clone(), DomainEvents::BOUNDS, rhs_local_id);
        rhs.add_to_scope(&mut scope, rhs_local_id);

        context.add_inference_checker(
            InferenceCode::new(constraint_tag, Maximum),
            Box::new(MaximumChecker {
                array: array.clone(),
                rhs: rhs.clone(),
            }),
        );

        context.add_consistency_checker(
            scope,
            StrongRetentionChecker::new(
                StrongConsistency::Bounds,
                MaximumChecker {
                    array: array.clone(),
                    rhs: rhs.clone(),
                },
            ),
        );

        let propagator = MaximumPropagator {
            array,
            rhs,
            inference_code,
        };

        (registration.build(), propagator)
    }
}
