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

impl<ElementVar, Rhs> PropagatorConstructor for MaximumConstructor<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    type PropagatorImpl = MaximumPropagator<ElementVar, Rhs>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let MaximumConstructor {
            array,
            rhs,
            constraint_tag,
        } = self;

        context.add_inference_checker(
            InferenceCode::new(constraint_tag, Maximum),
            Box::new(MaximumChecker {
                array: array.clone(),
                rhs: rhs.clone(),
            }),
        );

        for (idx, var) in array.iter().enumerate() {
            context.register(var.clone(), DomainEvents::BOUNDS, LocalId::from(idx as u32));
        }

        context.register(
            rhs.clone(),
            DomainEvents::BOUNDS,
            LocalId::from(array.len() as u32),
        );

        let inference_code = InferenceCode::new(constraint_tag, Maximum);

        MaximumPropagator {
            array,
            rhs,
            inference_code,
        }
    }
}
