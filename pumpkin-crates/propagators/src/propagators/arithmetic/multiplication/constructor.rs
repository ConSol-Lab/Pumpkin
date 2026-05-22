use pumpkin_core::checkers::StrongConsistency;
use pumpkin_core::checkers::StrongConsistencyChecker;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::IntegerMultiplicationPropagator;
use crate::arithmetic::multiplication::IntegerMultiplication;
use crate::arithmetic::multiplication::checker::IntegerMultiplicationChecker;

/// The [`PropagatorConstructor`] for [`IntegerMultiplicationPropagator`].
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationArgs<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB, VC> PropagatorConstructor for IntegerMultiplicationArgs<VA, VB, VC>
where
    VA: IntegerVariable + SupportsValue<f32> + 'static,
    VB: IntegerVariable + SupportsValue<f32> + 'static,
    VC: IntegerVariable + SupportsValue<f32> + 'static,
{
    type PropagatorImpl = IntegerMultiplicationPropagator<VA, VB, VC>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        } = self;

        context.add_inference_checker(
            InferenceCode::new(constraint_tag, IntegerMultiplication),
            Box::new(IntegerMultiplicationChecker {
                a: a.clone(),
                b: b.clone(),
                c: c.clone(),
            }),
        );

        context.add_consistency_checker(
            ((super::ID_A, &a), (super::ID_B, &b), (super::ID_C, &c)),
            StrongConsistencyChecker::new(
                StrongConsistency::Bounds,
                IntegerMultiplicationChecker {
                    a: a.clone(),
                    b: b.clone(),
                    c: c.clone(),
                },
            ),
        );

        context.register(a.clone(), DomainEvents::ANY_INT, super::ID_A);
        context.register(b.clone(), DomainEvents::ANY_INT, super::ID_B);
        context.register(c.clone(), DomainEvents::ANY_INT, super::ID_C);

        IntegerMultiplicationPropagator {
            a,
            b,
            c,
            inference_code: InferenceCode::new(constraint_tag, IntegerMultiplication),
        }
    }
}
