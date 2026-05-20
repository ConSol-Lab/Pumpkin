use pumpkin_core::checkers::StrongConsistency;
use pumpkin_core::checkers::StrongConsistencyChecker;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::InferenceCheckers;
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

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, IntegerMultiplication),
            Box::new(IntegerMultiplicationChecker {
                a: self.a.clone(),
                b: self.b.clone(),
                c: self.c.clone(),
            }),
        );

        checkers.add_consistency_checker(
            self.constraint_tag,
            (
                (super::ID_A, &self.a),
                (super::ID_B, &self.b),
                (super::ID_C, &self.c),
            ),
            StrongConsistencyChecker::new(
                StrongConsistency::Bounds,
                IntegerMultiplicationChecker {
                    a: self.a.clone(),
                    b: self.b.clone(),
                    c: self.c.clone(),
                },
            ),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        } = self;

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
