use pumpkin_core::constraints::ReifiablePropagatorConstructor;
use pumpkin_core::declare_inference_label;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::ImpliedBy;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotReified;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::Reification;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;

use super::checker::IntegerMultiplicationChecker;
use super::propagator::IntegerMultiplicationPropagator;

declare_inference_label!(IntegerMultiplication);

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

/// The [`PropagatorConstructor`] for [`IntegerMultiplicationPropagator`].
///
/// Creates the propagator for `a * b = c`.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationConstructor<VA, VB, VC, R = NotReified> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
    pub constraint_tag: ConstraintTag,
    pub reification: R,
}

impl<VA, VB, VC> ReifiablePropagatorConstructor for IntegerMultiplicationConstructor<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type ImpliedBy = IntegerMultiplicationConstructor<VA, VB, VC, ImpliedBy>;

    fn into_implied_by(self, reification_literal: Literal) -> Self::ImpliedBy {
        IntegerMultiplicationConstructor {
            a: self.a,
            b: self.b,
            c: self.c,
            constraint_tag: self.constraint_tag,
            reification: ImpliedBy(reification_literal),
        }
    }
}

impl<VA, VB, VC, R> PropagatorConstructor for IntegerMultiplicationConstructor<VA, VB, VC, R>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
    R: Reification,
{
    type PropagatorImpl = IntegerMultiplicationPropagator<VA, VB, VC, R>;

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let IntegerMultiplicationConstructor {
            a,
            b,
            c,
            constraint_tag,
            reification,
        } = self;

        let mut registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ANY_INT, ID_A)
            .add(&b, DomainEvents::ANY_INT, ID_B)
            .add(&c, DomainEvents::ANY_INT, ID_C)
            .build();
        reification.register(&mut registration);

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            constraint_tag,
            IntegerMultiplication,
            IntegerMultiplicationChecker {
                a: a.clone(),
                b: b.clone(),
                c: c.clone(),
            },
        );

        let propagator = IntegerMultiplicationPropagator::new(a, b, c, inference_code, reification);

        PropagatorSpec {
            registration,
            checkers: checkers.build().reified(reification),
            propagator,
        }
    }
}
