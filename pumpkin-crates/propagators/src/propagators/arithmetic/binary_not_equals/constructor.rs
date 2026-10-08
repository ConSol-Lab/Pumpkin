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

use super::BinaryNotEqualsChecker;
use super::BinaryNotEqualsDescription;
use super::BinaryNotEqualsPropagator;

declare_inference_label!(BinaryNotEquals);

/// The [`PropagatorConstructor`] for the [`BinaryNotEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsPropagatorArgs<AVar, BVar> {
    pub constraint_description: BinaryNotEqualsDescription<AVar, BVar>,
    pub constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryNotEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryNotEqualsPropagator<AVar, BVar>;

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let BinaryNotEqualsPropagatorArgs {
            constraint_description,
            constraint_tag,
        } = self;
        let BinaryNotEqualsDescription { a, b } = constraint_description;

        // We only care about the case where one of the two is assigned
        let registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ASSIGN, LocalId::from(0))
            .add(&b, DomainEvents::ASSIGN, LocalId::from(1))
            .build();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_conflict_checker(
            constraint_tag,
            BinaryNotEquals,
            BinaryNotEqualsChecker {
                lhs: a.clone(),
                rhs: b.clone(),
            },
        );

        let propagator = BinaryNotEqualsPropagator {
            a,
            b,

            inference_code,
        };

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator,
        }
    }
}
