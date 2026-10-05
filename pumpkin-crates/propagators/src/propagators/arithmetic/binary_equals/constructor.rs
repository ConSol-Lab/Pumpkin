use pumpkin_core::containers::HashSet;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::BinaryEqualsDescription;
use crate::arithmetic::BinaryEqualsPropagator;
use crate::arithmetic::BinaryEqualsRule;

/// The [`PropagatorConstructor`] for the [`BinaryEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryEqualsPropagatorArgs<AVar, BVar> {
    pub constraint_description: BinaryEqualsDescription<AVar, BVar>,
    pub constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryEqualsPropagator<AVar, BVar>;
    type Rule = BinaryEqualsRule<AVar, BVar>;

    fn constraint_description(&self) -> BinaryEqualsDescription<AVar, BVar> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        _: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let BinaryEqualsDescription { a, b } = self.constraint_description;

        let registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ANY_INT, super::ID_LHS)
            .add(&b, DomainEvents::ANY_INT, super::ID_RHS)
            .build();

        let propagator = BinaryEqualsPropagator {
            a,
            b,

            a_removed_values: HashSet::default(),
            b_removed_values: HashSet::default(),

            inference_code,

            has_backtracked: false,
            first_propagation_loop: true,
            reason: Predicate::trivially_false(),
        };

        PropagatorSpec {
            registration,
            propagator,
        }
    }
}
