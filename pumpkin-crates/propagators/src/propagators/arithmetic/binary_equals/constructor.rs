use pumpkin_core::checkers::StrongConsistency;
use pumpkin_core::checkers::StrongConsistencyChecker;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::containers::HashSet;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::variables::IntegerVariable;

use crate::arithmetic::BinaryEqualsChecker;
use crate::arithmetic::BinaryEqualsPropagator;

/// The [`PropagatorConstructor`] for the [`BinaryEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryEqualsPropagatorArgs<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
    pub constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + SupportsValue + 'static,
    BVar: IntegerVariable + SupportsValue + 'static,
{
    type PropagatorImpl = BinaryEqualsPropagator<AVar, BVar>;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, super::BinaryEquals),
            Box::new(BinaryEqualsChecker {
                lhs: self.a.clone(),
                rhs: self.b.clone(),
            }),
        );

        checkers.add_consistency_checker(
            self.constraint_tag,
            ((super::ID_LHS, &self.a), (super::ID_RHS, &self.b)),
            StrongConsistencyChecker::new(
                StrongConsistency::Domain,
                BinaryEqualsChecker {
                    lhs: self.a.clone(),
                    rhs: self.b.clone(),
                },
            ),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        } = self;

        context.register(a.clone(), DomainEvents::ANY_INT, super::ID_LHS);
        context.register(b.clone(), DomainEvents::ANY_INT, super::ID_RHS);

        BinaryEqualsPropagator {
            a,
            b,

            a_removed_values: HashSet::default(),
            b_removed_values: HashSet::default(),

            inference_code: InferenceCode::new(constraint_tag, super::BinaryEquals),

            has_backtracked: false,
            first_propagation_loop: true,
            reason: Predicate::trivially_false(),
        }
    }
}
