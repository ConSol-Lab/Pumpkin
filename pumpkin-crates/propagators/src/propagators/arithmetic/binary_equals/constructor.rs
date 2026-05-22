use pumpkin_core::checkers::StrongConsistency;
use pumpkin_core::checkers::StrongConsistencyChecker;
use pumpkin_core::containers::HashSet;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
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
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryEqualsPropagator<AVar, BVar>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        } = self;

        context.add_inference_checker(
            InferenceCode::new(constraint_tag, super::BinaryEquals),
            Box::new(BinaryEqualsChecker {
                lhs: a.clone(),
                rhs: b.clone(),
            }),
        );

        context.add_consistency_checker(
            ((super::ID_LHS, &a), (super::ID_RHS, &b)),
            StrongConsistencyChecker::new(
                StrongConsistency::Domain,
                BinaryEqualsChecker {
                    lhs: a.clone(),
                    rhs: b.clone(),
                },
            ),
        );

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
