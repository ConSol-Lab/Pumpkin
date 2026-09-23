use pumpkin_checking::checkers::IntegerDivisionChecker;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::Division;
use super::DivisionPropagator;
use super::ID_DENOMINATOR;
use super::ID_NUMERATOR;
use super::ID_RHS;

/// The [`PropagatorConstructor`] for the [`DivisionPropagator`].
#[derive(Clone, Debug)]
pub struct DivisionArgs<VA, VB, VC> {
    pub numerator: VA,
    pub denominator: VB,
    pub rhs: VC,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB, VC> PropagatorConstructor for DivisionArgs<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type PropagatorImpl = DivisionPropagator<VA, VB, VC>;

    fn create(self, context: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let DivisionArgs {
            numerator,
            denominator,
            rhs,
            constraint_tag,
        } = self;

        pumpkin_assert_simple!(
            !context.contains(&denominator, 0),
            "Denominator cannot contain 0"
        );

        let registration = EventsToRegister::builder()
            .add(&numerator, DomainEvents::BOUNDS, ID_NUMERATOR)
            .add(&denominator, DomainEvents::BOUNDS, ID_DENOMINATOR)
            .add(&rhs, DomainEvents::BOUNDS, ID_RHS)
            .build();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            constraint_tag,
            Division,
            IntegerDivisionChecker {
                numerator: numerator.clone(),
                denominator: denominator.clone(),
                rhs: rhs.clone(),
            },
        );

        let propagator = DivisionPropagator {
            numerator,
            denominator,
            rhs,
            inference_code,
        };

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator,
        }
    }
}
