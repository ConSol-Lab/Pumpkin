use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::Division;
use super::DivisionDescription;
use super::DivisionPropagator;
use super::IntegerDivisionChecker;

/// The [`PropagatorConstructor`] for the [`DivisionPropagator`].
#[derive(Clone, Debug)]
pub struct DivisionArgs<VA, VB, VC> {
    pub constraint_description: DivisionDescription<VA, VB, VC>,
    pub constraint_tag: ConstraintTag,
}

const ID_NUMERATOR: LocalId = LocalId::from(0);
const ID_DENOMINATOR: LocalId = LocalId::from(1);
const ID_RHS: LocalId = LocalId::from(2);

impl<VA, VB, VC> PropagatorConstructor for DivisionArgs<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type PropagatorImpl = DivisionPropagator<VA, VB, VC>;

    fn create(self, context: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let DivisionArgs {
            constraint_description,
            constraint_tag,
        } = self;
        let DivisionDescription {
            numerator,
            denominator,
            rhs,
        } = constraint_description;

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
        let inference_code = checkers.add_conflict_checker(
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
