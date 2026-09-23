use pumpkin_checking::checkers::ElementChecker;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::Element;
use super::ElementPropagator;
use super::ID_INDEX;
use super::ID_RHS;
use super::ID_X_OFFSET;

#[derive(Clone, Debug)]
pub struct ElementArgs<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
    pub constraint_tag: ConstraintTag,
}

impl<VX, VI, VE> PropagatorConstructor for ElementArgs<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    type PropagatorImpl = ElementPropagator<VX, VI, VE>;

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let ElementArgs {
            array,
            index,
            rhs,
            constraint_tag,
        } = self;

        let mut registration = EventsToRegister::builder();
        for (i, x_i) in array.iter().enumerate() {
            registration = registration.add(
                x_i,
                DomainEvents::ANY_INT,
                LocalId::from(i as u32 + ID_X_OFFSET),
            );
        }

        registration = registration.add(&index, DomainEvents::ANY_INT, ID_INDEX);
        registration = registration.add(&rhs, DomainEvents::ANY_INT, ID_RHS);

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            constraint_tag,
            Element,
            ElementChecker::new(array.clone(), index.clone(), rhs.clone()),
        );

        let propagator = ElementPropagator {
            array,
            index,
            rhs,
            inference_code,
            rhs_reason_buffer: vec![],
        };

        PropagatorSpec {
            registration: registration.build(),
            checkers: checkers.build(),
            propagator,
        }
    }
}
