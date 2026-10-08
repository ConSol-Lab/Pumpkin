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
use super::ElementDescription;
use super::ElementPropagator;

#[derive(Clone, Debug)]
pub struct ElementArgs<VX, VI, VE> {
    pub constraint_description: ElementDescription<VX, VI, VE>,
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
            constraint_description,
            constraint_tag,
        } = self;
        let ElementDescription { array, index, rhs } = constraint_description;

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
        let inference_code = checkers.add_conflict_checker(
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

const ID_INDEX: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);

// local ids of array vars are shifted by ID_X_OFFSET
const ID_X_OFFSET: u32 = 2;
