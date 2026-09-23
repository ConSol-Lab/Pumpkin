use pumpkin_checking::checkers::LinearLessOrEqualChecker;
use pumpkin_core::checkers::Scope;
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

use super::LinearBounds;
use super::LinearLessOrEqualPropagator;

/// The [`PropagatorConstructor`] for the [`LinearLessOrEqualPropagator`].
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualPropagatorArgs<Var> {
    pub x: Box<[Var]>,
    pub c: i32,
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LinearLessOrEqualPropagatorArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LinearLessOrEqualPropagator<Var>;

    fn create(
        self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let LinearLessOrEqualPropagatorArgs {
            x,
            c,
            constraint_tag,
        } = self;

        let mut lower_bound_left_hand_side = 0_i64;
        let mut current_bounds = vec![];

        let mut registration = EventsToRegister::builder();
        for (i, x_i) in x.iter().enumerate() {
            registration =
                registration.add(x_i, DomainEvents::LOWER_BOUND, LocalId::from(i as u32));
            lower_bound_left_hand_side += context.lower_bound(x_i) as i64;
            current_bounds.push(context.new_trailed_integer(context.lower_bound(x_i) as i64));
        }

        let lower_bound_left_hand_side = context.new_trailed_integer(lower_bound_left_hand_side);

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_rule(
            Scope::from_variables(x.iter()),
            constraint_tag,
            LinearBounds,
            LinearLessOrEqualChecker::new(x.clone(), c),
        );

        let propagator = LinearLessOrEqualPropagator {
            x,
            c,
            lower_bound_left_hand_side,
            current_bounds: current_bounds.into(),
            inference_code,
            reason_buffer: Vec::default(),
        };

        PropagatorSpec {
            registration: registration.build(),
            checkers: checkers.build(),
            propagator,
        }
    }
}
