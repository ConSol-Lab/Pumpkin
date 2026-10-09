use pumpkin_checking::checkers::LinearLessOrEqualConflictChecker;
use pumpkin_core::declare_inference_label;
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

use super::LinearLessOrEqualDescription;
use super::LinearLessOrEqualPropagator;

declare_inference_label!(LinearBounds);

/// The [`PropagatorConstructor`] for the [`LinearLessOrEqualPropagator`].
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualPropagatorArgs<Var> {
    pub constraint_description: LinearLessOrEqualDescription<Var>,
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
            constraint_description,
            constraint_tag,
        } = self;
        let LinearLessOrEqualDescription { terms: x, bound: c } = constraint_description;

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
        let inference_code = checkers.add_conflict_checker(
            constraint_tag,
            LinearBounds,
            LinearLessOrEqualConflictChecker::new(x.clone(), c),
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
