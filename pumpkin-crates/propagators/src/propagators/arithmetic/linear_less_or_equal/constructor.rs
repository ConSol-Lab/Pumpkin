use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

use super::LinearLessOrEqualDescription;
use super::LinearLessOrEqualPropagator;
use super::LinearLessOrEqualRule;

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
    type Rule = LinearLessOrEqualRule<Var>;

    fn constraint_description(&self) -> LinearLessOrEqualDescription<Var> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        mut context: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let LinearLessOrEqualDescription { terms: x, bound: c } = self.constraint_description;

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
            propagator,
        }
    }
}
