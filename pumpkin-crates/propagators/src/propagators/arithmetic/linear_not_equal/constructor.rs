use enumset::enum_set;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorWithEvents;
use pumpkin_core::variables::IntegerVariable;

use super::LinearNotEqualDescription;
use super::LinearNotEqualPropagator;
use super::LinearNotEqualRule;

/// The [`PropagatorConstructor`] for the [`LinearNotEqualPropagator`].
#[derive(Clone, Debug)]
pub struct LinearNotEqualPropagatorArgs<Var> {
    pub constraint_description: LinearNotEqualDescription<Var>,
    /// The constraint tag of the constraint this propagator is propagating for.
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LinearNotEqualPropagatorArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LinearNotEqualPropagator<Var>;
    type Rule = LinearNotEqualRule<Var>;

    fn constraint_description(&self) -> LinearNotEqualDescription<Var> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        mut context: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorWithEvents<Self::PropagatorImpl> {
        let LinearNotEqualDescription { terms, rhs } = self.constraint_description;

        let mut events_to_register = EventsToRegister::builder();
        for (i, x_i) in terms.iter().enumerate() {
            events_to_register =
                events_to_register.add(x_i, DomainEvents::ASSIGN, LocalId::from(i as u32));
            context.register_backtrack(
                x_i.clone(),
                DomainEvents::new(enum_set!(DomainEvent::Assign | DomainEvent::Removal)),
                LocalId::from(i as u32),
            );
        }

        let mut propagator = LinearNotEqualPropagator {
            terms,
            rhs,
            number_of_fixed_terms: 0,
            fixed_lhs: 0,
            unfixed_variable_has_been_updated: false,
            should_recalculate_lhs: false,
            inference_code,
        };

        propagator.recalculate_fixed_variables(context.domains());

        PropagatorWithEvents {
            events_to_register: events_to_register.build(),
            propagator,
        }
    }
}
