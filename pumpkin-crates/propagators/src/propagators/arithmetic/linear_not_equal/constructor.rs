use std::rc::Rc;

use enumset::enum_set;
use pumpkin_checking::checkers::LinearNotEqualChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::LinearNotEqualPropagator;
use super::LinearNotEquals;

/// The [`PropagatorConstructor`] for the [`LinearNotEqualPropagator`].
#[derive(Clone, Debug)]
pub struct LinearNotEqualPropagatorArgs<Var> {
    /// The terms of the sum
    pub terms: Rc<[Var]>,
    /// The right-hand side of the sum
    pub rhs: i32,
    /// The constraint tag of the constraint this propagator is propagating for.
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LinearNotEqualPropagatorArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LinearNotEqualPropagator<Var>;

    fn create(
        self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let LinearNotEqualPropagatorArgs {
            terms,
            rhs,
            constraint_tag,
        } = self;

        let mut registration = EventsToRegister::builder();
        for (i, x_i) in terms.iter().enumerate() {
            registration = registration.add(x_i, DomainEvents::ASSIGN, LocalId::from(i as u32));
            context.register_backtrack(
                x_i.clone(),
                DomainEvents::new(enum_set!(DomainEvent::Assign | DomainEvent::Removal)),
                LocalId::from(i as u32),
            );
        }

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_rule(
            Scope::from_variables(terms.iter()),
            constraint_tag,
            LinearNotEquals,
            LinearNotEqualChecker {
                terms: terms.as_ref().into(),
                bound: rhs,
            },
        );

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

        PropagatorSpec {
            registration: registration.build(),
            checkers: checkers.build(),
            propagator,
        }
    }
}
