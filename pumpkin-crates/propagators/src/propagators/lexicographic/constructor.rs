use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorWithEvents;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

use super::LexLessOrEqualDescription;
use super::LexLessOrEqualPropagator;
use super::LexLessOrEqualRule;

#[derive(Clone, Debug)]
pub struct LexLessOrEqualArgs<Var> {
    pub constraint_description: LexLessOrEqualDescription<Var>,
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LexLessOrEqualArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LexLessOrEqualPropagator<Var>;
    type Rule = LexLessOrEqualRule<Var>;

    fn constraint_description(&self) -> LexLessOrEqualDescription<Var> {
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
        let LexLessOrEqualArgs {
            constraint_description: LexLessOrEqualDescription { x, y },
            constraint_tag: _,
        } = self;
        assert_eq!(
            x.len(),
            y.len(),
            "lex_less_or_equal needs vectors of equal length"
        );

        let mut registration = EventsToRegister::builder();
        for (index, var) in x.iter().chain(y.iter()).enumerate() {
            registration = registration.add(var, DomainEvents::BOUNDS, LocalId::from(index as u32));
        }

        PropagatorWithEvents {
            events_to_register: registration.build(),
            propagator: LexLessOrEqualPropagator {
                alpha: context.new_trailed_integer(0),
                beta: context.new_trailed_integer(x.len() as i64),
                x,
                y,
                inference_code,
                reason_buffer: Vec::new(),
            },
        }
    }
}
