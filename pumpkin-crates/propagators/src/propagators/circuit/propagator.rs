use pumpkin_core::declare_inference_label;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

use crate::circuit::CircuitChecker;

#[derive(Debug)]
pub struct CircuitConstructor<Var> {
    pub successors: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor for CircuitConstructor<Var> {
    type PropagatorImpl = CircuitPropagator<Var>;

    fn create(
        self,
        mut context: pumpkin_core::propagation::PropagatorConstructorContext,
    ) -> Self::PropagatorImpl {
        self.successors
            .iter()
            .enumerate()
            .for_each(|(index, successor)| {
                context.register(
                    successor.clone(),
                    DomainEvents::ASSIGN,
                    LocalId::from(index as u32),
                )
            });

        CircuitPropagator {
            successors: self.successors,
            inference_code: InferenceCode::new(self.constraint_tag, CircuitPrevent),
        }
    }

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, CircuitPrevent),
            Box::new(CircuitChecker {
                successors: self.successors.clone(),
            }),
        );
    }
}

declare_inference_label!(CircuitPrevent);

#[derive(Debug, Clone)]
pub struct CircuitPropagator<Var> {
    successors: Box<[Var]>,
    inference_code: InferenceCode,
}

impl<Var: IntegerVariable + 'static> Propagator for CircuitPropagator<Var> {
    fn name(&self) -> &str {
        "Circuit"
    }

    fn priority(&self) -> Priority {
        // TODO
        Priority::Medium
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        // Note that circuit is currently 1-indexed!
        self.check(context.domains())?;

        self.prevent(&mut context)
    }
}

impl<Var: IntegerVariable + 'static> CircuitPropagator<Var> {
    fn check(&self, context: Domains) -> PropagationStatusCP {
        todo!()
    }

    fn prevent(&self, context: &mut PropagationContext) -> PropagationStatusCP {
        todo!()
    }
}
