use std::marker::PhantomData;

use pumpkin_core::declare_inference_label;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

declare_inference_label!(CircuitForwardCheck);

#[derive(Clone, Debug)]
pub struct CircuitConstructor<Var> {
    pub successors: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for CircuitConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = CircuitPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        // Register for events

        CircuitPropagator {
            // TODO
            _inference_code: context
                .create_inference_code(self.constraint_tag, CircuitForwardCheck),
            phantom_data: PhantomData,
        }
    }
}

/// Propagator for the Circuit constraint.
#[derive(Clone, Debug)]
pub struct CircuitPropagator<Var> {
    // TODO
    _inference_code: InferenceCode,
    /// Here to avoid build warnings
    phantom_data: PhantomData<Var>,
}

impl<Var: 'static> Propagator for CircuitPropagator<Var>
where
    Var: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::VeryLow
    }

    fn name(&self) -> &str {
        "Circuit"
    }

    fn propagate_from_scratch(&self, mut _context: PropagationContext) -> PropagationStatusCP {
        // TODO
        todo!()
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        // TODO: create test cases here
    }
}
