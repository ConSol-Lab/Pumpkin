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

declare_inference_label!(LinearBounds);

#[derive(Clone, Debug)]
pub struct LinearConstructor<Var> {
    pub x: Box<[Var]>,
    pub c: i32,
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LinearConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LinearLessOrEqualPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        // Register for events

        LinearLessOrEqualPropagator {
            // TODO
            _inference_code: context.create_inference_code(self.constraint_tag, LinearBounds),
            phantom_data: PhantomData,
        }
    }
}

/// Propagator for the constraint `\sum x_i <= c`.
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualPropagator<Var> {
    // TODO
    _inference_code: InferenceCode,
    /// Here to avoid build warnings
    phantom_data: PhantomData<Var>,
}

impl<Var: 'static> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "LinearLeq"
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
        // TODO: we will create test cases here
    }
}
