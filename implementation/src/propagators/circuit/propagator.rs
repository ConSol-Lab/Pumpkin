use std::marker::PhantomData;

use pumpkin_core::declare_inference_label;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
#[allow(unused, reason = "Will be used in the assignments")]
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

use crate::propagators::circuit::CircuitChecker;

declare_inference_label!(Circuit);

#[derive(Clone, Debug)]
pub struct CircuitConstructor<Var> {
    pub successors: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
    pub conflict_detection_only: bool,
}

impl<Var> PropagatorConstructor for CircuitConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = CircuitPropagator<Var>;

    fn create(self, _context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        // Register for events

        CircuitPropagator {
            // TODO
            conflict_detection_only: self.conflict_detection_only,
            _inference_code: InferenceCode::new(self.constraint_tag, Circuit),
            phantom_data: PhantomData,
        }
    }

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, Circuit),
            Box::new(CircuitChecker {
                successors: self.successors.to_vec(),
            }),
        );
    }
}

/// Propagator for the Circuit constraint.
#[derive(Clone, Debug)]
pub struct CircuitPropagator<Var> {
    // TODO
    conflict_detection_only: bool,
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
        if self.conflict_detection_only {
            // TODO: Only perform conflict detection
            todo!();
        }

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
