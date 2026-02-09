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

use crate::propagators::all_different::AllDifferentChecker;

declare_inference_label!(AllDifferent);

#[derive(Clone, Debug)]
pub struct AllDifferentConstructor<Var> {
    pub x: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
    pub conflict_detection_only: bool,
}

impl<Var> PropagatorConstructor for AllDifferentConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = AllDifferentPropagator<Var>;

    fn create(self, _context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        // Register for events

        AllDifferentPropagator {
            // TODO
            conflict_detection_only: self.conflict_detection_only,
            _inference_code: InferenceCode::new(self.constraint_tag, AllDifferent),
            phantom_data: PhantomData,
        }
    }

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, AllDifferent),
            Box::new(AllDifferentChecker { x: self.x.to_vec() }),
        );
    }
}

/// Propagator for the Circuit constraint.
#[derive(Clone, Debug)]
pub struct AllDifferentPropagator<Var> {
    // TODO
    conflict_detection_only: bool,
    _inference_code: InferenceCode,
    /// Here to avoid build warnings
    phantom_data: PhantomData<Var>,
}

impl<Var: 'static> Propagator for AllDifferentPropagator<Var>
where
    Var: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::Low
    }

    fn name(&self) -> &str {
        "AllDifferent"
    }

    fn propagate_from_scratch(&self, mut _context: PropagationContext) -> PropagationStatusCP {
        if self.conflict_detection_only {
            // TODO: Only perform conflict detection

            #[allow(
                unreachable_code,
                reason = "Should not be a warning after implementing"
            )]
            return todo!();
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
