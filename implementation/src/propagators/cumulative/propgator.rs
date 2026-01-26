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

use crate::propagators::cumulative::CumulativeChecker;

declare_inference_label!(CumulativeTimeTable);

#[derive(Clone, Debug)]
pub struct CumulativeConstructor<Var> {
    pub tasks: Box<[Task<Var>]>,
    pub capacity: u32,
    pub constraint_tag: ConstraintTag,
    pub conflict_detection_only: bool,
}

#[derive(Clone, Debug)]
pub struct Task<Var> {
    pub start_time: Var,
    pub duration: u32,
    pub resource_usage: u32,
}

impl<Var> PropagatorConstructor for CumulativeConstructor<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = CumulativeTimeTablePropagator<Var>;

    fn create(self, _context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        // Register for events

        CumulativeTimeTablePropagator {
            // TODO
            conflict_detection_only: self.conflict_detection_only,
            _inference_code: InferenceCode::new(self.constraint_tag, CumulativeTimeTable),
            phantom_data: PhantomData,
        }
    }

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, CumulativeTimeTable),
            Box::new(CumulativeChecker {
                tasks: self.tasks.to_vec(),
                capacity: self.capacity,
            }),
        );
    }
}

/// Propagator for the Cumulative constraint using time-tabling.
#[derive(Clone, Debug)]
pub struct CumulativeTimeTablePropagator<Var> {
    // TODO
    conflict_detection_only: bool,
    _inference_code: InferenceCode,
    /// Here to avoid build warnings
    phantom_data: PhantomData<Var>,
}

impl<Var: 'static> Propagator for CumulativeTimeTablePropagator<Var>
where
    Var: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::Low
    }

    fn name(&self) -> &str {
        "CumulativeTimeTable"
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
