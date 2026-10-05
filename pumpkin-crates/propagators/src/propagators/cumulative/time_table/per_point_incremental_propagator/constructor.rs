use std::fmt::Debug;

use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::variables::IntegerVariable;

use super::TimeTablePerPointIncrementalPropagator;
use crate::cumulative::time_table::CumulativeDescription;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTablePerPointPropagator;
use crate::cumulative::time_table::TimeTableRule;
use crate::cumulative::util::register_tasks;

impl<Var: IntegerVariable + 'static + Debug, const SYNCHRONISE: bool> PropagatorConstructor
    for TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE>
{
    type PropagatorImpl = Self;
    type Rule = TimeTableRule<Var>;

    fn constraint_description(&self) -> CumulativeDescription<Var> {
        CumulativeDescription::from_parameters(&self.parameters)
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        mut self,
        mut context: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let registration = register_tasks(&self.parameters.tasks, context.reborrow(), true);
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters);

        // Then we do normal propagation
        self.is_time_table_outdated = true;

        self.inference_code = Some(inference_code);

        PropagatorSpec {
            registration,
            propagator: self,
        }
    }
}
