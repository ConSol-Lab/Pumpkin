use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorWithEvents;
use pumpkin_core::variables::IntegerVariable;

use super::TimeTableOverIntervalIncrementalPropagator;
use crate::cumulative::time_table::CumulativeDescription;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTableOverIntervalPropagator;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTablePerPointPropagator;
use crate::cumulative::time_table::TimeTableRule;
use crate::cumulative::util::register_tasks;

impl<Var: IntegerVariable + 'static, const SYNCHRONISE: bool> PropagatorConstructor
    for TimeTableOverIntervalIncrementalPropagator<Var, SYNCHRONISE>
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
    ) -> PropagatorWithEvents<Self::PropagatorImpl> {
        // We only register for notifications of backtrack events if incremental backtracking is
        // enabled
        let events_to_register = register_tasks(
            &self.parameters.tasks,
            context.reborrow(),
            self.parameters.options.incremental_backtracking,
        );

        // First we store the bounds in the parameters
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters);

        self.is_time_table_outdated = true;

        self.inference_code = Some(inference_code);

        PropagatorWithEvents {
            events_to_register,
            propagator: self,
        }
    }
}
