use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::variables::IntegerVariable;

use super::TimeTablePerPointPropagator;
use crate::cumulative::time_table::CumulativeDescription;
use crate::cumulative::time_table::TimeTableRule;
use crate::cumulative::util::register_tasks;

impl<Var: IntegerVariable + 'static> PropagatorConstructor for TimeTablePerPointPropagator<Var> {
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
        self.updatable_structures
            .initialise_bounds_and_remove_fixed(context.domains(), &self.parameters);
        let registration = register_tasks(&self.parameters.tasks, context.reborrow(), false);

        self.inference_code = Some(inference_code);

        PropagatorSpec {
            registration,
            propagator: self,
        }
    }
}
