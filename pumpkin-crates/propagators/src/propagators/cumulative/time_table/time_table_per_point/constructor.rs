use pumpkin_checking::checkers::CheckerTask;
use pumpkin_checking::checkers::TimeTableChecker;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::TimeTablePerPointPropagator;
use crate::cumulative::time_table::TimeTable;
use crate::cumulative::util::register_tasks;

impl<Var: IntegerVariable + 'static> PropagatorConstructor for TimeTablePerPointPropagator<Var> {
    type PropagatorImpl = Self;

    fn create(
        mut self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        self.updatable_structures
            .initialise_bounds_and_remove_fixed(context.domains(), &self.parameters);
        let registration = register_tasks(&self.parameters.tasks, context.reborrow(), false);

        let mut checkers = RuntimeCheckers::builder();
        self.inference_code = Some(
            checkers.add_inference_checker(
                self.constraint_tag,
                TimeTable,
                TimeTableChecker {
                    tasks: self
                        .parameters
                        .tasks
                        .iter()
                        .map(|task| CheckerTask {
                            start_time: task.start_variable.clone(),
                            processing_time: task.processing_time,
                            resource_usage: task.resource_usage,
                        })
                        .collect(),
                    capacity: self.parameters.capacity,
                },
            ),
        );

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator: self,
        }
    }
}
