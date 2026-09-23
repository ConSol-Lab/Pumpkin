use std::fmt::Debug;

use pumpkin_checking::checkers::CheckerTask;
use pumpkin_checking::checkers::TimeTableChecker;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::TimeTablePerPointIncrementalPropagator;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTablePerPointPropagator;
use crate::cumulative::util::register_tasks;
use crate::propagators::cumulative::time_table::TimeTable;

impl<Var: IntegerVariable + 'static + Debug, const SYNCHRONISE: bool> PropagatorConstructor
    for TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE>
{
    type PropagatorImpl = Self;

    fn create(
        mut self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let registration = register_tasks(&self.parameters.tasks, context.reborrow(), true);
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters);

        // Then we do normal propagation
        self.is_time_table_outdated = true;

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
