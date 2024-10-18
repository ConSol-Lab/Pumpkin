use super::Constraint;
use crate::propagators::node_packing::NodePackingPropagator;
use crate::propagators::ArgTask;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

pub fn node_packing<Var: IntegerVariable + 'static>(
    start_times: &[Var],
    durations: &[i32],
    resource_requirements: &[i32],
    resource_capacity: i32,
    num_cycles: usize,
    makespan_variable: Var,
) -> impl Constraint {
    pumpkin_assert_simple!(
        start_times.len() == durations.len() && durations.len() == resource_requirements.len(),
        "The number of start variables, durations and resource requirements should be the
same!"
    );

    NodePackingPropagator::new(
        &start_times
            .iter()
            .zip(durations)
            .zip(resource_requirements)
            .map(|((start_time, duration), resource_requirement)| ArgTask {
                start_time: start_time.clone(),
                processing_time: *duration,
                resource_usage: *resource_requirement,
            })
            .collect::<Vec<_>>(),
        resource_capacity,
        makespan_variable,
        num_cycles,
    )
}
