use super::Constraint;
use crate::propagators::ArgTask;
use crate::propagators::NodePackingPropagator;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

pub fn node_packing<Var: IntegerVariable + 'static>(
    start_times: &[Var],
    durations: &[i32],
    num_cycles: usize,
    makespan_variable: Var,
    static_incompatibilities: Vec<Vec<bool>>,
) -> impl Constraint {
    pumpkin_assert_simple!(
        start_times.len() == durations.len(),
        "The number of start variables, durations and resource requirements should be the
same!"
    );

    NodePackingPropagator::new(
        &start_times
            .iter()
            .zip(durations)
            .map(|(start_time, duration)| ArgTask {
                start_time: start_time.clone(),
                processing_time: *duration,
                // FIXME This code should not be aware of the concept of resource
                resource_usage: 1,
            })
            .collect::<Vec<_>>(),
        makespan_variable,
        num_cycles,
        static_incompatibilities,
    )
}
