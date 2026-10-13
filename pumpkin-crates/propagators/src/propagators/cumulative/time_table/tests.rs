use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;

use super::CumulativeDescription;
use crate::cumulative::ArgTask;
use crate::fixed_domains;

#[test]
fn cumulative_is_violated_by_an_overload_at_a_start_time() {
    let (variables, domains) = fixed_domains(&[0, 2]);
    let description = CumulativeDescription {
        tasks: Box::from([
            ArgTask {
                start_time: variables[0],
                processing_time: 3,
                resource_usage: 2,
            },
            ArgTask {
                start_time: variables[1],
                processing_time: 3,
                resource_usage: 2,
            },
        ]),
        capacity: 3,
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintViolated
    );
}

#[test]
fn cumulative_allows_a_task_to_start_when_another_ends() {
    let (variables, domains) = fixed_domains(&[0, 3]);
    let description = CumulativeDescription {
        tasks: Box::from([
            ArgTask {
                start_time: variables[0],
                processing_time: 3,
                resource_usage: 2,
            },
            ArgTask {
                start_time: variables[1],
                processing_time: 3,
                resource_usage: 2,
            },
        ]),
        capacity: 3,
    };

    assert_eq!(
        description.check_solution(&domains),
        SolutionCheck::ConstraintSatisfied
    );
}
