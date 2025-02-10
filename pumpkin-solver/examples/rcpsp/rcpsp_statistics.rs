use pumpkin_solver::variables::DomainId;
use pumpkin_solver::Solver;

use crate::precedence_closure::PrecedenceClosure;

// The density of the transitive closure of the precedence graph - a value of 0 means total
// parallelism and 1 means that they are totally ordered
/// A higher value can indicate that the instance is easier (since the size of the search space
/// decreases as we add more precedence constraints) The transitive closure (obtained by creating a
/// graph in which each node is connected to another node if there is a path to it in the original
/// graph) provides a way to ignore redundant edges in the original precedence graph
pub(crate) fn calculate_order_strength(
    precedences: &PrecedenceClosure,
    number_of_tasks: u32,
) -> f64 {
    precedences.num_edges() as f64 / ((number_of_tasks * (number_of_tasks + 1)) / 2) as f64
}

/// The average number of required resources
pub(crate) fn calculate_resource_factor(
    resource_requirements: &[Vec<u32>],
    number_of_tasks: u32,
) -> f64 {
    resource_requirements
        .iter()
        .map(|resource_requirements_per_resource| {
            resource_requirements_per_resource
                .iter()
                .filter(|resource_requirement| **resource_requirement > 0)
                .count()
        })
        .sum::<usize>() as f64
        / (resource_requirements.len() as u32 * number_of_tasks) as f64
}

/// The availability of a resource compared to the average level of activity requirements (per
/// resource) According to [De Reyck & Herroelen (1996)], the required CPU time of an instance
/// varies according to an easy-hard-easy bell curve as a function of this metric
pub(crate) fn calculate_resource_constrainedness(
    resource_requirements: &[Vec<u32>],
    resource_capacities: &[u32],
    number_of_tasks: u32,
) -> Vec<f64> {
    assert_eq!(resource_requirements.len(), resource_capacities.len());
    resource_requirements
        .iter()
        .zip(resource_capacities.iter())
        .map(|(resource_requirements_per_resource, resource_capacity)| {
            resource_requirements_per_resource.iter().sum::<u32>() as f64
                / (number_of_tasks * resource_capacity) as f64
        })
        .collect::<Vec<_>>()
}

/// The average ability of an activity to be shifted without shifting any other activity (also
/// called density) Instances with a lower FFR are seen as harder since a higher FFR allows more
/// flexibility
pub(crate) fn calculate_free_float_ratio(
    precedences: &PrecedenceClosure,
    processing_times: &[u32],
    variables: &[DomainId],
    csp_solver: &Solver,
) -> f64 {
    let horizon = processing_times.iter().sum::<u32>();
    let free_float_sum = (0..variables.len())
        .map(|task_index| {
            calculate_free_float_of_task(precedences, variables, task_index, csp_solver, horizon)
        })
        .sum::<i32>();
    horizon as f64 / (horizon as i32 + free_float_sum) as f64
}

fn calculate_free_float_of_task(
    precedences: &PrecedenceClosure,
    variables: &[DomainId],
    task_index: usize,
    solver: &Solver,
    horizon: u32,
) -> i32 {
    // TODO: should we use the transitive closure here or the original graph?
    // TODO: what should we return in case a task is not constrained?
    precedences
        .get_incoming_edges(task_index)
        .map(|precedence| {
            assert!(
                solver.lower_bound(&variables[precedence.predecessor])
                    <= solver.lower_bound(&variables[task_index]),
                "Expected task {} (EST {}) to start after {} (EST {})",
                precedence.predecessor,
                solver.lower_bound(&variables[precedence.predecessor]),
                task_index,
                solver.lower_bound(&variables[task_index]),
            );
            solver.lower_bound(&variables[task_index])
                - solver.lower_bound(&variables[precedence.predecessor])
        })
        .min()
        .unwrap_or(horizon as i32)
}

/// This indicator was designed to ensure that the smallest feasible resource availability for a
/// resource corresponds to 0 and 1 corresponds to the absence of resource constraints since the
/// peak resource demand for the earliest precedence-feasible schedule is satisfied
///
/// It has been shown that the required CPU time varies as a function of this metric according to a
/// bell-shaped easy-hard-easy pattern where the hard instnaces are closer to 0
///
/// There has been criticism that a single task can cause this value to be 0 irrespective of other
/// tasks
pub(crate) fn calculate_resource_strength(
    resource_requirements: &[Vec<u32>],
    resource_capacities: &[u32],
    solver: &Solver,
    variables: &[DomainId],
    processing_times: &Vec<u32>,
    horizon: u32,
) -> Vec<f64> {
    resource_requirements
        .iter()
        .zip(resource_capacities.iter())
        .map(|(requirements, capacity)| {
            let largest_resource_requirement: &u32 = requirements
                .iter()
                .max()
                .expect("Expect at least 1 task to be present");
            let peak_demand = (0..=horizon)
                .map(|t| {
                    variables
                        .iter()
                        .zip(processing_times)
                        .map(|(variable, processing_time)| -> u32 {
                            (solver.lower_bound(variable) + *processing_time as i32 > t as i32
                                && solver.lower_bound(variable) <= t as i32)
                                as u32
                                * processing_time
                        })
                        .sum::<u32>()
                })
                .max()
                .unwrap_or_default();
            // TODO: the book ("Resource-constrained project scheduling - Models, Algorithms,
            // Extensions and Applications") states that this should be multiplication (but does
            // state that 0 <= value <= 1) while "On the use of the complexity index as
            // a measure of complexity in activity networks" states that it should be division
            (capacity - largest_resource_requirement) as f64
                / (peak_demand - largest_resource_requirement) as f64
        })
        .collect::<Vec<_>>()
}

/// Indicates how cumulative and disjunctive instances are (respecticaly, instances with a low
/// disjunctive ratio and instances with a high disjunctive ratio) This metric can give an
/// indication of which techniques are more suited to which types of benchmarks
pub(crate) fn calculate_disjunction_ratio(
    precedences: &PrecedenceClosure,
    number_of_tasks: u32,
    resource_requirements: &[Vec<u32>],
    resource_capacities: &[u32],
) -> f64 {
    let number_of_resources = resource_requirements.len();
    let mut num_resource_infeasible = 0;
    (0..number_of_tasks as usize).for_each(|current| {
        (0..number_of_tasks as usize).for_each(|other| {
            if (0..number_of_resources).any(|resource| {
                resource_requirements[resource][current] + resource_requirements[resource][other]
                    > resource_capacities[resource]
            }) && !precedences.contains_edge(current, other)
                && !precedences.contains_edge(other, current)
            {
                num_resource_infeasible += 1;
            }
        })
    });

    (precedences.num_edges() + num_resource_infeasible) as f64
        / ((number_of_tasks * (number_of_tasks + 1)) as f64 / 2_f64)
}

pub(crate) fn calculate_process_range(processing_times: &[u32]) -> f64 {
    *processing_times
        .iter()
        .max()
        .expect("Expected at least 1 task in the instance") as f64
        / *processing_times.iter().min().unwrap() as f64
}

pub(crate) fn calculate_mandatory_ratio(
    variables: &[DomainId],
    processing_times: &[u32],
    solver: &Solver,
) -> f64 {
    variables
        .iter()
        .zip(processing_times)
        .filter(|(current, processing_time)| {
            solver.upper_bound(*current) < solver.lower_bound(*current) + **processing_time as i32
        })
        .count() as f64
        / variables.len() as f64
}

pub(crate) fn calculate_mandatory_constrainedness(
    variables: &[DomainId],
    processing_times: &[u32],
    resource_requirements: &[Vec<u32>],
    solver: &Solver,
    resource_capacities: &[u32],
    horizon: u32,
) -> Vec<f64> {
    resource_capacities
        .iter()
        .enumerate()
        .map(|(resource_index, resource_capacity)| {
            (0..horizon)
                .map(|t| {
                    variables
                        .iter()
                        .enumerate()
                        .zip(processing_times)
                        .filter_map(|((index, i), processing_time)| {
                            if solver.upper_bound(i) <= t as i32
                                && solver.lower_bound(i) + *processing_time as i32 > t as i32
                            {
                                return Some(resource_requirements[resource_index][index]);
                            }
                            None
                        })
                        .sum::<u32>()
                })
                .sum::<u32>() as f64
                / (resource_capacity * horizon) as f64
        })
        .collect::<Vec<_>>()
}
