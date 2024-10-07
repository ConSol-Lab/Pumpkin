use std::collections::BTreeSet;
use std::ops::Div;
use std::rc::Rc;

use log::warn;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::statistics::statistic_logger::StatisticLogger;
use crate::basic_types::PropagationStatusCP;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::propagation::ReadDomains;
use crate::engine::DomainEvents;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::util::create_tasks;
use crate::propagators::ArgTask;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

#[allow(unused)]
pub(crate) struct ParallelMachinePropagator<Var> {
    parameters: ParallelMachineParameters<Var>,
    makespan_variable: Var,
    responsible_tasks: Vec<(u32, u32)>,
    n_machine: usize,

    n_calls: usize,
    n_propagations: usize,
    n_conflicts: usize,
    average_size_of_propagation: CumulativeMovingAverage,
}

#[derive(Clone, Debug)]
pub(crate) struct ParallelMachineParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: i32,
}

impl<Var: IntegerVariable + Clone + 'static> ParallelMachinePropagator<Var> {
    pub(crate) fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        min_machine: usize,
        max_machine: usize,
        makespan_variable: Var,
    ) -> Vec<Self> {
        let parameters = ParallelMachineParameters {
            tasks: create_tasks(arg_tasks)
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            capacity,
        };
        (min_machine..max_machine)
            .map(|n_machine| {
                let min_resource_usage = 1 + parameters.capacity as u32 / (1 + n_machine as u32);

                let responsible_tasks = (0..parameters.tasks.len())
                    .map(|activity_index| {
                        let n_copies = parameters.tasks[activity_index].resource_usage as u32
                            / min_resource_usage;
                        (n_copies, activity_index as u32)
                    })
                    .filter(|(n_copies, _)| *n_copies != 0)
                    .collect();
                ParallelMachinePropagator {
                    responsible_tasks,
                    n_machine,
                    parameters: parameters.clone(),
                    makespan_variable: makespan_variable.clone(),
                    n_calls: 0,
                    n_propagations: 0,
                    n_conflicts: 0,
                    average_size_of_propagation: CumulativeMovingAverage::default(),
                }
            })
            .collect::<Vec<_>>()
    }

    fn get_parallel_machine_bound(&self, context: &PropagationContext) -> i32 {
        for (_, activity_index) in &self.responsible_tasks {
            let task = &self.parameters.tasks[(*activity_index) as usize];
            let max_makespan = context.upper_bound(&self.makespan_variable) as u32;
            let latest_finish_time =
                (context.upper_bound(&task.start_variable) + task.processing_time) as u32;
            if latest_finish_time > max_makespan {
                return context.lower_bound(&self.makespan_variable);
            }
        }
        let jobs = self
            .responsible_tasks
            .iter()
            .flat_map(|(n_copies, activity_index)| {
                let task = &self.parameters.tasks[(*activity_index) as usize];
                // TODO: double check lower or upper
                let max_makespan = context.upper_bound(&self.makespan_variable) as u32;

                let earliest_start_time = context.lower_bound(&task.start_variable) as u32;
                let latest_finish_time =
                    (context.upper_bound(&task.start_variable) + task.processing_time) as u32;
                // TODO underflow happens
                let tail_time = if latest_finish_time > max_makespan {
                    unreachable!();
                } else {
                    max_makespan - latest_finish_time
                };

                std::iter::repeat_with(move || ParallelMachineJob {
                    earliest_start_time,
                    duration: task.processing_time as u32,
                    tail_time,
                })
                .take((*n_copies) as usize)
            })
            .collect();
        ParallelMachineProblem {
            n_machines: self.n_machine,
            jobs,
        }
        .bound_makespan() as i32
    }
}

impl<Var: IntegerVariable + 'static> Propagator for ParallelMachinePropagator<Var> {
    fn name(&self) -> &str {
        "ParallelMachinePropagator"
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        statistic_logger.log_statistic("numberOfCalls", self.n_calls);
        statistic_logger.log_statistic("numberOfPropagations", self.n_propagations);
        statistic_logger.log_statistic("numberOfConflicts", self.n_conflicts);
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        self.n_calls += 1;

        let parallel_machine_bound = self.get_parallel_machine_bound(&context.as_readonly());

        if parallel_machine_bound > context.lower_bound(&self.makespan_variable) {
            self.average_size_of_propagation.add_term(
                (parallel_machine_bound - context.lower_bound(&self.makespan_variable)) as u64,
            );
            self.n_propagations += 1;
            let result = context.set_lower_bound(
                &self.makespan_variable,
                parallel_machine_bound,
                self.responsible_tasks
                    .iter()
                    .flat_map(|(_, index)| {
                        let task = &self.parameters.tasks[(*index) as usize];
                        vec![
                            predicate!(
                                task.start_variable >= context.lower_bound(&task.start_variable)
                            ),
                            predicate!(
                                task.start_variable <= context.upper_bound(&task.start_variable)
                            ),
                        ]
                    })
                    .collect::<PropositionalConjunction>(),
            );
            if result.is_err() {
                self.n_conflicts += 1;
                result?;
            }
        }
        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let parallel_machine_bound = self.get_parallel_machine_bound(&context.as_readonly());

        if parallel_machine_bound > context.lower_bound(&self.makespan_variable) {
            let result = context.set_lower_bound(
                &self.makespan_variable,
                parallel_machine_bound,
                self.responsible_tasks
                    .iter()
                    .flat_map(|(_, index)| {
                        let task = &self.parameters.tasks[(*index) as usize];
                        vec![
                            predicate!(
                                task.start_variable >= context.lower_bound(&task.start_variable)
                            ),
                            predicate!(
                                task.start_variable <= context.upper_bound(&task.start_variable)
                            ),
                        ]
                    })
                    .collect::<PropositionalConjunction>(),
            );
            if result.is_err() {
                result?;
            }
        }
        Ok(())
    }

    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        for (index, (_, task_index)) in self.responsible_tasks.iter().enumerate() {
            let task = &self.parameters.tasks[(*task_index) as usize];
            let _ = context.register(
                task.start_variable.clone(),
                DomainEvents::BOUNDS,
                LocalId::from(index as u32),
            );
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct ParallelMachineProblem {
    pub n_machines: usize,
    pub jobs: Vec<ParallelMachineJob>,
}

impl ParallelMachineProblem {
    fn bound_makespan(&self) -> u32 {
        // Choose all possible values of the lower bound on earliest start time
        let res = self
            .jobs
            .iter()
            .map(|h| {
                // We go over all jobs h, i in S such that est_h <= est_i /\ tail_h >= tail_i
                self.jobs
                    .iter()
                    .filter(|i| {
                        h.earliest_start_time <= i.earliest_start_time && h.tail_time >= i.tail_time
                    })
                    .map(|i| {
                        // Then the lower-bound on the makespan is the min(est) +
                        // ceil(sum(durations) / n_machine) + min(tail)
                        h.earliest_start_time
                            + i.tail_time
                            + self
                                .jobs
                                .iter()
                                .filter(|job| {
                                    job.earliest_start_time >= h.earliest_start_time
                                        && job.tail_time >= i.tail_time
                                })
                                .map(|job| (job.duration as f32))
                                .sum::<f32>()
                                .div(self.n_machines as f32)
                                .ceil() as u32
                    })
                    .max()
                    .unwrap_or(0)
            })
            .max()
            .unwrap_or(0);
        res
    }
}

#[derive(Debug, Clone)]
struct ParallelMachineJob {
    /// Named `head` in the LB5 paper
    pub earliest_start_time: u32,
    pub duration: u32,
    pub tail_time: u32,
}
