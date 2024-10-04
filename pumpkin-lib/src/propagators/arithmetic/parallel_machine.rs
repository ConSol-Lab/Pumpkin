use std::collections::BTreeSet;
use std::rc::Rc;

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
                            / min_resource_usage
                            - 1;
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
                }
            })
            .collect::<Vec<_>>()
    }

    fn get_parallel_machine_bound(&self, context: &PropagationContext) -> i32 {
        ParallelMachineProblem {
            n_machines: self.n_machine,
            jobs: self
                .responsible_tasks
                .iter()
                .flat_map(|(n_copies, activity_index)| {
                    let task = &self.parameters.tasks[(*activity_index) as usize];
                    // TODO: double check lower or upper
                    let max_makespan = context.upper_bound(&self.makespan_variable) as u32;

                    let earliest_start_time = context.lower_bound(&task.start_variable) as u32;
                    let tail_time = max_makespan
                        - (context.upper_bound(&task.start_variable) + task.processing_time) as u32;

                    std::iter::repeat_with(move || ParallelMachineJob {
                        earliest_start_time,
                        duration: task.processing_time as u32,
                        tail_time,
                    })
                    .take((*n_copies) as usize)
                })
                .collect(),
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

struct ParallelMachineProblem {
    pub n_machines: usize,
    pub jobs: Vec<ParallelMachineJob>,
}

impl ParallelMachineProblem {
    fn bound_makespan(&self) -> u32 {
        let est_bounds = self
            .jobs
            .iter()
            .map(|job| job.earliest_start_time)
            .collect::<BTreeSet<_>>();
        let tail_bounds = self
            .jobs
            .iter()
            .map(|job| job.tail_time)
            .collect::<BTreeSet<_>>();
        est_bounds
            .iter()
            .flat_map(|&est_lower_bound| {
                tail_bounds
                    .iter()
                    .map(|&tail_upper_bound| {
                        est_lower_bound
                            + tail_upper_bound
                            + self
                                .jobs
                                .iter()
                                .filter(|job| {
                                    job.earliest_start_time >= est_lower_bound
                                        && job.tail_time <= tail_upper_bound
                                })
                                .map(|job| (job.duration as f32) / (self.n_machines as f32))
                                .sum::<f32>()
                                .ceil() as u32
                    })
                    .collect::<Vec<_>>()
            })
            .max()
            .unwrap_or(0)
    }
}

struct ParallelMachineJob {
    /// Named `head` in the LB5 paper
    pub earliest_start_time: u32,
    pub duration: u32,
    pub tail_time: u32,
}
