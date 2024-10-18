use std::cmp::max;
use std::cmp::min;
use std::collections::VecDeque;
use std::ops::Div;
use std::rc::Rc;

use super::has_overlap_with_interval;
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
pub(crate) struct NodePackingPropagator<Var> {
    parameters: NodePackingParameters<Var>,
    makespan_variable: Var,

    number_of_cycles: usize,

    n_calls: usize,
    n_propagations: usize,
    n_conflicts: usize,
    average_size_of_propagation: CumulativeMovingAverage,
}

#[derive(Clone, Debug)]
pub(crate) struct NodePackingParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: i32,
}

impl<Var: IntegerVariable + Clone + 'static> NodePackingPropagator<Var> {
    pub(crate) fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        makespan_variable: Var,
        number_of_cycles: usize,
    ) -> Self {
        let parameters = NodePackingParameters {
            tasks: create_tasks(arg_tasks)
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            capacity,
        };

        NodePackingPropagator {
            parameters: parameters.clone(),
            makespan_variable: makespan_variable.clone(),
            number_of_cycles,
            n_calls: 0,
            n_propagations: 0,
            n_conflicts: 0,
            average_size_of_propagation: CumulativeMovingAverage::default(),
        }
    }

    fn create_initial_activity_list(&self) -> VecDeque<Rc<Task<Var>>> {
        self.parameters.tasks.to_vec().into()
    }

    fn rotate_activities(
        &self,
        cycle_number: usize,
        activities: &mut VecDeque<Rc<Task<Var>>>,
    ) -> Result<(), ()> {
        if cycle_number >= activities.len() {
            return Err(());
        }
        activities.rotate_right(cycle_number);
        Ok(())
    }

    fn remove_compatible(
        &self,
        context: PropagationContext,
        activities: &mut VecDeque<Rc<Task<Var>>>,
        selected_activity: Rc<Task<Var>>,
    ) {
        activities.retain(|activity| {
            let has_overlap_with_interval = has_overlap_with_interval(
                context.lower_bound(&activity.start_variable),
                context.upper_bound(&activity.start_variable) + 1,
                context.lower_bound(&selected_activity.start_variable),
                context.upper_bound(&selected_activity.start_variable),
            );
            // Either they would exceed the resource capacities or their executions times cannot
            // overlap
            selected_activity.resource_usage + activity.resource_usage > self.parameters.capacity
                || !has_overlap_with_interval
        })
    }

    fn node_packing_bound(&self, context: PropagationContext<'_>) -> (i32, Vec<Rc<Task<Var>>>) {
        let mut max_lower_bound = i32::MIN;
        let mut tasks = vec![];

        let initial_activity_list = self.create_initial_activity_list();

        for cycle_number in 0..=min(self.number_of_cycles, initial_activity_list.len()) {
            let mut sum_duration_selected = 0;
            let mut selected_activities = vec![];

            let mut activities = initial_activity_list.clone();
            let _ = self.rotate_activities(cycle_number, &mut activities);

            while !activities.is_empty() {
                let selected_activity = activities.pop_front().unwrap();

                sum_duration_selected += selected_activity.processing_time;
                self.remove_compatible(context, &mut activities, Rc::clone(&selected_activity));

                selected_activities.push(selected_activity);
            }

            if sum_duration_selected > max_lower_bound {
                max_lower_bound = sum_duration_selected;
                tasks = selected_activities
            }
        }
        (max_lower_bound, tasks)
    }
}

impl<Var: IntegerVariable + 'static> Propagator for NodePackingPropagator<Var> {
    fn name(&self) -> &str {
        "NodePackingPropagator"
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        statistic_logger.log_statistic("numberOfCalls", self.n_calls);
        statistic_logger.log_statistic("numberOfPropagations", self.n_propagations);
        statistic_logger.log_statistic("numberOfConflicts", self.n_conflicts);
        statistic_logger.log_statistic(
            "averageSizeOfPropagation",
            self.average_size_of_propagation.value(),
        )
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        self.n_calls += 1;

        let (node_packing_bound, tasks) = self.node_packing_bound(context.as_readonly());

        if node_packing_bound > context.lower_bound(&self.makespan_variable) {
            self.average_size_of_propagation.add_term(
                (node_packing_bound - context.lower_bound(&self.makespan_variable)) as u64,
            );
            self.n_propagations += 1;
            let result = context.set_lower_bound(
                &self.makespan_variable,
                node_packing_bound,
                tasks
                    .iter()
                    .flat_map(|task| {
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
        let (node_packing_bound, tasks) = self.node_packing_bound(context.as_readonly());

        if node_packing_bound > context.lower_bound(&self.makespan_variable) {
            let result = context.set_lower_bound(
                &self.makespan_variable,
                node_packing_bound,
                tasks
                    .iter()
                    .flat_map(|task| {
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
        for (index, task) in self.parameters.tasks.iter().enumerate() {
            let _ = context.register(
                task.start_variable.clone(),
                DomainEvents::BOUNDS,
                LocalId::from(index as u32),
            );
        }
        Ok(())
    }
}
