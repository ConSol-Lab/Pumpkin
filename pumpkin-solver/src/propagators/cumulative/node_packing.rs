use std::cmp::min;
use std::collections::VecDeque;
use std::rc::Rc;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::PropagationStatusCP;
use crate::create_statistics_struct;
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
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::IntegerVariable;

create_statistics_struct!(NodePackingStatistics {
    n_calls: usize,
    n_propagations: usize,
    n_conflicts: usize,
    n_lft_conflicts: usize,

    average_size_of_propagation: CumulativeMovingAverage,
});

pub(crate) struct NodePackingPropagator<Var> {
    parameters: NodePackingParameters<Var>,
    makespan_variable: Var,

    number_of_cycles: usize,

    statistics: NodePackingStatistics,
}

#[derive(Clone, Debug)]
pub(crate) struct NodePackingParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) static_incompatibilities: Vec<Vec<bool>>,
}

impl<Var: IntegerVariable + Clone + 'static> NodePackingPropagator<Var> {
    pub(crate) fn new(
        arg_tasks: &[ArgTask<Var>],
        makespan_variable: Var,
        number_of_cycles: usize,
        static_incompatibilities: Vec<Vec<bool>>,
    ) -> Self {
        let parameters = NodePackingParameters {
            tasks: create_tasks(arg_tasks)
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            static_incompatibilities,
        };

        NodePackingPropagator {
            parameters: parameters.clone(),
            makespan_variable: makespan_variable.clone(),
            number_of_cycles,
            statistics: NodePackingStatistics::default(),
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
            // Either they would exceed the resource capacities or their executions times cannot
            // overlap
            self.parameters.static_incompatibilities[selected_activity.id.unpack() as usize]
                [activity.id.unpack() as usize]
                || !(context.lower_bound(&activity.start_variable)
                    <= context.upper_bound(&selected_activity.start_variable)
                        + selected_activity.processing_time
                    && context.lower_bound(&selected_activity.start_variable)
                        <= context.upper_bound(&activity.start_variable) + activity.processing_time)
        })
    }

    #[allow(clippy::type_complexity, reason = "Should be refactored")]
    fn node_packing_bound(
        &self,
        context: PropagationContext<'_>,
    ) -> Result<(i32, Vec<Rc<Task<Var>>>), PropositionalConjunction> {
        let mut max_lower_bound = i32::MIN;
        let mut tasks = vec![];

        let initial_activity_list = self.create_initial_activity_list();

        for cycle_number in 0..=min(self.number_of_cycles, initial_activity_list.len()) {
            let mut sum_duration_selected = 0;
            let mut selected_activities = vec![];

            let mut activities = initial_activity_list.clone();
            let result = self.rotate_activities(cycle_number, &mut activities);
            if result.is_err() {
                return Ok((max_lower_bound, tasks));
            }

            // We attempt to find the smallest clique which are mutually exclusive such that it
            // finds a conflict
            while !activities.is_empty()
                && sum_duration_selected <= context.upper_bound(&self.makespan_variable)
            {
                let selected_activity = activities.pop_front().unwrap();

                sum_duration_selected += selected_activity.processing_time;
                self.remove_compatible(context, &mut activities, Rc::clone(&selected_activity));

                selected_activities.push(selected_activity);
            }

            selected_activities.sort_by(|a, b| {
                context
                    .lower_bound(&a.start_variable)
                    .cmp(&context.lower_bound(&b.start_variable))
            });

            let mut updated = false;
            let mut _index = 0;
            for (activity_index, selected_activity) in selected_activities.iter().enumerate() {
                let maximum_finish_time = selected_activities[activity_index..]
                    .iter()
                    .max_by_key(|a| context.upper_bound(&a.start_variable) + a.processing_time)
                    .map(|task| context.upper_bound(&task.start_variable) + task.processing_time)
                    .expect("Expected at least 1 task");

                if context.lower_bound(&selected_activity.start_variable) + sum_duration_selected
                    > maximum_finish_time
                {
                    return Err(selected_activities[activity_index..]
                        .iter()
                        .flat_map(|task| {
                            [
                                predicate!(
                                    task.start_variable
                                        >= context.lower_bound(&task.start_variable)
                                ),
                                predicate!(
                                    task.start_variable
                                        <= context.upper_bound(&task.start_variable)
                                ),
                            ]
                        })
                        .collect());
                }

                if context.lower_bound(&selected_activity.start_variable) + sum_duration_selected
                    > max_lower_bound
                {
                    updated = true;
                    _index = activity_index;
                    max_lower_bound = context.lower_bound(&selected_activity.start_variable)
                        + sum_duration_selected;
                }
                sum_duration_selected -= selected_activity.processing_time;
            }
            if updated {
                tasks = selected_activities;
            }
        }
        Ok((max_lower_bound, tasks))
    }
}

impl<Var: IntegerVariable + 'static> Propagator for NodePackingPropagator<Var> {
    fn name(&self) -> &str {
        "NodePackingPropagator"
    }

    fn log_statistics(&self, statistic_logger: StatisticLogger) {
        self.statistics.log(statistic_logger)
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        self.statistics.n_calls += 1;

        let result = self.node_packing_bound(context.as_readonly());
        if result.is_err() {
            self.statistics.n_lft_conflicts += 1;
        }

        let (node_packing_bound, tasks) = result?;
        if node_packing_bound > context.lower_bound(&self.makespan_variable) {
            self.statistics.average_size_of_propagation.add_term(
                (node_packing_bound - context.lower_bound(&self.makespan_variable)) as u64,
            );
            self.statistics.n_propagations += 1;
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
                self.statistics.n_conflicts += 1;
                result?;
            }
        }
        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let (node_packing_bound, tasks) = self.node_packing_bound(context.as_readonly())?;

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
