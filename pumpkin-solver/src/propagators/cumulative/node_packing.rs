use std::cmp::max;
use std::collections::VecDeque;
use std::rc::Rc;
use std::usize;

use itertools::Itertools;
use log::info;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::constraints;
use crate::constraints::TrivialCriterion;
use crate::containers::KeyedVec;
use crate::create_statistics_struct;
use crate::engine::propagation::contexts::HasAssignments;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::propagation::ReadDomains;
use crate::engine::DomainEvents;
use crate::engine::EmptyDomain;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::util::create_tasks;
use crate::propagators::ArgTask;
use crate::propagators::Task;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

use rand::prelude::*;

create_statistics_struct!(NodePackingStatistics {
    n_calls: usize,
    n_conflicts: usize,
    n_trivial_conflicts: usize,
    average_clique_size: CumulativeMovingAverage<usize>,
    average_backjump_height: CumulativeMovingAverage<usize>,
    average_nonzero_trivial_choices: CumulativeMovingAverage<usize>,

});

pub(crate) struct NodePackingPropagator<Var> {
    parameters: NodePackingParameters<Var>,
    makespan_variable: Var,
    statistics: NodePackingStatistics,
    trivial_criterion: TrivialCriterion,
}

#[derive(Clone, Debug)]
pub(crate) struct NodePackingParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) disjointness: Vec<Vec<Literal>>,
    pub(crate) mapping: KeyedVec<LocalId, usize>,
}

impl<Var: IntegerVariable + Clone + 'static> NodePackingPropagator<Var> {
    pub(crate) fn new(
        arg_tasks: &[ArgTask<Var>],
        makespan_variable: Var,
        disjointness: Vec<Vec<Literal>>,
        trivial_criterion: TrivialCriterion,
    ) -> Self {
        let (tasks, mapping) = create_tasks(arg_tasks);
        let parameters = NodePackingParameters {
            tasks: tasks
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),

            disjointness,
            mapping,
        };

        NodePackingPropagator {
            parameters: parameters.clone(),
            makespan_variable: makespan_variable.clone(),
            statistics: NodePackingStatistics::default(),
            trivial_criterion,
        }
    }

    fn create_initial_activity_list(&self) -> VecDeque<Rc<Task<Var>>> {
        self.parameters.tasks.to_vec().into()
    }

    fn find_trivial_conflict(
        &mut self,
        context: &mut PropagationContextMut<'_>,
        intervals: &[(i32, i32)],
    ) -> Result<Option<Vec<usize>>, EmptyDomain> {
        let trivial_conflicts = intervals
            .iter()
            .enumerate()
            .map(|(index_lhs, (start_lhs, finish_lhs))| {
                let lhs = &self.parameters.tasks[index_lhs];
                let lhs_conflicts = intervals
                    .iter()
                    .enumerate()
                    .map(|(index_rhs, (start_rhs, finish_rhs))| {
                        if index_rhs == index_lhs {
                            return Ok(None);
                        }
                        let rhs = &self.parameters.tasks[index_rhs];

                        let maybe_conflict = if self.are_disjoint(context, lhs, rhs, intervals)?
                            && lhs.processing_time + rhs.processing_time
                                > finish_rhs.max(finish_lhs) - start_lhs.min(start_rhs)
                        {
                            Some((index_lhs, index_rhs))
                        } else {
                            None
                        };
                        Ok(maybe_conflict)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(lhs_conflicts)
            })
            .collect::<Result<Vec<_>, _>>()?
            .iter()
            .flat_map(|conflict_vec| conflict_vec.iter())
            .filter_map(|&maybe_conflict| maybe_conflict)
            .collect_vec();
        if trivial_conflicts.is_empty() {
            return Ok(None);
        }
        self.statistics
            .average_nonzero_trivial_choices
            .add_term(trivial_conflicts.len());
        let conflict = match self.trivial_criterion {
            TrivialCriterion::First => trivial_conflicts.first(),
            TrivialCriterion::Random => trivial_conflicts.choose(&mut rand::thread_rng()),
        };
        Ok(conflict.map(|(lhs, rhs)| vec![*lhs, *rhs]))
    }
    // {
    //     trivial_conflicts
    //         .iter()
    //         .min_by_key(|(index_lhs, index_rhs)| {
    //             let (start_lhs, finish_lhs) = intervals[*index_lhs];
    //             let (start_rhs, finish_rhs) = intervals[*index_rhs];
    //             let est = start_lhs.min(start_rhs);
    //             let lft = finish_lhs.min(finish_rhs);
    //             let disjoint = self.parameters.disjointness[*index_lhs][*index_rhs];
    //             [*index_lhs, *index_rhs]
    //                 .iter()
    //                 .flat_map(|&task_ix| {
    //                     let task = &self.parameters.tasks[task_ix];
    //                     [
    //                         predicate!(task.start_variable >= est),
    //                         predicate!(task.start_variable <= lft - task.processing_time),
    //                     ]
    //                 })
    //                 .chain(std::iter::once(predicate!(disjoint == 1)))
    //                 .filter_map(|pred| {
    //                     context
    //                         .assignments()
    //                         .get_decision_level_for_predicate(&pred)
    //                 })
    //                 .sorted()
    //                 .dedup()
    //                 .count()

    //             // let est_gap_lhs = start_lhs - start;
    //             // let est_gap_rhs = start_lhs - start;
    //             // let lft_gap_lhs = finish - finish_lhs;
    //             // let lft_gap_rhs = finish - finish_rhs;
    //             // (1 + est_gap_lhs + lft_gap_lhs) * (1 + est_gap_rhs + lft_gap_rhs)
    //         })
    // }

    fn find_conflict(
        &mut self,
        context: &mut PropagationContextMut<'_>,
    ) -> Result<Option<Vec<usize>>, EmptyDomain> {
        let intervals = self
            .parameters
            .tasks
            .iter()
            .map(|task| {
                (
                    context.lower_bound(&task.start_variable),
                    context.upper_bound(&task.start_variable) + task.processing_time,
                )
            })
            .collect_vec();
        // Try finding a conflicting *pair* of tasks
        if let Some(conflict) = self.find_trivial_conflict(context, &intervals)? {
            return Ok(Some(conflict));
        }
        // Run a greedy heuristic from all intervals
        for (seed_index, (mut start, mut finish)) in intervals.iter().enumerate() {
            let mut clique = vec![seed_index];
            let mut remaining = (0..self.parameters.tasks.len())
                .filter(|&ix| ix != seed_index)
                .collect_vec();
            let mut last_selected = &self.parameters.tasks[seed_index];
            loop {
                // Keep the intervals that not in the clique and can be added to a clique
                //
                // Note that we only need to check whether it is compatible with the currently
                // selected index to be added to the clique since it is necessarily incompatible
                // with all the elements that came before it
                remaining.retain(|&remaining_ix| {
                    !clique.contains(&remaining_ix)
                        && self
                            .are_disjoint(
                                context,
                                last_selected,
                                &self.parameters.tasks[remaining_ix],
                                &intervals,
                            )
                            .expect("Should not be an error here")
                });
                // Choose the interval that is not disconnected from [start, finish)
                // and minimizes the length added to the interval, breaking ties
                // in favor of intervals with longer durations.
                let next_ix = remaining
                    .iter()
                    .filter(|&&remaining_ix| {
                        let (rem_start, rem_finish) = intervals[remaining_ix];
                        if rem_start > finish || rem_finish < start {
                            return false;
                        }
                        true
                    })
                    .min_by_key(|&&remaining_ix| {
                        let new_length = finish.max(intervals[remaining_ix].1)
                            - start.min(intervals[remaining_ix].0);
                        let new_duration = self.parameters.tasks[remaining_ix].processing_time;
                        (
                            new_length,
                            -new_duration,
                            -self.parameters.tasks[remaining_ix].resource_usage,
                        )
                    });
                if let Some(&next_ix) = next_ix {
                    clique.push(next_ix);
                    start = start.min(intervals[next_ix].0);
                    finish = finish.max(intervals[next_ix].1);
                    last_selected = &self.parameters.tasks[next_ix];
                } else {
                    break;
                }
            }
            // Update the best clique if the overflow condition holds
            if clique
                .iter()
                .map(|&ix| self.parameters.tasks[ix].processing_time)
                .sum::<i32>()
                > finish - start
            {
                return Ok(Some(clique));
            }
        }
        Ok(None)
    }

    fn are_overlapping(interval_1: (i32, i32), interval_2: (i32, i32)) -> bool {
        interval_1.0 <= interval_2.1 && interval_2.0 <= interval_1.1
    }

    fn are_disjoint(
        &self,
        context: &mut PropagationContextMut<'_>,
        task_lhs: &Rc<Task<Var>>,
        task_rhs: &Rc<Task<Var>>,
        intervals: &[(i32, i32)],
    ) -> Result<bool, EmptyDomain> {
        let is_literal_true = context.is_literal_true(
            &self.parameters.disjointness[self.parameters.mapping[task_lhs.id]]
                [self.parameters.mapping[task_rhs.id]],
        );
        if !is_literal_true
            && !Self::are_overlapping(
                intervals[task_lhs.id.unpack() as usize],
                intervals[task_rhs.id.unpack() as usize],
            )
        {
            context.assign_literal(
                &self.parameters.disjointness[self.parameters.mapping[task_lhs.id]]
                    [self.parameters.mapping[task_rhs.id]],
                true,
                conjunction!(
                    [task_lhs.start_variable >= intervals[task_lhs.id.unpack() as usize].0]
                        & [task_lhs.start_variable
                            <= intervals[task_lhs.id.unpack() as usize].1
                                - task_lhs.processing_time]
                        & [task_rhs.start_variable >= intervals[task_rhs.id.unpack() as usize].0]
                        & [task_rhs.start_variable
                            <= intervals[task_rhs.id.unpack() as usize].1
                                - task_rhs.processing_time]
                ),
            )?;
        }
        Ok(is_literal_true)
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
        if let Some(clique) = self.find_conflict(&mut context)? {
            self.statistics.n_conflicts += 1;
            if clique.len() == 2 {
                self.statistics.n_trivial_conflicts += 1;
            }
            self.statistics.average_clique_size.add_term(clique.len());
            let tasks = self.create_initial_activity_list();
            let est = clique
                .iter()
                .map(|&task_ix| context.lower_bound(&tasks[task_ix].start_variable))
                .min()
                .expect("Empty clique");
            let lft = clique
                .iter()
                .map(|&task_ix| {
                    let task = &tasks[task_ix];
                    context.upper_bound(&task.start_variable) + task.processing_time
                })
                .max()
                .expect("Empty clique");
            let nogood = clique
                .iter()
                .flat_map(|&task_ix| {
                    let task = &tasks[task_ix];
                    [
                        predicate!(task.start_variable >= est),
                        predicate!(task.start_variable <= lft - task.processing_time),
                    ]
                })
                .chain(clique.iter().tuple_combinations::<(_, _)>().map(
                    |(&task_lhs_ix, &task_rhs_ix)| {
                        let disjoint = self.parameters.disjointness[task_lhs_ix][task_rhs_ix];
                        predicate!(disjoint == 1)
                    },
                ))
                .collect_vec();
            info!("Found cluster with explanation: {nogood:?}");
            Err(crate::basic_types::Inconsistency::Conflict(
                nogood.into_iter().collect(),
            ))
        } else {
            Ok(())
        }
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        todo!();
        if let Some(clique) = self.find_conflict(&mut context)? {
            let tasks = self.create_initial_activity_list();
            let est = clique
                .iter()
                .map(|&task_ix| context.lower_bound(&tasks[task_ix].start_variable))
                .min()
                .expect("Empty clique");
            let lft = clique
                .iter()
                .map(|&task_ix| {
                    let task = &tasks[task_ix];
                    context.upper_bound(&task.start_variable) + task.processing_time
                })
                .max()
                .expect("Empty clique");
            Err(crate::basic_types::Inconsistency::Conflict(
                clique
                    .iter()
                    .flat_map(|&task_ix| {
                        let task = &tasks[task_ix];
                        [
                            predicate!(task.start_variable >= est),
                            predicate!(task.start_variable <= lft - task.processing_time),
                        ]
                    })
                    .chain(clique.iter().tuple_combinations::<(_, _)>().map(
                        |(&task_lhs_ix, &task_rhs_ix)| {
                            let disjoint = self.parameters.disjointness[task_lhs_ix][task_rhs_ix];
                            predicate!(disjoint == 1)
                        },
                    ))
                    .collect(),
            ))
        } else {
            Ok(())
        }
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
