use std::rc::Rc;

use itertools::Itertools;
use log::info;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::containers::KeyedVec;
use crate::create_statistics_struct;
use crate::engine::propagation::LocalId;
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
use crate::pumpkin_assert_simple;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

create_statistics_struct!(NodePackingStatistics {
    n_calls: usize,
    n_conflicts: usize,
    n_trivial_conflicts: usize,
    average_clique_size: CumulativeMovingAverage<usize>,
    average_backjump_height: CumulativeMovingAverage<usize>,

});

pub(crate) struct NodePackingPropagator<Var> {
    parameters: NodePackingParameters<Var>,
    _makespan_variable: Var,
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
    pub(crate) disjointness: Vec<Vec<Literal>>,
    pub(crate) mapping: KeyedVec<LocalId, usize>,
}

impl<Var: IntegerVariable + Clone + 'static> NodePackingPropagator<Var> {
    pub(crate) fn new(
        arg_tasks: &[ArgTask<Var>],
        makespan_variable: Var,
        disjointness: Vec<Vec<Literal>>,
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
            _makespan_variable: makespan_variable.clone(),
            statistics: NodePackingStatistics::default(),
        }
    }

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
        for (index_lhs, (start_lhs, finish_lhs)) in intervals.iter().enumerate() {
            let lhs = &self.parameters.tasks[index_lhs];
            for (index_rhs, (start_rhs, finish_rhs)) in intervals.iter().enumerate() {
                if index_rhs == index_lhs {
                    continue;
                }
                let rhs = &self.parameters.tasks[index_rhs];

                if self.are_disjoint(context, lhs, rhs, &intervals)?
                    && lhs.processing_time + rhs.processing_time
                        > finish_rhs.max(finish_lhs) - start_lhs.min(start_rhs)
                {
                    return Ok(Some([index_lhs, index_rhs].to_vec()));
                }
            }
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
        // TODO Commenting out the MIP model construction so far; to be revisited
        // // Split the timeline by the start and finish points from all intervals
        // let mut time_points = intervals.iter().flat_map(|x| [x.0, x.1]).collect_vec();
        // time_points.sort();
        // time_points.dedup();
        // // Construct a MIP model for finding an conflict-generating clique
        // // of smallest cardinality
        // let mut model = Model::new()
        //     .hide_output()
        //     .include_default_plugins()
        //     .create_prob("conflict_discovery")
        //     .set_obj_sense(ObjSense::Minimize);
        // // Binary variables encoding interval selection
        // let interval_vars = durations
        //     .iter()
        //     .enumerate()
        //     .map(|(ix, _)| {
        //         model.add_var(
        //             0.,
        //             1.,
        //             1.,
        //             format!("x{ix}").as_str(),
        //             russcip::VarType::Binary,
        //         )
        //     })
        //     .collect_vec();
        // // Binary variables encoding time segment selection
        // let time_segments = time_points
        //     .iter()
        //     .skip(1)
        //     .zip(time_points.iter())
        //     .map(|(&finish, &start)| (start, finish))
        //     .collect_vec();
        // let time_segment_vars = time_segments
        //     .iter()
        //     .enumerate()
        //     .map(|(ix, _)| {
        //         model.add_var(
        //             0.,
        //             1.,
        //             0.,
        //             format!("t{ix}").as_str(),
        //             russcip::VarType::Binary,
        //         )
        //     })
        //     .collect_vec();
        // // Duration bound
        // model.add_cons(
        //     interval_vars
        //         .iter()
        //         .chain(time_segment_vars.iter())
        //         .cloned()
        //         .collect_vec(),
        //     &durations
        //         .iter()
        //         .cloned()
        //         .chain(time_segments.iter().map(|(start, finish)| start - finish))
        //         .map(|x| x as f64)
        //         .collect_vec(),
        //     1.0,
        //     f64::INFINITY,
        //     "duration",
        // );
        // // Interval choice implies choosing all contained time spand
        // for ((int_lhs, int_rhs), int_var) in intervals.iter().zip(interval_vars.iter()) {
        //     for ((ts_lhs, ts_rhs), ts_var) in time_segments.iter().zip(time_segment_vars.iter())
        // {         if int_lhs <= ts_lhs && ts_rhs <= int_rhs {
        //             model.add_cons(
        //                 vec![int_var.clone(), ts_var.clone()],
        //                 &[-1.0, 1.0],
        //                 0.,
        //                 f64::INFINITY,
        //                 format!("contain_impl_{}_{}_{}_{}", int_lhs, int_rhs, ts_lhs, ts_rhs)
        //                     .as_str(),
        //             );
        //         }
        //     }
        // }
        // // Clique constraints
        // for (index_lhs, lhs_var) in interval_vars.iter().enumerate() {
        //     for (index_rhs, rhs_var) in interval_vars.iter().enumerate() {
        //         if index_lhs == index_rhs {
        //             continue;
        //         }
        //         if !self.parameters.static_incompatibilities[index_lhs][index_rhs] {
        //             // Forbid choosing both intervals
        //             model.add_cons(
        //                 vec![lhs_var.clone(), rhs_var.clone()],
        //                 &[1.0, 1.0],
        //                 0.,
        //                 1.,
        //                 format!("clique_{}_{}", index_lhs, index_rhs).as_str(),
        //             );
        //         }
        //     }
        // }
        // let solved_model = model.solve();
        // match solved_model.status() {
        //     russcip::Status::Optimal => {
        //         // An optimal clique is discovered, store it
        //         let sol = solved_model.best_sol().unwrap();
        //         println!("{}", sol.obj_val());
        //         let collect = all_tasks
        //             .iter()
        //             .enumerate()
        //             .filter_map(|(ix, task)| {
        //                 if sol.val(interval_vars[ix].clone()) > 1e-3 {
        //                     Some(task.clone())
        //                 } else {
        //                     None
        //                 }
        //             })
        //             .collect();
        //         let info = all_tasks
        //             .iter()
        //             .enumerate()
        //             .filter_map(|(ix, task)| {
        //                 if sol.val(interval_vars[ix].clone()) > 1e-3 {
        //                     Some((ix, durations[ix], intervals[ix]))
        //                 } else {
        //                     None
        //                 }
        //             })
        //             .collect::<Vec<_>>();
        //         println!("{info:?}");
        //         if info.len() == 2 {
        //             let incompatible =
        //                 self.parameters.static_incompatibilities[info[0].0][info[1].0];
        //             println!("{incompatible}");
        //         }

        //         Some(collect)
        //     }
        //     russcip::Status::Infeasible => {
        //         // No conflict exists, keep going
        //         None
        //     }
        //     _ => {
        //         // Miscellaneous reason, stay away
        //         None
        //     }
        // }
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

            let (est, lft, sum_of_durations) =
                clique
                    .iter()
                    .fold((i32::MAX, 0, 0), |(est, lft, sum_of_durations), task_ix| {
                        let task = &self.parameters.tasks[*task_ix];
                        (
                            est.min(context.lower_bound(&task.start_variable)),
                            lft.max(
                                context.upper_bound(&task.start_variable) + task.processing_time,
                            ),
                            sum_of_durations + task.processing_time,
                        )
                    });

            pumpkin_assert_simple!(lft - est < sum_of_durations);

            // Based on "Computing Explanations for the Unary Resource Constraint - Petr Vilim"
            let delta = sum_of_durations - (lft - est) - 1;
            let nogood = clique
                .iter()
                .flat_map(|&task_ix| {
                    let task = &self.parameters.tasks[task_ix];
                    [
                        predicate!(
                            task.start_variable >= est - (delta as f64 / 2.0).floor() as i32
                        ),
                        predicate!(
                            task.start_variable
                                <= lft + (delta as f64 / 2.0).ceil() as i32 - task.processing_time
                        ),
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

    fn debug_propagate_from_scratch(&self, _context: PropagationContextMut) -> PropagationStatusCP {
        todo!();
        // if let Some(clique) = self.find_conflict(&mut context)? {
        //    let tasks = self.create_initial_activity_list();
        //    let est = clique
        //        .iter()
        //        .map(|&task_ix| context.lower_bound(&tasks[task_ix].start_variable))
        //        .min()
        //        .expect("Empty clique");
        //    let lft = clique
        //        .iter()
        //        .map(|&task_ix| {
        //            let task = &tasks[task_ix];
        //            context.upper_bound(&task.start_variable) + task.processing_time
        //        })
        //        .max()
        //        .expect("Empty clique");
        //    Err(crate::basic_types::Inconsistency::Conflict(
        //        clique
        //            .iter()
        //            .flat_map(|&task_ix| {
        //                let task = &tasks[task_ix];
        //                [
        //                    predicate!(task.start_variable >= est),
        //                    predicate!(task.start_variable <= lft - task.processing_time),
        //                ]
        //            })
        //            .chain(clique.iter().tuple_combinations::<(_, _)>().map(
        //                |(&task_lhs_ix, &task_rhs_ix)| {
        //                    let disjoint = self.parameters.disjointness[task_lhs_ix][task_rhs_ix];
        //                    predicate!(disjoint == 1)
        //                },
        //            ))
        //            .collect(),
        //    ))
        //} else {
        //    Ok(())
        //}
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
