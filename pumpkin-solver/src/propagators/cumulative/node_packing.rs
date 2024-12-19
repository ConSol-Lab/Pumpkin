use core::f64;
use std::collections::VecDeque;
use std::rc::Rc;

use itertools::Itertools;
use russcip::Model;
use russcip::ObjSense;
use russcip::ProblemOrSolving;
use russcip::WithSolutions;

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
    n_conflicts: usize,
    average_clique_size: CumulativeMovingAverage,
});

pub(crate) struct NodePackingPropagator<Var> {
    parameters: NodePackingParameters<Var>,
    makespan_variable: Var,
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
            statistics: NodePackingStatistics::default(),
        }
    }

    fn create_initial_activity_list(&self) -> VecDeque<Rc<Task<Var>>> {
        self.parameters.tasks.to_vec().into()
    }

    fn find_conflict(&self, context: PropagationContext<'_>) -> Option<Vec<Rc<Task<Var>>>> {
        let all_tasks = self.create_initial_activity_list();
        let durations = all_tasks
            .iter()
            .map(|task| task.processing_time)
            .collect_vec();
        let intervals = all_tasks
            .iter()
            .map(|task| {
                (
                    context.lower_bound(&task.start_variable),
                    context.upper_bound(&task.start_variable) + task.processing_time,
                )
            })
            .collect_vec();

        //// Try finding a conflicting *pair* of tasks
        for (index_lhs, (duration_lhs, (start_lhs, finish_lhs))) in
            durations.iter().zip(&intervals).enumerate()
        {
            for (index_rhs, (duration_rhs, (start_rhs, finish_rhs))) in
                durations.iter().take(index_lhs).zip(&intervals).enumerate()
            {
                if self.parameters.static_incompatibilities[index_lhs][index_rhs]
                    && duration_lhs + duration_rhs
                        > finish_rhs.max(finish_lhs) - start_lhs.min(start_rhs)
                {
                    return Some(
                        [index_lhs, index_rhs]
                            .iter()
                            .map(|&ix| all_tasks[ix].clone())
                            .collect(),
                    );
                }
            }
        }
        // Split the timeline by the start and finish points from all intervals
        let mut time_points = intervals.iter().flat_map(|x| [x.0, x.1]).collect_vec();
        time_points.sort();
        time_points.dedup();
        // Construct a MIP model for finding an conflict-generating clique
        // of smallest cardinality
        let mut model = Model::new()
            .hide_output()
            .include_default_plugins()
            .create_prob("conflict_discovery")
            .set_obj_sense(ObjSense::Minimize);
        // Binary variables encoding interval selection
        let interval_vars = durations
            .iter()
            .enumerate()
            .map(|(ix, _)| {
                model.add_var(
                    0.,
                    1.,
                    1.,
                    format!("x{ix}").as_str(),
                    russcip::VarType::Binary,
                )
            })
            .collect_vec();
        // Binary variables encoding time segment selection
        let time_segments = time_points
            .iter()
            .skip(1)
            .zip(time_points.iter())
            .map(|(&finish, &start)| (start, finish))
            .collect_vec();
        let time_segment_vars = time_segments
            .iter()
            .enumerate()
            .map(|(ix, _)| {
                model.add_var(
                    0.,
                    1.,
                    0.,
                    format!("t{ix}").as_str(),
                    russcip::VarType::Binary,
                )
            })
            .collect_vec();
        // Duration bound
        model.add_cons(
            interval_vars
                .iter()
                .chain(time_segment_vars.iter())
                .cloned()
                .collect_vec(),
            &durations
                .iter()
                .cloned()
                .chain(time_segments.iter().map(|(start, finish)| start - finish))
                .map(|x| x as f64)
                .collect_vec(),
            1.0,
            f64::INFINITY,
            "duration",
        );
        // Interval choice implies choosing all contained time spand
        for ((int_lhs, int_rhs), int_var) in intervals.iter().zip(interval_vars.iter()) {
            for ((ts_lhs, ts_rhs), ts_var) in time_segments.iter().zip(time_segment_vars.iter()) {
                if int_lhs <= ts_lhs && ts_rhs <= int_rhs {
                    model.add_cons(
                        vec![int_var.clone(), ts_var.clone()],
                        &[-1.0, 1.0],
                        0.,
                        f64::INFINITY,
                        format!("contain_impl_{}_{}_{}_{}", int_lhs, int_rhs, ts_lhs, ts_rhs)
                            .as_str(),
                    );
                }
            }
        }
        // Clique constraints
        for (index_lhs, lhs_var) in interval_vars.iter().enumerate() {
            for (index_rhs, rhs_var) in interval_vars.iter().enumerate() {
                if index_lhs == index_rhs {
                    continue;
                }
                if !self.parameters.static_incompatibilities[index_lhs][index_rhs] {
                    // Forbid choosing both intervals
                    model.add_cons(
                        vec![lhs_var.clone(), rhs_var.clone()],
                        &[1.0, 1.0],
                        0.,
                        1.,
                        format!("clique_{}_{}", index_lhs, index_rhs).as_str(),
                    );
                }
            }
        }
        let solved_model = model.solve();
        match solved_model.status() {
            russcip::Status::Optimal => {
                // An optimal clique is discovered, store it
                let sol = solved_model.best_sol().unwrap();
                println!("{}", sol.obj_val());
                let collect = all_tasks
                    .iter()
                    .enumerate()
                    .filter_map(|(ix, task)| {
                        if sol.val(interval_vars[ix].clone()) > 1e-3 {
                            Some(task.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                let info = all_tasks
                    .iter()
                    .enumerate()
                    .filter_map(|(ix, task)| {
                        if sol.val(interval_vars[ix].clone()) > 1e-3 {
                            Some((ix, durations[ix], intervals[ix]))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                println!("{info:?}");
                if info.len() == 2 {
                    let incompatible =
                        self.parameters.static_incompatibilities[info[0].0][info[1].0];
                    println!("{incompatible}");
                }

                Some(collect)
            }
            russcip::Status::Infeasible => {
                // No conflict exists, keep going
                None
            }
            _ => {
                // Miscellaneous reason, stay away
                None
            }
        }
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
        if let Some(clique) = self.find_conflict(context.as_readonly()) {
            self.statistics.n_conflicts += 1;
            self.statistics
                .average_clique_size
                .add_term(clique.len() as u64);
            let est = clique
                .iter()
                .map(|task| context.lower_bound(&task.start_variable))
                .min()
                .expect("Empty clique");
            let lft = clique
                .iter()
                .map(|task| context.upper_bound(&task.start_variable) + task.processing_time)
                .max()
                .expect("Empty clique");
            // eprintln!("Tasks {clique:?} do not fit in [{est},{lft}]");
            Err(crate::basic_types::Inconsistency::Conflict(
                clique
                    .iter()
                    .flat_map(|task| {
                        [
                            predicate!(task.start_variable >= est),
                            predicate!(task.start_variable <= lft - task.processing_time),
                        ]
                    })
                    .collect(),
            ))
        } else {
            Ok(())
        }
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        if let Some(clique) = self.find_conflict(context.as_readonly()) {
            let est = clique
                .iter()
                .map(|task| context.lower_bound(&task.start_variable))
                .min()
                .expect("Empty clique");
            let lst = clique
                .iter()
                .map(|task| context.upper_bound(&task.start_variable))
                .max()
                .expect("Empty clique");
            Err(crate::basic_types::Inconsistency::Conflict(
                clique
                    .iter()
                    .flat_map(|task| {
                        [
                            predicate!(task.start_variable >= est),
                            predicate!(task.start_variable <= lst),
                        ]
                    })
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
