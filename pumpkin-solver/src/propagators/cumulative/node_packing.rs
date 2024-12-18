use core::f64;
use std::cmp::min;
use std::collections::VecDeque;
use std::rc::Rc;
use std::usize;

use itertools::Itertools;
use russcip::Model;
use russcip::ObjSense;
use russcip::ProblemOrSolving;
use russcip::WithSolutions;
use russcip::WithSolvingStats;

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
        // Try finding a conflicting *pair* of tasks
        for (index_lhs, (duration_lhs, (start_lhs, finish_lhs))) in
            durations.iter().zip(&intervals).enumerate()
        {
            for (index_rhs, (duration_rhs, (start_rhs, finish_rhs))) in
                durations.iter().take(index_lhs).zip(&intervals).enumerate()
            {
                if self.parameters.static_incompatibilities[index_lhs][index_rhs]
                    && duration_lhs + duration_rhs
                        > finish_rhs.max(finish_lhs) - start_rhs.min(start_lhs)
                {
                    // eprintln!("Trivial clique {index_lhs} {index_rhs}");
                    return Some(
                        [index_lhs, index_rhs]
                            .iter()
                            .map(|&ix| all_tasks[ix].clone())
                            .collect(),
                    );
                }
            }
        }
        return None;
        // Decompose the problem into the one of finding the conflict that fits into
        // an interval [x..y), with x and y being the start/finish points for some of the intervals
        let mut time_points = intervals.iter().flat_map(|x| [x.0, x.1]).collect_vec();
        time_points.sort();
        time_points.dedup();
        let mut best_clique: Option<Vec<usize>> = None;
        let mut candidate_intervals = (0..time_points.len())
            .flat_map(|rhs_index| {
                let rhs = time_points[rhs_index];
                let all_tasks = all_tasks.clone();
                let intervals = intervals.clone();
                let durations = durations.clone();
                time_points[0..rhs_index].iter().filter_map(move |&lhs| {
                    let total_duration = (0..all_tasks.len())
                        .filter(|&ix| lhs <= intervals[ix].0 && intervals[ix].1 <= rhs)
                        .map(|ix| durations[ix])
                        .sum::<i32>();
                    if total_duration > rhs - lhs {
                        Some((lhs, rhs))
                    } else {
                        None
                    }
                })
            })
            .collect_vec();
        candidate_intervals.sort_by_key(|(lhs, rhs)| {
            -(0..all_tasks.len())
                .filter(|&ix| *lhs <= intervals[ix].0 && intervals[ix].1 <= *rhs)
                .map(|ix| durations[ix])
                .sum::<i32>()
        });
        for (lhs, rhs) in candidate_intervals.iter().take(5).cloned() {
            // for rhs_index in 0..time_points.len() {
            //     let rhs = time_points[rhs_index];
            //     for &lhs in time_points[0..rhs_index].iter() {
            // Filter the fitting intervals and construct the clique-finding MIP
            let indices = (0..all_tasks.len())
                .filter(|&ix| lhs <= intervals[ix].0 && intervals[ix].1 <= rhs)
                .collect_vec();
            let mut model = Model::new()
                .hide_output()
                .include_default_plugins()
                .create_prob("conflict_discovery")
                .set_obj_sense(ObjSense::Minimize);
            // Binary variables encoding interval selection
            let vars = indices
                .iter()
                .map(|ix| {
                    model.add_var(
                        0.,
                        1.,
                        1.,
                        format!("x{ix}").as_str(),
                        russcip::VarType::Binary,
                    )
                })
                .collect_vec();
            // Duration bound
            model.add_cons(
                vars.clone(),
                &indices.iter().map(|&ix| durations[ix] as f64).collect_vec(),
                (rhs - lhs + 1) as f64,
                f64::INFINITY,
                "duration",
            );
            // Clique constraints
            for (j, &b) in indices.iter().enumerate() {
                for (i, &a) in indices[0..j].iter().enumerate() {
                    if self.parameters.static_incompatibilities[a][b] {
                        // Static incompatible tasks, do not constrain
                    }
                    // else if intervals[a].0.max(intervals[b].0)
                    //     >= intervals[a].1.min(intervals[b].1)
                    // {
                    //     // Disjoint intervals, do not constrain
                    // }
                    else {
                        // Forbid choosing both intervals
                        model.add_cons(
                            vec![vars[i].clone(), vars[j].clone()],
                            &[1.0, 1.0],
                            0.,
                            1.,
                            format!("clique_{}_{}", a, b).as_str(),
                        );
                    }
                }
            }
            let solved_model = model.solve();
            let mut new_clique = vec![];
            match solved_model.status() {
                russcip::Status::Optimal => {
                    // An optimal clique is discovered, store it
                    let sol = solved_model.best_sol().unwrap();
                    for (i, var) in vars.iter().enumerate() {
                        if sol.val(var.clone()) > 1e-3 {
                            new_clique.push(indices[i]);
                        }
                    }
                    // eprintln!("{lhs}..{rhs} {new_clique:?}");
                    if new_clique.len()
                        < best_clique.as_ref().map(|x| x.len()).unwrap_or(usize::MAX)
                    {
                        best_clique = Some(new_clique);
                    }
                }
                russcip::Status::Infeasible => {
                    // No conflict exists, keep going
                }
                _ => {
                    // Miscellaneous reason, stay away
                }
            }
        }
        if let Some(indices) = best_clique.as_ref() {
            eprintln!("The best clique is {indices:?}");
        };
        best_clique
            .map(|indices: Vec<usize>| indices.iter().map(|&ix| all_tasks[ix].clone()).collect())
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
