use std::cmp::min;
use std::collections::VecDeque;
use std::rc::Rc;
use std::usize;

use itertools::Itertools;

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

    fn remove_compatible(
        &self,
        context: PropagationContext,
        activities: &mut VecDeque<Rc<Task<Var>>>,
        selected_activity: Rc<Task<Var>>,
    ) {
        activities.retain(|activity| {
            // `activity` and `selected_activity` must either be statically incompatible
            // or have disjoint time spans for execution
            self.parameters.static_incompatibilities[selected_activity.id.unpack() as usize]
                [activity.id.unpack() as usize]
                || !(context.lower_bound(&activity.start_variable)
                    <= context.upper_bound(&selected_activity.start_variable)
                        + selected_activity.processing_time
                    && context.lower_bound(&selected_activity.start_variable)
                        <= context.upper_bound(&activity.start_variable) + activity.processing_time)
        })
    }

    fn cover_size(intervals: &mut [(i32, i32)]) -> i64 {
        let mut cover = 0;
        intervals.sort();
        let mut lo = 0;
        let mut hi = 0;
        for (a, b) in intervals {
            assert!(*a >= 0);
            assert!(*b >= 0);
            if *a > hi {
                cover += hi as i64;
                cover -= lo as i64;
                lo = *a;
            }
            if *b > hi {
                hi = *b;
            }
        }
        cover += hi as i64;
        cover -= lo as i64;
        cover
    }

    fn score_violation(
        durations: &[i32],
        intervals: &[(i32, i32)],
        clique: &[usize]
    ) -> i64 {
        Self::cover_size(&mut clique.iter().map(|&ix| intervals[ix]).collect_vec()) -
            clique.iter().map(|&ix| durations[ix] as i64).sum::<i64>() + 1
    }

    fn score(
        clique: &[usize],
        durations: &[i32],
        intervals: &[(i32, i32)],
        lambda: f64
    ) -> f64 {
        clique.iter().map(|&ix| durations[ix] as f64 - 1.0 / lambda).sum::<f64>() - 
            Self::cover_size(&mut clique.iter().map(|&ix| intervals[ix]).collect_vec()) as f64
    }

    fn is_clique(&self, clique: &[usize], intervals: &[(i32, i32)]) -> bool {
        if !clique.is_empty() {
            for &a in clique {
                for &b in clique {
                    if a == b { continue; }
                    if self.parameters.static_incompatibilities[a][b] { continue; }
                    if !(intervals[a].0 <= intervals[b].1 && intervals[b].0 <= intervals[a].1) { continue; }
                    return false;
                }
            }
            true
        } else {
            // HACK Empty clique *is* a clique but the LS seems to fall into it too hastily
            false
        }
    }

    fn accept(
        &self,
        new_clique: &[usize],
        ref_clique: &[usize],
        durations: &[i32],
        intervals: &[(i32, i32)],
        lambda: f64
    ) -> bool {
        self.is_clique(new_clique, intervals) && (
            Self::score(new_clique, durations, intervals, lambda) >
            Self::score(ref_clique, durations, intervals, lambda)
        )
    }

    fn find_conflict(
        &self,
        context: PropagationContext<'_>,
    ) -> Option<Vec<Rc<Task<Var>>>> {
        let all_tasks = self.create_initial_activity_list();
        let durations = all_tasks.iter().map(|task| task.processing_time).collect_vec();
        let intervals = all_tasks.iter().map(|task| (
            context.lower_bound(&task.start_variable),
            context.upper_bound(&task.start_variable) + task.processing_time,
        )).collect_vec();
        let mut curr_lambda = 0.1 * durations.iter().map(|&d| 1.0 / d as f64).reduce(f64::max).unwrap_or(0.0);
        let mut curr_clique = vec![
            (0..all_tasks.len())
                .max_by_key(|&task_ix| durations[task_ix] + intervals[task_ix].1 - intervals[task_ix].0)
                .expect("Empty task list")
        ];
        let mut best_clique: Option<Vec<usize>> = None;
        for ls_iter in 1..=100 {
            // Enumerate all candidates for extending the clique
            'ls: for add_vertex in 0..all_tasks.len() {
                if curr_clique.contains(&add_vertex) {
                    continue;
                }
                let mut add_clique = curr_clique.clone();
                add_clique.push(add_vertex);
                if self.accept(&add_clique, &curr_clique, &durations, &intervals, curr_lambda) {
                    eprintln!("{ls_iter} {curr_clique:?} ADD {add_vertex}");
                    curr_clique = add_clique;
                    break;
                }
                for rem_vertex in 0..all_tasks.len() {
                    if !curr_clique.contains(&rem_vertex) {
                        continue;
                    }
                    let mut rem_clique = curr_clique.clone();
                    rem_clique.retain(|&x| x != rem_vertex);
                    if self.accept(&rem_clique, &curr_clique, &durations, &intervals, curr_lambda) {
                        eprintln!("{ls_iter} {curr_clique:?} REM {rem_vertex}");
                        curr_clique = rem_clique;
                        break 'ls; 
                    }
                    let mut swap_clique = rem_clique.clone();
                    swap_clique.push(add_vertex);
                    if self.accept(&swap_clique, &curr_clique, &durations, &intervals, curr_lambda) {
                        eprintln!("{ls_iter} {curr_clique:?} SWAP {add_vertex} {rem_vertex}");
                        curr_clique = swap_clique;
                        break 'ls; 
                    }
                }
            }
            // Update the Lagrangian multiplier
            let violation = Self::score_violation(&durations, &intervals, &curr_clique);
            curr_lambda += 0.1 * (violation as f64);
            eprintln!("{ls_iter} {curr_clique:?} {curr_lambda} {violation}");
            if curr_lambda < 0.0 { curr_lambda = 0.0; }
            // If possible, store a better valid clique
            if violation <= 0 && curr_clique.len() < best_clique.as_ref().map(|x| x.len()).unwrap_or(usize::MAX) {
                best_clique = Some(curr_clique.clone());
            }
        }
        best_clique.map(|indices: Vec<usize>| indices.iter().map(|&ix| all_tasks[ix].clone()).collect())
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
            self.statistics.average_clique_size.add_term(clique.len() as u64);
            let est = clique.iter().map(|task| context.lower_bound(&task.start_variable)).min().expect("Empty clique");
            let lft = clique.iter().map(|task| context.upper_bound(&task.start_variable) + task.processing_time).max().expect("Empty clique");
            eprintln!("Tasks {clique:?} do not fit in [{est},{lft}]");
            Err(
                crate::basic_types::Inconsistency::Conflict(
                    clique
                        .iter()
                        .flat_map(|task| {
                            [
                                predicate!(task.start_variable >= est),
                                predicate!(task.start_variable <= lft - task.processing_time),
                            ]
                        })
                        .collect()
                )
            )
        } else {
            Ok(())
        }
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        if let Some(clique) = self.find_conflict(context.as_readonly()) {
            let est = clique.iter().map(|task| context.lower_bound(&task.start_variable)).min().expect("Empty clique");
            let lst = clique.iter().map(|task| context.upper_bound(&task.start_variable)).max().expect("Empty clique");
            Err(
                crate::basic_types::Inconsistency::Conflict(
                    clique
                        .iter()
                        .flat_map(|task| {
                            [
                                predicate!(task.start_variable >= est),
                                predicate!(task.start_variable <= lst),
                            ]
                        })
                        .collect()
                )
            )
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
