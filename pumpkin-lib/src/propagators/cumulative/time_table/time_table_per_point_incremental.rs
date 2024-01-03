use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::propagators::PerPointIteratorType;
use crate::propagators::PerPointTimeTableType;
use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, EnqueueDecision,
        PropagationContext, PropagatorConstructorContext,
    },
    propagators::{
        cumulative::time_table::time_table_propagator::{check_for_updates, generate_update_range},
        CumulativeArgs, CumulativeParameters, PropagationStatusWithExplanation, Task, Updated,
        Util,
    },
    pumpkin_assert_extreme,
};
use crate::{propagators::TimeTablePerPointProp, pumpkin_assert_advanced};

use super::time_table_propagator::{should_enqueue, ResourceProfile, TimeTablePropagator};

/// Propagator responsible for using time-table reasoning to propagate the [Cumulative] constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at different time-points -
/// This method creates a [ResourceProfile] per time point rather than creating one over an interval (hence the name).
/// Additionally, it avoids recalculating the time-table from scratch every time by making use of incrementality
///
/// See ["Improving Scheduling by Learning - Andreas Schutt (2011)"](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
/// Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 for more information about time-table reasoning.
pub struct TimeTablePerPointIncrementalProp<Var> {
    /// The key t (representing a time-point) holds the mandatory resource consumption of tasks at that time (stored in a [ResourceProfile]);
    /// the [ResourceProfile]s are sorted based on start time and they are assumed to be non-overlapping
    time_table: PerPointTimeTableType<Var>,
    /// For each variable, eagerly maps the explanation of the lower-bound change;
    /// For a task i (representing a map in the [Vec]), \[bound\] stores the explanation for \[s_i >= bound\]
    reasons_for_propagation_lower_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// For each variable, eagerly maps the explanation of the upper-bound change
    /// For a task i (representing a map in the [Vec]), \[bound\] stores the explanation for \[s_i <= bound\]
    reasons_for_propagation_upper_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
    /// Keeps track of whether the current state of the time-table is outdated (i.e. whether it needs to be recalculated from scratch)
    ///
    /// Imagine the situation where a synchronisation takes place and the propagator eagerly recalculates the time-table
    /// but then another propagator finds a conflict and the time-table calculation was superfluous;
    /// this flag ensures that the recalculation is done lazily, only when required
    time_table_outdated: bool,
}

impl<Var> CPPropagatorConstructor for CumulativeArgs<Var, TimeTablePerPointIncrementalProp<Var>>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Propagator = TimeTablePerPointIncrementalProp<Var>;

    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let (tasks, horizon) = Util::create_tasks(&self.tasks, context);
        TimeTablePerPointIncrementalProp::new(CumulativeParameters::new(
            tasks,
            self.capacity,
            horizon,
        ))
    }
}

impl<Var: IntVar + 'static> TimeTablePerPointIncrementalProp<Var> {
    pub fn new(parameters: CumulativeParameters<Var>) -> TimeTablePerPointIncrementalProp<Var> {
        let reasons_for_propagation: Vec<HashMap<i32, PropositionalConjunction>> =
            vec![HashMap::new(); parameters.tasks.len()];
        TimeTablePerPointIncrementalProp {
            time_table: BTreeMap::new(),
            reasons_for_propagation_lower_bound: reasons_for_propagation.to_vec(),
            reasons_for_propagation_upper_bound: reasons_for_propagation,
            parameters,
            time_table_outdated: false,
        }
    }
}

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator
    for TimeTablePerPointIncrementalProp<Var>
{
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            Util::check_bounds_equal_at_propagation(
                context,
                &self.parameters.tasks,
                &self.parameters.bounds,
            ),
            "Bound were not equal when propagating"
        );
        if self.time_table_outdated {
            // The time-table needs to be recalculated from scratch anyways so we perform the calculation now
            TimeTablePropagator::reset_structures(self, context)?;
            self.time_table_outdated = false;
        } else {
            for Updated {
                task,
                old_lower_bound,
                old_upper_bound,
                new_lower_bound,
                new_upper_bound,
            } in self.parameters.updated.iter()
            {
                //Go over all of the updated tasks and calculate the added mandatory part (we know that for each of these tasks, a mandatory part exists, otherwise it would not have been added (see [TimeTablePropagator::should_propagate]))
                let (lower_update_range, upper_update_range) = generate_update_range(
                    task,
                    *old_lower_bound,
                    *old_upper_bound,
                    *new_lower_bound,
                    *new_upper_bound,
                );

                for t in lower_update_range
                    .into_iter()
                    .chain(upper_update_range.into_iter())
                {
                    //We first go over the lower update range and then we go over the lower update range
                    pumpkin_assert_extreme!(
                        !self.time_table.contains_key(&(t as u32))
                        || !self.time_table.get(&(t as u32)).unwrap().profile_tasks.iter().any(|profile_task| profile_task.id.unpack() as usize == task.id.unpack() as usize),
                        "Attempted to insert mandatory part where it already exists at time point {t} for task {} in time-table per time-point propagator", task.id.unpack() as usize);

                    //Add the updated profile to the ResourceProfile at time t
                    let current_profile: &mut ResourceProfile<Var> = self
                        .time_table
                        .entry(t as u32)
                        .or_insert(ResourceProfile::default(t));
                    current_profile.height += task.resource_usage;
                    current_profile.profile_tasks.push(Rc::clone(task));
                    if current_profile.height > self.parameters.capacity {
                        //The newly introduced mandatory part(s) caused an overflow of the resource
                        return Util::create_error_clause(context, &current_profile.profile_tasks);
                    }
                }
            }
        }

        self.parameters.updated.clear(); //We have processed all of the updates, we can clear the structure
                                         //We pass the entirety of the table to check due to the fact that the propagation of the current profile could lead to the propagation across multiple profiles
                                         //For example, if we have updated 1 resource profile which caused a propagation then this could cause another propagation by a profile which has not been updated
        let PropagationStatusWithExplanation {
            status,
            explanations,
        } = check_for_updates(context, self.get_time_table(), self.get_parameters());

        Util::store_explanations(
            explanations,
            &mut self.reasons_for_propagation_lower_bound,
            &mut self.reasons_for_propagation_upper_bound,
        );
        status
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        Util::reset_bounds_clear_updated(
            context,
            &mut self.parameters.updated,
            &mut self.parameters.bounds,
            &self.parameters.tasks,
        );
        self.time_table_outdated = true;
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        Util::get_reason_for_propagation(
            delta,
            &self.reasons_for_propagation_lower_bound,
            &self.reasons_for_propagation_upper_bound,
            &self.parameters.tasks,
        )
    }

    fn notify(
        &mut self,
        context: &mut PropagationContext,
        local_id: crate::engine::LocalId,
        _event: crate::engine::OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        should_enqueue(
            &mut self.parameters,
            updated_task,
            context,
            self.time_table.is_empty(), // Time table could be out-of-date but if it is empty then backtracking will not make it more empty
        )
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        Util::initialise_at_root(true, &mut self.parameters, context);
        let PropagationStatusWithExplanation {
            status,
            explanations,
        } = TimeTablePropagator::propagate_from_scratch(self, context);

        Util::store_explanations(
            explanations,
            &mut self.reasons_for_propagation_lower_bound,
            &mut self.reasons_for_propagation_upper_bound,
        );
        status
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        //Use the same debug propagator from `TimeTablePerPoint`
        TimeTablePerPointProp::debug_propagate_from_scratch_time_table_point(
            context,
            self.get_parameters(),
        )
    }
}

impl<Var: IntVar + 'static> TimeTablePropagator<Var> for TimeTablePerPointIncrementalProp<Var> {
    type TimeTableType = PerPointTimeTableType<Var>;
    type TimeTableIteratorType<'a> = PerPointIteratorType<'a, Var>;

    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
    ) -> Result<(), Vec<Rc<Task<Var>>>> {
        match <TimeTablePerPointIncrementalProp<Var> as TimeTablePropagator<Var>>::create_time_table(
            context,
            self.get_parameters(),
        ) {
            Ok(result) => {
                self.time_table = result;
                Ok(())
            }
            Err(explanation) => Err(explanation),
        }
    }

    fn create_time_table(
        context: &PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) -> Result<Self::TimeTableType, Vec<Rc<Task<Var>>>> {
        TimeTablePerPointProp::create_time_table_per_point_from_scratch(context, parameters)
    }

    fn get_parameters(&self) -> &CumulativeParameters<Var> {
        &self.parameters
    }

    fn get_time_table(&self) -> Self::TimeTableIteratorType<'_> {
        self.time_table.values()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::{
            ConflictInfo, Inconsistency, Predicate, PredicateConstructor, PropositionalConjunction,
        },
        engine::{test_helper::TestSolver, Delta, DomainChange, EnqueueDecision, LocalId},
        propagators::{ArgTask, TimeTablePerPointIncremental},
    };

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);
    }

    #[test]
    fn propagator_detects_conflict() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 1);

        let result = solver.new_propagator(TimeTablePerPointIncremental::new(
            [
                ArgTask {
                    start_time: s1,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2,
                    processing_time: 4,
                    resource_usage: 1,
                },
            ]
            .into_iter()
            .collect(),
            1,
        ));
        assert!(match result {
            Err(Inconsistency::Other(ConflictInfo::Explanation(x))) => {
                let expected = [
                    s1.upper_bound_predicate(1),
                    s1.lower_bound_predicate(1),
                    s2.upper_bound_predicate(1),
                    s2.lower_bound_predicate(1),
                ];
                expected
                    .iter()
                    .all(|y| x.iter().collect::<Vec<&Predicate>>().contains(&y))
                    && x.iter().all(|y| expected.contains(y))
            }
            _ => false,
        });
    }

    #[test]
    fn propagator_propagates_nothing() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(0, 6);

        solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 6);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(2, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b = solver.new_variable(2, 3);
        let a = solver.new_variable(0, 1);

        solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 5,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect(),
                5,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);

        let mut propagator = solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        let notification_status = solver.increase_lower_bound_and_notify(&mut propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 7);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 5);
        assert_eq!(solver.upper_bound(s1), 6);
    }

    #[test]
    fn propagator_propagates_end_time() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(6, 6);
        let s2 = solver.new_variable(1, 8);

        let mut propagator = solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                1,
            ))
            .expect("No conflict");
        let result = solver.propagate_until_fixed_point(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::UpperBound(3)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(6),
                s1.lower_bound_predicate(6),
                s1.upper_bound_predicate(6),
            ]),
            reason
        );
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt_after_update() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(0, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b = solver.new_variable(2, 3);
        let a = solver.new_variable(0, 1);

        let mut propagator = solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 4,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect(),
                5,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(a), 0);
        assert_eq!(solver.upper_bound(a), 1);
        assert_eq!(solver.lower_bound(b), 2);
        assert_eq!(solver.upper_bound(b), 3);
        assert_eq!(solver.lower_bound(c), 8);
        assert_eq!(solver.upper_bound(c), 9);
        assert_eq!(solver.lower_bound(d), 0);
        assert_eq!(solver.upper_bound(d), 2);
        assert_eq!(solver.lower_bound(e), 0);
        assert_eq!(solver.upper_bound(e), 4);
        assert_eq!(solver.lower_bound(f), 0);
        assert_eq!(solver.upper_bound(f), 14);

        let notification_status = solver.increase_lower_bound_and_notify(&mut propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt_multiple_profiles() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(0, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b2 = solver.new_variable(5, 5);
        let b1 = solver.new_variable(3, 3);
        let a = solver.new_variable(0, 1);

        let mut propagator = solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b1,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: b2,
                        processing_time: 3,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 4,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect(),
                5,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(a), 0);
        assert_eq!(solver.upper_bound(a), 1);
        assert_eq!(solver.lower_bound(c), 8);
        assert_eq!(solver.upper_bound(c), 9);
        assert_eq!(solver.lower_bound(d), 0);
        assert_eq!(solver.upper_bound(d), 2);
        assert_eq!(solver.lower_bound(e), 0);
        assert_eq!(solver.upper_bound(e), 4);
        assert_eq!(solver.lower_bound(f), 0);
        assert_eq!(solver.upper_bound(f), 14);

        let notification_status = solver.increase_lower_bound_and_notify(&mut propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let mut propagator = solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::LowerBound(5)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.lower_bound_predicate(2), //Note that this not the most general explanation, if s2 could have started at 0 then it would still have overlapped with the current interval
                s1.lower_bound_predicate(1),
                s1.upper_bound_predicate(1),
            ]),
            reason
        );
    }

    #[test]
    fn propagator_propagates_generic_bounds() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(3, 3);
        let s2 = solver.new_variable(5, 5);
        let s3 = solver.new_variable(1, 15);

        let mut propagator = solver
            .new_propagator(TimeTablePerPointIncremental::new(
                [
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                1,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(2), DomainChange::LowerBound(7)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(5),
                s2.lower_bound_predicate(5),
                s3.lower_bound_predicate(3), //Note that s3 would have been able to propagate this bound even if it started at time 0
            ]),
            reason
        );
    }
}
