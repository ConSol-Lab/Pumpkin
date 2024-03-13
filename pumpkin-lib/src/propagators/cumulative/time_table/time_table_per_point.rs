//! [`ConstraintProgrammingPropagator`] for the Cumulative constraint; it
//! reasons over individual time-points instead of intervals. See [`TimeTablePerPointPropagator`]
//! for more information.

use std::collections::BTreeMap;
use std::rc::Rc;

use super::time_table_util::propagate_based_on_timetable;
use super::time_table_util::should_enqueue;
use super::time_table_util::ResourceProfile;
use crate::basic_types::variables::IntVar;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::EnqueueDecision;
use crate::engine::PropagationContext;
use crate::engine::PropagationContextMut;
use crate::engine::PropagatorConstructorContext;
use crate::engine::ReadDomains;
use crate::propagators::reset_bounds_clear_updated;
use crate::propagators::update_bounds_task;
use crate::propagators::util::create_inconsistency;
use crate::propagators::util::create_tasks;
use crate::propagators::CumulativeArgs;
use crate::propagators::CumulativeParameters;
use crate::pumpkin_assert_extreme;

/// [`ConstraintProgrammingPropagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a [`ResourceProfile`] per time point rather than
/// creating one over an interval (hence the name). Furthermore, the [`TimeTablePerPointPropagator`]
/// has a generic argument which represents the type of variable used for modelling the start
/// variables, this will be an implementation of [`IntVar`].
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug)]
pub struct TimeTablePerPointPropagator<Var> {
    /// Stores whether the time-table is empty
    is_time_table_empty: bool,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
}

/// The type of the time-table used by propagators which use time-table reasoning per time-point;
/// using a [`ResourceProfile`] is more complex than necessary (as [`ResourceProfile::start`] =
/// [`ResourceProfile::end`]) but it allows for a more unified implementation of methods.
///
/// The key t (representing a time-point) holds the mandatory resource consumption of tasks at
/// that time (stored in a [`ResourceProfile`]); the [ResourceProfile]s are sorted based on
/// start time and they are non-overlapping
pub(crate) type PerPointTimeTableType<Var> = BTreeMap<u32, ResourceProfile<Var>>;

impl<Var> CPPropagatorConstructor for CumulativeArgs<Var, TimeTablePerPointPropagator<Var>>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Propagator = TimeTablePerPointPropagator<Var>;

    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let tasks = create_tasks(&self.tasks, context);
        TimeTablePerPointPropagator::new(CumulativeParameters::new(tasks, self.capacity))
    }
}

impl<Var: IntVar + 'static> TimeTablePerPointPropagator<Var> {
    pub fn new(parameters: CumulativeParameters<Var>) -> TimeTablePerPointPropagator<Var> {
        TimeTablePerPointPropagator {
            is_time_table_empty: true,
            parameters,
        }
    }

    pub(crate) fn debug_propagate_from_scratch_time_table_point(
        context: &mut PropagationContextMut,
        parameters: &CumulativeParameters<Var>,
    ) -> PropagationStatusCP {
        // We first create a time-table per point in the horizon and return an error if there was
        // one while building the time-table
        let time_table = TimeTablePerPointPropagator::create_time_table_per_point_from_scratch(
            context, parameters,
        )?;
        // Then we check whether propagation can take place
        propagate_based_on_timetable(context, time_table.values(), parameters)
    }

    /// Creates a time-table consisting of [`ResourceProfile`]s which represent rectangles with a
    /// start and end (both inclusive) consisting of tasks with a cumulative height Assumptions:
    /// The time-table is sorted based on start time and none of the profiles overlap - generally,
    /// it is assumed that the calculated [`ResourceProfile`]s are maximal
    ///
    /// The result of this method is either the time-table of type
    /// [`PerPointTimeTableType`] or the tasks responsible for the
    /// conflict in the form of an [`Inconsistency`].
    pub(crate) fn create_time_table_per_point_from_scratch(
        context: &PropagationContextMut,
        parameters: &CumulativeParameters<Var>,
    ) -> Result<PerPointTimeTableType<Var>, Inconsistency> {
        let mut time_table: PerPointTimeTableType<Var> = PerPointTimeTableType::new();
        // First we go over all tasks and determine their mandatory parts
        for task in parameters.tasks.iter() {
            let upper_bound = context.upper_bound(&task.start_variable);
            let lower_bound = context.lower_bound(&task.start_variable);

            if upper_bound < lower_bound + task.processing_time {
                // There is a mandatory part
                for i in upper_bound..(lower_bound + task.processing_time) {
                    // For every time-point of the mandatory part,
                    //  add the resource usage of the current task to the ResourceProfile and add it
                    // to the profile tasks of the resource
                    let current_profile: &mut ResourceProfile<Var> = time_table
                        .entry(i as u32)
                        .or_insert(ResourceProfile::default(i));
                    current_profile.height += task.resource_usage;
                    current_profile.profile_tasks.push(Rc::clone(task));

                    if current_profile.height > parameters.capacity {
                        // The addition of the current task to the resource profile has caused an
                        // overflow
                        return Err(create_inconsistency(
                            context,
                            &current_profile.profile_tasks,
                        ));
                    }
                }
            }
        }
        pumpkin_assert_extreme!(
            time_table
                .values()
                .all(|profile| profile.start == profile.end),
            "The TimeTablePerPoint method should only create profiles where `start == end`"
        );
        Ok(time_table)
    }
}

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator for TimeTablePerPointPropagator<Var> {
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        let time_table = TimeTablePerPointPropagator::create_time_table_per_point_from_scratch(
            context,
            &self.parameters,
        )?;
        self.is_time_table_empty = time_table.is_empty();
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(context, time_table.values(), &self.parameters)
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        reset_bounds_clear_updated(
            context,
            &mut self.parameters.updated,
            &mut self.parameters.bounds,
            &self.parameters.tasks,
        );
    }

    fn notify(
        &mut self,
        context: &mut PropagationContextMut,
        local_id: crate::engine::LocalId,
        _event: crate::engine::OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        // Note that it could be the case that `is_time_table_empty` is inaccurate here since it
        // wasn't updated in `synchronise`; however, `synchronise` will only remove profiles
        // meaning that `is_time_table_empty` will always return `false` when it is not
        // empty and it might return `false` even when the time-table is not empty *but* it
        // will never return `true` when the time-table is not empty.
        let result = should_enqueue(
            &self.parameters,
            &updated_task,
            context,
            self.is_time_table_empty,
        );
        update_bounds_task(context, &mut self.parameters.bounds, &updated_task);
        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        // First we store the bounds in the parameters
        for task in self.parameters.tasks.iter() {
            self.parameters.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ))
        }

        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        TimeTablePerPointPropagator::debug_propagate_from_scratch_time_table_point(
            context,
            &self.parameters,
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::ConflictInfo;
    use crate::basic_types::Inconsistency;
    use crate::basic_types::Predicate;
    use crate::basic_types::PredicateConstructor;
    use crate::basic_types::PropositionalConjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::engine::EnqueueDecision;
    use crate::propagators::ArgTask;
    use crate::propagators::TimeTablePerPoint;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let _ = solver
            .new_propagator(TimeTablePerPoint::new(
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

        let result = solver.new_propagator(TimeTablePerPoint::new(
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

        let _ = solver
            .new_propagator(TimeTablePerPoint::new(
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

        let _ = solver
            .new_propagator(TimeTablePerPoint::new(
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
            .new_propagator(TimeTablePerPoint::new(
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
            .new_propagator(TimeTablePerPoint::new(
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

        let reason = solver.get_reason_int(s2.upper_bound_predicate(3)).clone();
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
            .new_propagator(TimeTablePerPoint::new(
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
            .new_propagator(TimeTablePerPoint::new(
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

        let _ = solver
            .new_propagator(TimeTablePerPoint::new(
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

        let reason = solver.get_reason_int(s2.lower_bound_predicate(5)).clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.lower_bound_predicate(2), /* Note that this not the most general explanation, if s2 could have started at 0 then it would still have overlapped with the current interval */
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

        let _ = solver
            .new_propagator(TimeTablePerPoint::new(
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

        let reason = solver.get_reason_int(s3.lower_bound_predicate(7)).clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(5),
                s2.lower_bound_predicate(5),
                s3.lower_bound_predicate(3), /* Note that s3 would have been able to propagate
                                              * this bound even if it started at time 0 */
            ]),
            reason
        );
    }
}
