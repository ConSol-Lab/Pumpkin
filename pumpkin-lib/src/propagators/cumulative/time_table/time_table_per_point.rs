//! [`Propagator`] for the Cumulative constraint; it
//! reasons over individual time-points instead of intervals. See [`TimeTablePerPointPropagator`]
//! for more information.

use std::collections::BTreeMap;
use std::rc::Rc;

use super::time_table_util::propagate_based_on_timetable;
use super::time_table_util::should_enqueue;
use super::time_table_util::ResourceProfile;
use crate::basic_types::PropagationStatusCP;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::predicates::PropositionalConjunction;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::util::create_tasks;
use crate::propagators::util::reset_bounds_clear_updated;
use crate::propagators::util::update_bounds_task;
use crate::propagators::CumulativeConstructor;
use crate::propagators::CumulativeParameters;
use crate::pumpkin_assert_extreme;

/// [`Propagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a resource profile per time point rather than
/// creating one over an interval (hence the name). Furthermore, the [`TimeTablePerPointPropagator`]
/// has a generic argument which represents the type of variable used for modelling the start
/// variables, this will be an implementation of [`IntegerVariable`].
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug)]
pub(crate) struct TimeTablePerPointPropagator<Var> {
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

impl<Var> PropagatorConstructor for CumulativeConstructor<Var, TimeTablePerPointPropagator<Var>>
where
    Var: IntegerVariable + 'static + std::fmt::Debug,
{
    type Propagator = TimeTablePerPointPropagator<Var>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let tasks = create_tasks(&self.tasks, context);
        TimeTablePerPointPropagator::new(CumulativeParameters::new(
            tasks,
            self.capacity,
            self.options,
        ))
    }
}

impl<Var: IntegerVariable + 'static> TimeTablePerPointPropagator<Var> {
    pub(crate) fn new(parameters: CumulativeParameters<Var>) -> TimeTablePerPointPropagator<Var> {
        TimeTablePerPointPropagator {
            is_time_table_empty: true,
            parameters,
        }
    }
}

impl<Var: IntegerVariable + 'static> Propagator for TimeTablePerPointPropagator<Var> {
    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        let time_table =
            create_time_table_per_point_from_scratch(&context.as_readonly(), &self.parameters)?;
        self.is_time_table_empty = time_table.is_empty();
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(&mut context, time_table.values(), &self.parameters)
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
        context: PropagationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
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
            &context,
            self.is_time_table_empty,
        );

        // Note that the non-incremental proapgator does not make use of `result.updated` since it
        // propagates from scratch anyways
        update_bounds_task(&context, &mut self.parameters.bounds, &updated_task);
        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }

    fn initialise_at_root(
        &mut self,
        context: PropagationContext,
    ) -> Result<(), PropositionalConjunction> {
        // First we store the bounds in the parameters
        for task in self.parameters.tasks.iter() {
            self.parameters.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ));
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(&self, context: PropagationContextMut) -> PropagationStatusCP {
        debug_propagate_from_scratch_time_table_point(context, &self.parameters)
    }
}

/// Creates a time-table consisting of [`ResourceProfile`]s which represent rectangles with a
/// start and end (both inclusive) consisting of tasks with a cumulative height Assumptions:
/// The time-table is sorted based on start time and none of the profiles overlap - generally,
/// it is assumed that the calculated [`ResourceProfile`]s are maximal
///
/// The result of this method is either the time-table of type
/// [`PerPointTimeTableType`] or the tasks responsible for the
/// conflict in the form of an [`Inconsistency`].
pub(crate) fn create_time_table_per_point_from_scratch<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    parameters: &CumulativeParameters<Var>,
) -> Result<PerPointTimeTableType<Var>, PropositionalConjunction> {
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
                    return Err(create_conflict_explanation(
                        context,
                        current_profile,
                        parameters.options.explanation_type,
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

pub(crate) fn debug_propagate_from_scratch_time_table_point<Var: IntegerVariable + 'static>(
    mut context: PropagationContextMut,
    parameters: &CumulativeParameters<Var>,
) -> PropagationStatusCP {
    // We first create a time-table per point and return an error if there was
    // an overflow of the resource capacity while building the time-table
    let time_table = create_time_table_per_point_from_scratch(&context.as_readonly(), parameters)?;
    // Then we check whether propagation can take place
    propagate_based_on_timetable(&mut context, time_table.values(), parameters)
}

#[cfg(test)]
mod tests {
    use crate::basic_types::ConflictInfo;
    use crate::basic_types::Inconsistency;
    use crate::basic_types::PropositionalConjunction;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_helper::TestSolver;
    use crate::predicate;
    use crate::propagators::ArgTask;
    use crate::propagators::CumulativeExplanationType;
    use crate::propagators::CumulativeOptions;
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::default(),
                    generate_sequence: false,
                },
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
            CumulativeOptions {
                allow_holes_in_domain: false,
                explanation_type: CumulativeExplanationType::Naive,
                generate_sequence: false,
            },
        ));
        assert!(match result {
            Err(Inconsistency::Other(ConflictInfo::Explanation(x))) => {
                let expected = [
                    predicate!(s1 <= 1),
                    predicate!(s1 >= 1),
                    predicate!(s2 <= 1),
                    predicate!(s2 >= 1),
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::default(),
                    generate_sequence: false,
                },
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::default(),
                    generate_sequence: false,
                },
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::default(),
                    generate_sequence: false,
                },
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::Naive,
                    generate_sequence: false,
                },
            ))
            .expect("No conflict");
        let result = solver.propagate_until_fixed_point(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver
            .get_reason_int(predicate!(s2 <= 3).try_into().unwrap())
            .clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                predicate!(s2 <= 5),
                predicate!(s1 >= 6),
                predicate!(s1 <= 6),
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::default(),
                    generate_sequence: false,
                },
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::default(),
                    generate_sequence: false,
                },
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::Naive,
                    generate_sequence: false,
                },
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver
            .get_reason_int(predicate!(s2 >= 5).try_into().unwrap())
            .clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                predicate!(s2 >= 4),
                predicate!(s1 >= 1),
                predicate!(s1 <= 1), /* Note that this not the most general explanation, if s2
                                      * could have started at 0 then it would still have
                                      * overlapped with the current interval */
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
                CumulativeOptions {
                    allow_holes_in_domain: false,
                    explanation_type: CumulativeExplanationType::Naive,
                    generate_sequence: false,
                },
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver
            .get_reason_int(predicate!(s3 >= 7).try_into().unwrap())
            .clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                predicate!(s2 <= 5),
                predicate!(s2 >= 5),
                predicate!(s3 >= 6), /* Note that s3 would have been able to propagate
                                      * this bound even if it started at time 0 */
            ]),
            reason
        );
    }

    #[test]
    fn propagator_propagates_with_holes() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(4, 4);
        let s2 = solver.new_variable(0, 8);

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
                CumulativeOptions {
                    allow_holes_in_domain: true,
                    explanation_type: CumulativeExplanationType::Naive,
                    generate_sequence: false,
                },
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 4);
        assert_eq!(solver.upper_bound(s1), 4);

        for removed in 2..8 {
            assert!(!solver.contains(s2, removed));
            let reason = solver
                .get_reason_int(predicate!(s2 != removed).try_into().unwrap())
                .clone();
            assert_eq!(
                PropositionalConjunction::from(vec![predicate!(s1 <= 4), predicate!(s1 >= 4),]),
                reason
            );
        }
    }
}
