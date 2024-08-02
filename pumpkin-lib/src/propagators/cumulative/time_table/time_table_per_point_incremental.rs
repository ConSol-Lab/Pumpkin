use std::collections::BTreeMap;
use std::rc::Rc;

use super::create_time_table_per_point_from_scratch;
use super::debug_propagate_from_scratch_time_table_point;
use super::time_table_util::should_enqueue;
use crate::basic_types::PropagationStatusCP;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
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
use crate::propagators::cumulative::time_table::time_table_util::generate_update_range;
use crate::propagators::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::propagators::cumulative::time_table::time_table_util::ResourceProfile;
use crate::propagators::util::check_bounds_equal_at_propagation;
use crate::propagators::util::create_tasks;
use crate::propagators::util::reset_bounds_clear_updated;
use crate::propagators::util::update_bounds_task;
use crate::propagators::CumulativeConstructor;
use crate::propagators::CumulativeParameters;
use crate::propagators::PerPointTimeTableType;
#[cfg(doc)]
use crate::propagators::Task;
#[cfg(doc)]
use crate::propagators::TimeTablePerPointPropagator;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;

/// [`Propagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a resource profile per time point rather than
/// creating one over an interval (hence the name). Furthermore, the [`TimeTablePerPointPropagator`]
/// has a generic argument which represents the type of variable used for modelling the start
/// variables, this will be an implementation of [`IntegerVariable`].
///
/// The difference between the [`TimeTablePerPointIncrementalPropagator`] and
/// [`TimeTablePerPointPropagator`] is that the [`TimeTablePerPointIncrementalPropagator`] does not
/// recalculate the time-table from scratch whenever the
/// [`Propagator::propagate`] method is called but it utilises the
/// [`Propagator::notify`] method to determine when a mandatory part is added
/// and only updates the structure based on these updated mandatory parts.
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug)]
pub(crate) struct TimeTablePerPointIncrementalPropagator<Var> {
    /// The key `t` (representing a time-point) holds the mandatory resource consumption of
    /// [`Task`]s at that time (stored in a [`ResourceProfile`]); the [`ResourceProfile`]s are
    /// sorted based on start time and they are assumed to be non-overlapping
    time_table: PerPointTimeTableType<Var>,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
    /// Keeps track of whether the current state of the
    /// [`TimeTablePerPointIncrementalPropagator::time_table`] is outdated (i.e. whether it
    /// needs to be recalculated from scratch). This can occur due to backtracking.
    ///
    /// Imagine the situation where a synchronisation takes place and the propagator eagerly
    /// recalculates the time-table but then another propagator finds a conflict and the
    /// time-table calculation was superfluous; this flag ensures that the recalculation is
    /// done lazily, only when required.
    time_table_outdated: bool,
}

impl<Var> PropagatorConstructor
    for CumulativeConstructor<Var, TimeTablePerPointIncrementalPropagator<Var>>
where
    Var: IntegerVariable + 'static + std::fmt::Debug,
{
    type Propagator = TimeTablePerPointIncrementalPropagator<Var>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let tasks = create_tasks(&self.tasks, context);
        TimeTablePerPointIncrementalPropagator::new(CumulativeParameters::new(
            tasks,
            self.capacity,
            self.options,
        ))
    }
}

impl<Var: IntegerVariable + 'static> TimeTablePerPointIncrementalPropagator<Var> {
    pub(crate) fn new(
        parameters: CumulativeParameters<Var>,
    ) -> TimeTablePerPointIncrementalPropagator<Var> {
        TimeTablePerPointIncrementalPropagator {
            time_table: BTreeMap::new(),
            parameters,
            time_table_outdated: false,
        }
    }

    /// Updates the stored time-table based on the updates stored in
    /// [`CumulativeParameters::updated`]. If the time-table is outdated then this method will
    /// simply calculate it from scratch.
    ///
    /// An error is returned if an overflow of the resource occurs while updating the time-table.
    fn update_time_table(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        if self.time_table_outdated {
            // The time-table needs to be recalculated from scratch anyways so we perform the
            // calculation now
            self.time_table =
                create_time_table_per_point_from_scratch(&context.as_readonly(), &self.parameters)?;
            self.time_table_outdated = false;
            self.parameters.updated.clear();
        } else {
            for updated_task_info in self.parameters.updated.drain(..) {
                // Go over all of the updated tasks and calculate the added mandatory part (we know
                // that for each of these tasks, a mandatory part exists, otherwise it would not
                // have been added (see [`should_propagate`]))
                let added_mandatory_consumption = generate_update_range(
                    &updated_task_info.task,
                    updated_task_info.old_lower_bound,
                    updated_task_info.old_upper_bound,
                    updated_task_info.new_lower_bound,
                    updated_task_info.new_upper_bound,
                );
                for time_point in added_mandatory_consumption {
                    pumpkin_assert_extreme!(
                        !self.time_table.contains_key(&(time_point as u32))
                        || !self.time_table.get(&(time_point as u32)).unwrap().profile_tasks.iter().any(|profile_task| profile_task.id.unpack() as usize == updated_task_info.task.id.unpack() as usize),
                        "Attempted to insert mandatory part where it already exists at time point {time_point} for task {} in time-table per time-point propagator", updated_task_info.task.id.unpack() as usize);

                    // Add the updated profile to the ResourceProfile at time t
                    let current_profile: &mut ResourceProfile<Var> = self
                        .time_table
                        .entry(time_point as u32)
                        .or_insert(ResourceProfile::default(time_point));

                    current_profile.height += updated_task_info.task.resource_usage;
                    current_profile
                        .profile_tasks
                        .push(Rc::clone(&updated_task_info.task));

                    if current_profile.height > self.parameters.capacity {
                        // The newly introduced mandatory part(s) caused an overflow of the resource
                        return Err(create_conflict_explanation(
                            &context.as_readonly(),
                            current_profile,
                            self.parameters.options.explanation_type,
                        )
                        .into());
                    }
                }
            }
        }
        Ok(())
    }
}

impl<Var: IntegerVariable + 'static> Propagator for TimeTablePerPointIncrementalPropagator<Var> {
    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            check_bounds_equal_at_propagation(
                &context.as_readonly(),
                &self.parameters.tasks,
                &self.parameters.bounds,
            ),
            "Bound were not equal when propagating"
        );

        // We update the time-table based on the stored updates
        self.update_time_table(&mut context)?;

        // We pass the entirety of the table to check due to the fact that the propagation of the
        // current profile could lead to the propagation across multiple profiles
        // For example, if we have updated 1 ResourceProfile which caused a propagation then this
        // could cause another propagation by a profile which has not been updated
        propagate_based_on_timetable(&mut context, self.time_table.values(), &self.parameters)
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        reset_bounds_clear_updated(
            context,
            &mut self.parameters.updated,
            &mut self.parameters.bounds,
            &self.parameters.tasks,
        );
        // If the time-table is already empty then backtracking will not cause it to become outdated
        if !self.time_table.is_empty() {
            self.time_table_outdated = true;
        }
    }

    fn notify(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        // Note that we do not take into account the fact that the time-table could be outdated
        // here; the time-table can only become outdated due to backtracking which means that if the
        // time-table is empty before backtracking then it will necessarily be so after
        // backtracking.
        //
        // However, this could mean that we potentially enqueue even though the time-table is empty
        // after backtracking but has not been recalculated yet.
        let result = should_enqueue(
            &self.parameters,
            &updated_task,
            &context,
            self.time_table.is_empty(),
        );

        // If there is a task which now has a mandatory part then we store it and process it when
        // the `propagate` method is called
        if let Some(update) = result.update {
            self.parameters.updated.push(update)
        }
        update_bounds_task(&context, &mut self.parameters.bounds, &updated_task);
        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPointIncremental"
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
            ))
        }
        self.parameters.updated.clear();

        // Then we do normal propagation
        self.time_table = create_time_table_per_point_from_scratch(&context, &self.parameters)?;
        self.time_table_outdated = false;
        Ok(())
    }

    fn debug_propagate_from_scratch(&self, context: PropagationContextMut) -> PropagationStatusCP {
        // Use the same debug propagator from `TimeTablePerPoint`
        debug_propagate_from_scratch_time_table_point(context, &self.parameters)
    }
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
    use crate::propagators::TimeTablePerPointIncremental;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let _ = solver
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
}
