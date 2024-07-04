use std::cmp::max;
use std::cmp::min;
use std::ops::Range;
use std::rc::Rc;

use super::time_table_util::has_overlap_with_interval;
use super::time_table_util::should_enqueue;
use super::time_table_util::ResourceProfile;
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
use crate::propagators::cumulative::time_table::time_table_util::generate_update_range;
use crate::propagators::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::propagators::util::check_bounds_equal_at_propagation;
use crate::propagators::util::create_inconsistency;
use crate::propagators::util::create_tasks;
use crate::propagators::util::reset_bounds_clear_updated;
use crate::propagators::util::update_bounds_task;
use crate::propagators::CumulativeConstructor;
use crate::propagators::CumulativeParameters;
use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::TimeTableOverIntervalPropagator;
use crate::propagators::UpdatedTaskInfo;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;

/// [`Propagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a resource profile over an interval rather than
/// creating one per time-point (hence the name). Furthermore, the
/// [`TimeTableOverIntervalPropagator`] has a generic argument which represents the type of variable
/// used for modelling the start variables, this will be an implementation of [`IntegerVariable`].
///
/// The difference between the [`TimeTableOverIntervalIncrementalPropagator`] and
/// [`TimeTableOverIntervalPropagator`] is that the [`TimeTableOverIntervalIncrementalPropagator`]
/// does not recalculate the time-table from scratch whenever the
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
pub struct TimeTableOverIntervalIncrementalPropagator<Var> {
    /// The key `t` (representing a time-point) holds the mandatory resource consumption of
    /// [`Task`]s at that time (stored in a [`ResourceProfile`]); the [`ResourceProfile`]s are
    /// sorted based on start time and they are assumed to be non-overlapping
    time_table: OverIntervalTimeTableType<Var>,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
    /// Keeps track of whether the current state of the
    /// [`TimeTableOverIntervalIncrementalPropagator::time_table`] is outdated (i.e. whether it
    /// needs to be recalculated from scratch). This can occur due to backtracking.
    ///
    /// Imagine the situation where a synchronisation takes place and the propagator eagerly
    /// recalculates the time-table but then another propagator finds a conflict and the
    /// time-table calculation was superfluous; this flag ensures that the recalculation is
    /// done lazily, only when required.
    time_table_outdated: bool,
}

impl<Var> PropagatorConstructor
    for CumulativeConstructor<Var, TimeTableOverIntervalIncrementalPropagator<Var>>
where
    Var: IntegerVariable + 'static + std::fmt::Debug,
{
    type Propagator = TimeTableOverIntervalIncrementalPropagator<Var>;

    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let tasks = create_tasks(&self.tasks, context);
        TimeTableOverIntervalIncrementalPropagator::new(CumulativeParameters::new(
            tasks,
            self.capacity,
            self.allow_holes_in_domain,
        ))
    }
}

impl<Var: IntegerVariable + 'static> TimeTableOverIntervalIncrementalPropagator<Var> {
    pub fn new(
        parameters: CumulativeParameters<Var>,
    ) -> TimeTableOverIntervalIncrementalPropagator<Var> {
        TimeTableOverIntervalIncrementalPropagator {
            time_table: Default::default(),
            parameters,
            time_table_outdated: false,
        }
    }

    /// Performs a binary search on the
    /// [time-table][TimeTableOverIntervalIncrementalPropagator::time_table] to find *an* element
    /// which overlaps with the `update_range`. If such an element can be found then it returns
    /// [Ok] containing the index of the overlapping profile. If no such element could be found,
    /// it returns [Err] containing the index at which the element should be inserted to
    /// preserve the ordering
    fn find_overlapping_profile(
        time_table: &OverIntervalTimeTableType<Var>,
        update_range: &Range<i32>,
    ) -> Result<usize, usize> {
        time_table.binary_search_by(|profile| {
            if has_overlap_with_interval(
                update_range.start,
                update_range.end,
                profile.start,
                profile.end,
            ) {
                return std::cmp::Ordering::Equal;
            } else if profile.end < update_range.start {
                return std::cmp::Ordering::Less;
            }
            std::cmp::Ordering::Greater
        })
    }

    /// Determines which profiles are required to be updated given a range of times which now
    /// include a mandatory part (i.e. determine the profiles which overlap with the update_range).
    /// It returns two indices into
    /// [time_table][TimeTableOverIntervalIncrementalPropagator::time_table] representing the
    /// index of the first profile which overlaps with the update_range (inclusive)
    /// and the index of the last profile which overlaps with the update_range (inclusive) or [None]
    /// if there are no overlapping profiles
    ///
    /// Note that the lower-bound of the range is inclusive and the upper-bound is exclusive
    fn determine_profiles_to_update(
        time_table: &OverIntervalTimeTableType<Var>,
        update_range: &Range<i32>,
    ) -> Result<(usize, usize), usize> {
        let overlapping_profile =
            TimeTableOverIntervalIncrementalPropagator::find_overlapping_profile(
                time_table,
                update_range,
            );

        if overlapping_profile.is_err() {
            // We have not found any profile which overlaps with the update range
            return Err(overlapping_profile.expect_err("Overlapping profile could not be found"));
        }

        // Now we need to find all of the profiles which are adjacent to `overlapping_profile` which
        // also overlap with `update_range` Our starting index is thus the index of
        // `overlapping_profile` and we need to search to the left and the right of the profile to
        // find the other overlapping profiles
        let mut left_most_overlapping_index = overlapping_profile.unwrap();
        let mut right_most_overlapping_index = left_most_overlapping_index;
        if left_most_overlapping_index > 0 {
            // We go to the left of `overlapping_profile`
            for left_profile_index in (0..left_most_overlapping_index).rev() {
                let profile = &time_table[left_profile_index];
                if has_overlap_with_interval(
                    update_range.start,
                    update_range.end,
                    profile.start,
                    profile.end,
                ) {
                    // We now know that the left most overlapping index is either
                    // `left_profile_index` or before it
                    left_most_overlapping_index = left_profile_index;
                } else {
                    // We know that no profile on the left of this one will be overlapping with
                    // `update_range`
                    break;
                }
            }
        }

        // We go to the right of `overlapping_profile`
        for (right_profile_index, _) in time_table
            .iter()
            .enumerate()
            .skip(left_most_overlapping_index + 1)
        {
            let profile = &time_table[right_profile_index];
            if has_overlap_with_interval(
                update_range.start,
                update_range.end,
                profile.start,
                profile.end,
            ) {
                // We now know that the right most overlapping index is either `right_profile_index`
                // or after it
                right_most_overlapping_index = right_profile_index;
            } else {
                // We know that no profile on the right of this one will be overlapping with
                // `update_range`
                break;
            }
        }
        Ok((left_most_overlapping_index, right_most_overlapping_index))
    }
}

impl<Var: IntegerVariable + 'static + std::fmt::Debug> Propagator
    for TimeTableOverIntervalIncrementalPropagator<Var>
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            check_bounds_equal_at_propagation(
                context,
                &self.parameters.tasks,
                &self.parameters.bounds,
            ),
            "Bound were not equal when propagating"
        );
        if self.time_table_outdated {
            // The time-table needs to be recalculated from scratch anyways so we perform the
            // calculation now
            self.time_table =
                TimeTableOverIntervalPropagator::create_time_table_over_interval_from_scratch(
                    context,
                    &self.parameters,
                )?;
            self.time_table_outdated = false;
        } else {
            // We keep track of the lowest index which can be merged
            let mut lowest_index = u32::MAX;
            for UpdatedTaskInfo {
                task,
                old_lower_bound,
                old_upper_bound,
                new_lower_bound,
                new_upper_bound,
            } in self.parameters.updated.iter()
            {
                let added_mandatory_consumption = generate_update_range(
                    task,
                    *old_lower_bound,
                    *old_upper_bound,
                    *new_lower_bound,
                    *new_upper_bound,
                );
                // We consider both of the possible update ranges
                // Note that the upper update range is first considered to avoid any issues with the
                // indices when processing the other update range
                for update_range in added_mandatory_consumption.get_reverse_update_ranges() {
                    // Keep track of the profiles which need to be added
                    let mut to_add: Vec<ResourceProfile<Var>> = Vec::new();

                    // First we attempt to find overlapping profiles
                    match TimeTableOverIntervalIncrementalPropagator::determine_profiles_to_update(
                        &self.time_table,
                        &update_range,
                    ) {
                        Ok((start_index, end_index)) => {
                            // Updates the lowest index of profiles which has been updated
                            lowest_index = min(
                                lowest_index,
                                if start_index == 0 {
                                    0
                                } else {
                                    start_index as u32 - 1
                                },
                            );

                            // Go over all indices of the profiles which overlap with the updated
                            // one and determine which one need to be updated
                            for current_index in start_index..=end_index {
                                let profile = &self.time_table[current_index];

                                if current_index == start_index
                                    && update_range.start < profile.start
                                {
                                    // We are considering the first overlapping profile and there is
                                    // a part before the start of this profile
                                    // This means we need to add a new mandatory part before the
                                    // first element
                                    to_add.push(ResourceProfile {
                                        start: update_range.start,
                                        end: profile.start - 1, // Note that this profile needs to end before the start of the current profile, hence the -1
                                        profile_tasks: vec![Rc::clone(task)],
                                        height: task.resource_usage,
                                    })
                                }
                                if current_index != start_index && current_index != 0 {
                                    // We are not considering the first profile and there could be a
                                    // new profile between the current profile and the previous one
                                    // caused by the updated task
                                    let previous_profile = &self.time_table[current_index - 1];

                                    // The following three points are checked:
                                    // - There is empty space between the current profile and the
                                    //   previous profile
                                    // - The update range starts before the end of the previous
                                    //   profile
                                    // - The update range ends after the start of the current
                                    //   profile
                                    if previous_profile.end < profile.start - 1
                                        && update_range.start <= previous_profile.end + 1
                                        && update_range.end > profile.start - 1
                                    {
                                        // There is empty space between the current profile and the
                                        // previous one, we should insert a new profile
                                        to_add.push(ResourceProfile {
                                            start: previous_profile.end + 1,
                                            end: profile.start - 1,
                                            profile_tasks: vec![Rc::clone(task)],
                                            height: task.resource_usage,
                                        })
                                    }
                                }

                                if update_range.start > profile.start {
                                    // We are splitting the current profile into one or more parts
                                    // The update range starts after the profile starts;
                                    // This if-statement takes care of creating a new (smaller)
                                    // profile which represents the previous profile up and until it
                                    // is split by the update range
                                    to_add.push(ResourceProfile {
                                        start: profile.start,
                                        end: min(update_range.start - 1, profile.end), // It could be that the update range extends past the profile in which case we should create a profile until the end of the profile
                                        profile_tasks: profile.profile_tasks.clone(),
                                        height: profile.height,
                                    })
                                }

                                // Now we create a new profile which consists of the part of the
                                // profile covered by the update range
                                // This means that we are adding the contribution of the updated
                                // task to the profile and adjusting the bounds appropriately

                                // Either the new profile starts at the start of the profile (in
                                // case the update range starts before the profile start)
                                // or the new profile starts at the start of the update range (since
                                // we are only looking at the part where there is overlap between
                                // the current profile and the update range)
                                let new_profile_lower_bound =
                                    max(profile.start, update_range.start);

                                // Either the new profile ends at the end of the profile (in case
                                // the update range ends after the profile end)
                                // or the new profile ends at the end of the update range (since we
                                // are only looking at the part where there is overlap between the
                                // current profile and the update range)
                                let new_profile_upper_bound =
                                    min(profile.end, update_range.end - 1); // Note that the end of the update_range is exclusive (hence the -1)
                                let mut new_profile_tasks = profile.profile_tasks.clone();
                                new_profile_tasks.push(Rc::clone(task));
                                if new_profile_upper_bound >= new_profile_lower_bound {
                                    // A sanity check, there is a new profile to create consisting
                                    // of a combination of the previous profile and the updated task
                                    if profile.height + task.resource_usage
                                        > self.parameters.capacity
                                    {
                                        // The addition of the new mandatory part to the profile
                                        // caused an overflow of the resource
                                        return Err(create_inconsistency(
                                            context,
                                            &new_profile_tasks,
                                        ));
                                    }

                                    // We thus create a new profile consisting of the combination of
                                    // the previous profile and the updated task under consideration
                                    to_add.push(ResourceProfile {
                                        start: new_profile_lower_bound,
                                        end: new_profile_upper_bound,
                                        profile_tasks: new_profile_tasks,
                                        height: profile.height + task.resource_usage,
                                    })
                                }

                                if profile.end >= update_range.end {
                                    // We are splitting the current profile into one or more parts
                                    // The update range ends before the end of the profile;
                                    // This if-statement takes care of creating a new (smaller)
                                    // profile which represents the previous profile after it is
                                    // split by the update range
                                    to_add.push(ResourceProfile {
                                        start: max(update_range.end, profile.start),
                                        end: profile.end,
                                        profile_tasks: profile.profile_tasks.clone(),
                                        height: profile.height,
                                    })
                                }
                                if current_index == end_index && update_range.end > profile.end + 1
                                {
                                    // We are considering the last overlapping profile and there is
                                    // a part after the end of this profile
                                    // This means we need to add a new mandatory part after the last
                                    // element
                                    to_add.push(ResourceProfile {
                                        start: profile.end + 1,
                                        end: update_range.end - 1,
                                        profile_tasks: vec![Rc::clone(task)],
                                        height: task.resource_usage,
                                    })
                                }
                            }
                            // We now update the time-table to insert the newly created profiles at
                            // the right place to ensure the ordering invariant
                            let _ = self.time_table.splice(start_index..end_index + 1, to_add);
                        }
                        Err(index_to_insert) => {
                            // Update the lowest index which we have found so far which has been
                            // updated
                            lowest_index = min(
                                lowest_index,
                                if index_to_insert == 0 {
                                    0
                                } else {
                                    index_to_insert as u32 - 1
                                },
                            );

                            pumpkin_assert_moderate!(
                                index_to_insert <= self.time_table.len()
                                    || index_to_insert >= self.time_table.len()
                                    || self.time_table[index_to_insert].start
                                        > update_range.end - 1,
                                "The index to insert at is incorrect"
                            );

                            // Insert the new profile at its index
                            self.time_table.insert(
                                index_to_insert,
                                ResourceProfile {
                                    start: update_range.start,
                                    end: update_range.end - 1,
                                    profile_tasks: vec![Rc::clone(task)],
                                    height: task.resource_usage,
                                },
                            );
                        }
                    }
                }
            }
        }

        pumpkin_assert_extreme!({
            let time_table_scratch = TimeTableOverIntervalPropagator::create_time_table_over_interval_from_scratch(
                context,
                &self.parameters,
            )
            .expect("Expected no error");
            self.time_table.len() == time_table_scratch.len()
                && self
                    .time_table
                    .iter()
                    .zip(time_table_scratch)
                    .all(|(actual, expected)| {
                        actual.height == expected.height
                            && actual.start == expected.start
                            && actual.end == expected.end
                            && actual.profile_tasks.len() == expected.profile_tasks.len()
                            && actual
                                .profile_tasks
                                .iter()
                                .all(|task| expected.profile_tasks.contains(task))
                    })
        }, "The profiles were not the same between the incremental and the non-incremental version");

        // We have processed all of the updates, we can clear the structure
        self.parameters.updated.clear();
        // We pass the entirety of the table to check due to the fact that the propagation of the
        // current profile could lead to the propagation across multiple profiles
        // For example, if we have updated 1 resource profile which caused a propagation then this
        // could cause another propagation by a profile which has not been updated
        propagate_based_on_timetable(context, self.time_table.iter(), &self.parameters)
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
        context: &mut PropagationContextMut,
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
            context,
            self.time_table.is_empty(),
        );
        if let Some(update) = result.update {
            self.parameters.updated.push(update)
        }
        update_bounds_task(context, &mut self.parameters.bounds, &updated_task);
        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverIntervalIncremental"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        // First we store the bounds in the parameters
        for task in self.parameters.tasks.iter() {
            self.parameters.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ))
        }
        self.parameters.updated.clear();

        // Then we do normal propagation
        self.time_table =
            TimeTableOverIntervalPropagator::create_time_table_over_interval_from_scratch(
                context,
                &self.parameters,
            )?;
        self.time_table_outdated = false;
        propagate_based_on_timetable(context, self.time_table.iter(), &self.parameters)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        // Use the same debug propagator from `TimeTableOverInterval`
        TimeTableOverIntervalPropagator::debug_propagate_from_scratch_time_table_interval(
            context,
            &self.parameters,
        )
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
    use crate::propagators::TimeTableOverIntervalIncremental;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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

        let result = solver.new_propagator(TimeTableOverIntervalIncremental::new(
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
            false,
        ));
        assert!(match result {
            Err(Inconsistency::Other(ConflictInfo::Explanation(x))) => {
                let expected = [
                    predicate!(s1 <= 1),
                    predicate!(s1 >= 1),
                    predicate!(s2 >= 1),
                    predicate!(s2 <= 1),
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver
            .get_reason_int(predicate!(s2 <= 3).try_into().unwrap())
            .clone();
        assert_eq!(
            PropositionalConjunction::from(vec![
                predicate!(s2 <= 9),
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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
                predicate!(s2 >= 0),
                predicate!(s1 >= 1),
                predicate!(s1 <= 1),
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
            .new_propagator(TimeTableOverIntervalIncremental::new(
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
                false,
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
                predicate!(s3 >= 2),
            ]),
            reason
        );
    }
}
