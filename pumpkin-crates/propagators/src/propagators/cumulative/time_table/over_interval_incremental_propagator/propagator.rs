use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;
use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::asserts::pumpkin_assert_extreme;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::propagator_conflict;
use pumpkin_core::variables::IntegerVariable;
use super::insertion;
use super::removal;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::create_time_table_over_interval_from_scratch;
use crate::cumulative::time_table::propagate_from_scratch_time_table_interval;
use crate::cumulative::util::check_bounds_equal_at_propagation;
use crate::cumulative::util::create_tasks;
use crate::cumulative::util::update_bounds_task;
use crate::cumulative::ArgTask;
use crate::cumulative::MandatoryPartAdjustments;
use crate::cumulative::UpdatableStructures;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::debug;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::check_synchronisation_conflict_explanation_over_interval;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::create_synchronised_conflict_explanation;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::find_synchronised_conflict;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::synchronise_time_table;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::cumulative::time_table::time_table_util::backtrack_update;
use crate::propagators::cumulative::time_table::time_table_util::has_overlap_with_interval;
use crate::propagators::cumulative::time_table::time_table_util::insert_update;
use crate::propagators::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::propagators::cumulative::time_table::time_table_util::should_enqueue;
use crate::cumulative::CumulativeParameters;
use crate::cumulative::time_table::OverIntervalTimeTableType;
use crate::cumulative::Task;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTableOverIntervalPropagator;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTablePerPointPropagator;

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
#[derive(Clone, Debug)]
pub struct TimeTableOverIntervalIncrementalPropagator<Var, const SYNCHRONISE: bool> {
    /// The key `t` (representing a time-point) holds the mandatory resource consumption of
    /// [`Task`]s at that time (stored in a [`ResourceProfile`]); the [`ResourceProfile`]s are
    /// sorted based on start time and they are assumed to be non-overlapping
    pub(super) time_table: OverIntervalTimeTableType<Var>,
    /// Stores the input parameters to the cumulative constraint
    pub(super) parameters: CumulativeParameters<Var>,
    /// Stores structures which change during the search; either to store bounds or when applying
    /// incrementality
    pub(super) updatable_structures: UpdatableStructures<Var>,
    /// Stores whether the propagator found a conflict in the previous call
    ///
    /// This is stored to deal with the case where the same conflict can be created via two
    /// distinct propagation chains; to the propagator it appears that nothing has changed (since
    /// the bounds on the variables remain the same) but there is still a conflicting profile in
    /// the time-table
    pub(super) found_previous_conflict: bool,
    /// Indicates whether the current time-table is outdated and should be recalculated from
    /// scratch or not; note that this variable is only used if
    /// [`CumulativePropagatorOptions::incremental_backtracking`] is set to false.
    pub(super) is_time_table_outdated: bool,

    // TODO: This should be refactored to use a propagator constructor.
    pub(super) constraint_tag: ConstraintTag,
    pub(super) inference_code: Option<InferenceCode>,
}

impl<Var: IntegerVariable + 'static, const SYNCHRONISE: bool>
    TimeTableOverIntervalIncrementalPropagator<Var, SYNCHRONISE>
{
    pub fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> TimeTableOverIntervalIncrementalPropagator<Var, SYNCHRONISE> {
        let tasks = create_tasks(arg_tasks);
        let parameters = CumulativeParameters::new(tasks, capacity, cumulative_options);
        let updatable_structures = UpdatableStructures::new(&parameters);

        TimeTableOverIntervalIncrementalPropagator {
            time_table: Default::default(),
            parameters,
            updatable_structures,
            found_previous_conflict: false,
            is_time_table_outdated: false,
            constraint_tag,
            inference_code: None,
        }
    }

    /// Adds the added parts in the provided [`MandatoryPartAdjustments`] to the time-table; note
    /// that all of the adjustments are applied even if a conflict is found.
    ///
    /// Returns true if the addition to the time-table caused an overflow of the capacity.
    fn conflicting_after_addition_to_time_table(
        &mut self,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var>>,
    ) -> bool {
        let mut found_conflict = false;
        // We consider both of the possible update ranges
        // Note that the upper update range is first considered to avoid any issues with the
        // indices when processing the other update range
        for update_range in mandatory_part_adjustments.get_added_parts() {
            // First we attempt to find overlapping profiles
            match determine_profiles_to_update(&self.time_table, &update_range) {
                Ok((start_index, end_index)) => {
                    let conflicting =
                        insertion::conflicting_after_insertion_of_overlapping_mandatory(
                            &mut self.time_table,
                            start_index,
                            end_index,
                            &update_range,
                            task,
                            self.parameters.capacity,
                        );
                    found_conflict |= conflicting;
                }
                Err(index_to_insert) => insertion::insert_profile_new_mandatory_part(
                    &mut self.time_table,
                    index_to_insert,
                    &update_range,
                    task,
                ),
            }
        }

        found_conflict
    }

    /// Removes the removed parts in the provided [`MandatoryPartAdjustments`] from the time-table
    fn remove_from_time_table(
        &mut self,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var>>,
    ) {
        // We consider both of the possible update ranges
        // Note that the upper update range is first considered to avoid any issues with the
        // indices when processing the other update range
        for update_range in mandatory_part_adjustments.get_removed_parts() {
            // First we attempt to find overlapping profiles
            match determine_profiles_to_update(&self.time_table, &update_range) {
                Ok((start_index, end_index)) => {
                    removal::reduce_profiles_overlapping_with_added_mandatory_part(
                        &mut self.time_table,
                        start_index,
                        end_index,
                        &update_range,
                        task,
                    )
                }
                Err(_) => {
                    panic!("Removal should always have overlap with a profile")
                }
            }
        }
    }

    /// Updates the stored time-table based on the updates stored in
    /// [`DynamicStructures::updated`] or recalculates it from scratch if
    /// [`TimeTableOverIntervalIncrementalPropagator::is_time_table_outdated`] is true.
    ///
    /// An error is returned if an overflow of the resource occurs while updating the time-table.
    fn update_time_table(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        if self.is_time_table_outdated {
            // We create the time-table from scratch (and return an error if it overflows)
            self.time_table = create_time_table_over_interval_from_scratch(
                context.domains(),
                &self.parameters,
                self.inference_code.as_ref().unwrap(),
            )?;

            // Then we note that the time-table is not outdated anymore
            self.is_time_table_outdated = false;

            // And we clear all of the updates since they have now necessarily been processed
            self.updatable_structures
                .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters);

            return Ok(());
        }

        // We keep track whether a conflict was found
        let mut found_conflict = false;

        // Then we go over all of the updated tasks
        while let Some(updated_task) = self.updatable_structures.pop_next_updated_task() {
            let element = self.updatable_structures.get_update_for_task(&updated_task);

            // We get the adjustments based on the stored updated
            let mandatory_part_adjustments = element.get_mandatory_part_adjustments();

            // Then we first remove from the time-table (if necessary)
            //
            // This order ensures that there is less of a chance of incorrect overflows being
            // reported
            self.remove_from_time_table(&mandatory_part_adjustments, &updated_task);

            // Then we add to the time-table (if necessary)
            //
            // Note that the inconsistency returned here does not necessarily hold since other
            // updates could remove from the profile
            let conflicting = self.conflicting_after_addition_to_time_table(
                &mandatory_part_adjustments,
                &updated_task,
            );

            // If we have found an overflow then we mark that we need to check the profile
            found_conflict |= conflicting;

            // Then we reset the update for the task since it has been processed
            self.updatable_structures
                .reset_update_for_task(&updated_task);
        }

        // After all the updates have been processed, we need to check whether there is still a
        // conflict in the time-table (if any calls have reported an overflow)
        if found_conflict || self.found_previous_conflict {
            if SYNCHRONISE {
                // If we are synchronising then we need to search for the conflict which would have
                // been found by the non-incremental propagator
                let conflicting_profile =
                    find_synchronised_conflict(&self.time_table, &self.parameters);
                // Now we need to find the same explanation as would have been found by
                // the non-incremental propagator
                if let Some(mut conflicting_profile) = conflicting_profile {
                    let synchronised_conflict_explanation =
                        create_synchronised_conflict_explanation(
                            context.domains(),
                            self.inference_code.as_ref().unwrap(),
                            &mut conflicting_profile,
                            &self.parameters,
                        );
                    pumpkin_assert_extreme!(
                        check_synchronisation_conflict_explanation_over_interval(
                            &synchronised_conflict_explanation,
                            context.domains(),
                            &self.parameters,
                            self.inference_code.as_ref().unwrap(),
                        ),
                        "The conflict explanation was not the same as the conflict explanation from scratch!"
                    );
                    self.found_previous_conflict = true;
                    return synchronised_conflict_explanation;
                }
                // Otherwise we mark that we have not found the previous conflict and continue
                self.found_previous_conflict = false;
            } else {
                // We linearly scan the profiles and find the first one which exceeds the capacity
                let conflicting_profile = self
                    .time_table
                    .iter_mut()
                    .find(|profile| profile.height > self.parameters.capacity);

                // If we have found such a conflict then we return it
                if let Some(conflicting_profile) = conflicting_profile {
                    pumpkin_assert_extreme!(
                        create_time_table_over_interval_from_scratch(
                            context.domains(),
                            &self.parameters,
                            self.inference_code.as_ref().unwrap(),
                        )
                        .is_err(),
                        "Time-table from scratch could not find conflict"
                    );
                    // We have found the previous conflict
                    self.found_previous_conflict = true;

                    return Err(create_conflict_explanation(
                        context.domains(),
                        self.inference_code.as_ref().unwrap(),
                        conflicting_profile,
                        self.parameters.options.explanation_type,
                        self.parameters.capacity,
                    )
                    .into());
                }
                // Otherwise we mark that we have not found the previous conflict and continue
                self.found_previous_conflict = false;
            }
        }

        if SYNCHRONISE {
            // We have not found a conflict; we need to ensure that the time-tables are the same by
            // ensuring that the profiles are maximal and the profile tasks are sorted in the same
            // order
            synchronise_time_table(&mut self.time_table, context.domains())
        }

        // We check whether there are no non-conflicting profiles in the time-table if we do not
        // report any conflicts
        pumpkin_assert_extreme!(
            self.time_table
                .iter()
                .all(|profile| profile.height <= self.parameters.capacity)
        );
        Ok(())
    }
}

impl<Var: IntegerVariable + 'static, const SYNCHRONISE: bool> Propagator
    for TimeTableOverIntervalIncrementalPropagator<Var, SYNCHRONISE>
{
    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            check_bounds_equal_at_propagation(
                context.domains(),
                &self.parameters.tasks,
                self.updatable_structures.get_stored_bounds(),
            ),
            "Bounds were not equal when propagating"
        );

        if self.parameters.is_infeasible {
            return propagator_conflict(conjunction!(), self.inference_code.as_ref().unwrap());
        }

        self.update_time_table(&mut context)?;

        pumpkin_assert_extreme!(
            debug::time_tables_are_the_same_interval::<Var, SYNCHRONISE>(
                context.domains(),
                self.inference_code.as_ref().unwrap(),
                &self.time_table,
                &self.parameters,
            ),
            "The profiles were not the same between the incremental and the non-incremental version"
        );

        // We pass the entirety of the table to check due to the fact that the propagation of the
        // current profile could lead to the propagation across multiple profiles
        // For example, if we have updated 1 resource profile which caused a propagation then this
        // could cause another propagation by a profile which has not been updated
        propagate_based_on_timetable(
            &mut context,
            self.inference_code.as_ref().unwrap(),
            self.time_table.iter(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn notify(
        &mut self,
        mut context: NotificationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
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
            &self.updatable_structures,
            &updated_task,
            context.domains(),
            &self.parameters,
        );

        // If there is a task which now has a mandatory part then we store it and process it when
        // the `propagate` method is called
        insert_update(&updated_task, &mut self.updatable_structures, result.update);

        update_bounds_task(
            context.domains(),
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if matches!(
            updated_task.start_variable.unpack_event(event),
            DomainEvent::Assign
        ) {
            self.updatable_structures.fix_task(&updated_task)
        }

        result.decision
    }

    fn notify_backtrack(
        &mut self,
        mut context: Domains,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) {
        pumpkin_assert_simple!(self.parameters.options.incremental_backtracking);

        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);

        backtrack_update(
            context.reborrow(),
            &mut self.updatable_structures,
            &updated_task,
        );

        update_bounds_task(
            context,
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if matches!(
            updated_task.start_variable.unpack_event(event),
            DomainEvent::Assign
        ) {
            // The start variable of the task has been unassigned, we should restore it to unfixed
            self.updatable_structures.unfix_task(updated_task);
        }
    }

    fn synchronise(&mut self, mut context: NotificationContext<'_>) {
        // We now recalculate the time-table from scratch if necessary and reset all of the bounds
        // *if* incremental backtracking is disabled
        if !self.parameters.options.incremental_backtracking {
            self.updatable_structures
                .reset_all_bounds_and_remove_fixed(context.domains(), &self.parameters);
            // If the time-table is already empty then backtracking will not cause it to become
            // outdated
            if !self.time_table.is_empty() {
                self.is_time_table_outdated = true;
            }
        } else if SYNCHRONISE {
            self.updatable_structures
                .remove_fixed(context.domains(), &self.parameters);
        }
    }

    fn priority(&self) -> Priority {
        Priority::VeryLow
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverIntervalIncremental"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        // Use the same debug propagator from `TimeTableOverInterval`
        propagate_from_scratch_time_table_interval(
            &mut context,
            &self.parameters,
            &self.updatable_structures,
            self.inference_code.as_ref().unwrap(),
        )
    }
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
fn determine_profiles_to_update<Var: IntegerVariable + 'static>(
    time_table: &OverIntervalTimeTableType<Var>,
    update_range: &Range<i32>,
) -> Result<(usize, usize), usize> {
    let overlapping_profile = find_overlapping_profile(time_table, update_range);

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

/// Performs a binary search on the
/// [time-table][TimeTableOverIntervalIncrementalPropagator::time_table] to find *an* element
/// which overlaps with the `update_range`. If such an element can be found then it returns
/// [Ok] containing the index of the overlapping profile. If no such element could be found,
/// it returns [Err] containing the index at which the element should be inserted to
/// preserve the ordering
fn find_overlapping_profile<Var: IntegerVariable + 'static>(
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
