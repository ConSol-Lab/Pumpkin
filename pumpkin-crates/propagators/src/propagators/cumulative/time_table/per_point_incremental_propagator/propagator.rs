use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt::Debug;
use std::rc::Rc;

use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::asserts::pumpkin_assert_extreme;
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

use crate::cumulative::ArgTask;
use crate::cumulative::CumulativeParameters;
use crate::cumulative::MandatoryPartAdjustments;
use crate::cumulative::ResourceProfile;
use crate::cumulative::Task;
use crate::cumulative::UpdatableStructures;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::PerPointTimeTableType;
#[cfg(doc)]
use crate::cumulative::time_table::TimeTablePerPointPropagator;
use crate::cumulative::time_table::create_time_table_per_point_from_scratch;
use crate::cumulative::time_table::propagate_from_scratch_time_table_point;
use crate::cumulative::util::check_bounds_equal_at_propagation;
use crate::cumulative::util::create_tasks;
use crate::cumulative::util::update_bounds_task;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::check_synchronisation_conflict_explanation_per_point;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::create_synchronised_conflict_explanation;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::find_synchronised_conflict;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::synchronise_time_table;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::cumulative::time_table::time_table_util::backtrack_update;
use crate::propagators::cumulative::time_table::time_table_util::insert_update;
use crate::propagators::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::propagators::cumulative::time_table::time_table_util::should_enqueue;

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
#[derive(Debug, Clone)]
pub struct TimeTablePerPointIncrementalPropagator<Var, const SYNCHRONISE: bool> {
    /// The key `t` (representing a time-point) holds the mandatory resource consumption of
    /// [`Task`]s at that time (stored in a [`ResourceProfile`]); the [`ResourceProfile`]s are
    /// sorted based on start time and they are assumed to be non-overlapping
    pub(super) time_table: PerPointTimeTableType<Var>,
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

    // TODO: This should be refactored to use a separate propagator constructor, but that is a lot
    // of work in this module and I don't know enough about it to not break it.
    pub(super) constraint_tag: ConstraintTag,
    pub(super) inference_code: Option<InferenceCode>,
}

impl<Var: IntegerVariable + 'static + Debug, const SYNCHRONISE: bool>
    TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE>
{
    pub fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE> {
        let tasks = create_tasks(arg_tasks);
        let parameters = CumulativeParameters::new(tasks, capacity, cumulative_options);
        let updatable_structures = UpdatableStructures::new(&parameters);
        TimeTablePerPointIncrementalPropagator {
            time_table: BTreeMap::new(),
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
    /// Returns true if the addition of the mandatory parts caused an overflow.
    fn conflicting_after_addition_to_time_table(
        &mut self,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var>>,
    ) -> bool {
        // Go over all of the updated tasks and calculate the added mandatory part (we know
        // that for each of these tasks, a mandatory part exists, otherwise it would not
        // have been added (see [`should_propagate`]))
        let mut conflict = false;

        for time_point in mandatory_part_adjustments.get_added_parts().flatten() {
            pumpkin_assert_extreme!(
                !self.time_table.contains_key(&(time_point as u32))
                    || !self
                        .time_table
                        .get(&(time_point as u32))
                        .unwrap()
                        .profile_tasks
                        .iter()
                        .any(|profile_task| profile_task.id.unpack() as usize
                            == task.id.unpack() as usize),
                "Attempted to insert mandatory part where it already exists at time point {time_point} for task {} in time-table per time-point propagator\n",
                task.id.unpack() as usize
            );

            // Add the updated profile to the ResourceProfile at time t
            let current_profile: &mut ResourceProfile<Var> = self
                .time_table
                .entry(time_point as u32)
                .or_insert(ResourceProfile::default(time_point));

            current_profile.height += task.resource_usage;
            current_profile.profile_tasks.push(Rc::clone(task));

            conflict |= current_profile.height > self.parameters.capacity;
        }

        conflict
    }

    /// Removes the removed parts in the provided [`MandatoryPartAdjustments`] from the time-table
    fn remove_from_time_table(
        &mut self,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var>>,
    ) {
        for time_point in mandatory_part_adjustments.get_removed_parts().flatten() {
            pumpkin_assert_extreme!(
                self.time_table.contains_key(&(time_point as u32))
                    && self
                        .time_table
                        .get(&(time_point as u32))
                        .unwrap()
                        .profile_tasks
                        .iter()
                        .any(|profile_task| profile_task.id.unpack() as usize
                            == task.id.unpack() as usize),
                "Attempted to remove mandatory part where it didn't exist at time point {time_point} for task {} in time-table per time-point propagator",
                task.id.unpack() as usize
            );

            // Then we update the time-table
            if let Entry::Occupied(entry) =
                self.time_table
                    .entry(time_point as u32)
                    .and_modify(|profile| {
                        // We remove the resource usage of the task from the height of the profile
                        profile.height -= task.resource_usage;

                        // If the height of the profile is not equal to 0 then we remove the task
                        // from the profile tasks
                        if profile.height != 0 {
                            let _ = profile.profile_tasks.remove(
                                profile
                                    .profile_tasks
                                    .iter()
                                    .position(|profile_task| profile_task.id == task.id)
                                    .expect("Task should be present"),
                            );
                        }
                    })
            {
                if entry.get().height == 0 {
                    // If the height of the profile is now 0 then we remove the entry
                    let _ = entry.remove();
                }
            } else {
                panic!("Entry for time-point did not exist when removing from time-table")
            }
        }
    }

    /// Updates the stored time-table based on the updates stored in
    /// [`DynamicStructures::updated`].
    ///
    /// An error is returned if an overflow of the resource occurs while updating the time-table.
    fn update_time_table(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        if self.is_time_table_outdated {
            // We create the time-table from scratch (and return an error if it overflows)
            self.time_table = create_time_table_per_point_from_scratch(
                context.domains(),
                self.inference_code.as_ref().unwrap(),
                &self.parameters,
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
                let synchronised_conflict =
                    find_synchronised_conflict(&mut self.time_table, &self.parameters);

                // After finding the profile which would have been found by the non-incremental
                // propagator, we also need to find the profile explanation which would have been
                // found by the non-incremental propagator
                if let Some(conflicting_time_point) = synchronised_conflict {
                    let conflicting_profile = self
                        .time_table
                        .get_mut(&conflicting_time_point)
                        .expect("Expected to find a conflicting profile");
                    let synchronised_conflict_explanation =
                        create_synchronised_conflict_explanation(
                            context.domains(),
                            self.inference_code.as_ref().unwrap(),
                            conflicting_profile,
                            &self.parameters,
                        );

                    pumpkin_assert_extreme!(
                        check_synchronisation_conflict_explanation_per_point(
                            &synchronised_conflict_explanation,
                            context.domains(),
                            self.inference_code.as_ref().unwrap(),
                            &self.parameters,
                        ),
                        "The conflict explanation was not the same as the conflict explanation from scratch!"
                    );

                    // We have found the previous conflict
                    self.found_previous_conflict = true;

                    return synchronised_conflict_explanation;
                }

                // Otherwise we mark that we have not found the previous conflict and continue
                self.found_previous_conflict = false;
            } else {
                // We linearly scan the profiles and find the first one which exceeds the capacity
                let conflicting_profile = self
                    .time_table
                    .values_mut()
                    .find(|profile| profile.height > self.parameters.capacity);

                // If we have found such a conflict then we return it
                if let Some(conflicting_profile) = conflicting_profile {
                    pumpkin_assert_extreme!(
                        create_time_table_per_point_from_scratch(
                            context.domains(),
                            self.inference_code.as_ref().unwrap(),
                            &self.parameters
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
            // ensuring that the profile tasks are sorted in the same order
            synchronise_time_table(self.time_table.values_mut());
        }

        // We check whether there are no non-conflicting profiles in the time-table if we do not
        // report any conflicts
        pumpkin_assert_extreme!(
            self.time_table
                .values()
                .all(|profile| profile.height <= self.parameters.capacity)
        );
        Ok(())
    }
}

impl<Var: IntegerVariable + 'static + Debug, const SYNCHRONISE: bool> Propagator
    for TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE>
{
    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            check_bounds_equal_at_propagation(
                context.domains(),
                &self.parameters.tasks,
                self.updatable_structures.get_stored_bounds(),
            ),
            "Bound were not equal when propagating"
        );

        if self.parameters.is_infeasible {
            return propagator_conflict(conjunction!(), self.inference_code.as_ref().unwrap());
        }

        // We update the time-table based on the stored updates
        self.update_time_table(&mut context)?;

        pumpkin_assert_extreme!(debug::time_tables_are_the_same_point::<Var, SYNCHRONISE>(
            context.domains(),
            self.inference_code.as_ref().unwrap(),
            &self.time_table,
            &self.parameters
        ));

        // We pass the entirety of the table to check due to the fact that the propagation of the
        // current profile could lead to the propagation across multiple profiles
        // For example, if we have updated 1 ResourceProfile which caused a propagation then this
        // could cause another propagation by a profile which has not been updated
        propagate_based_on_timetable(
            &mut context,
            self.inference_code.as_ref().unwrap(),
            self.time_table.values(),
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
            self.updatable_structures.fix_task(&updated_task);
        }

        result.decision
    }

    fn notify_backtrack(
        &mut self,
        mut context: Domains,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) {
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
            self.updatable_structures.unfix_task(updated_task)
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
        "CumulativeTimeTablePerPointIncremental"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        // Use the same debug propagator from `TimeTablePerPoint`
        propagate_from_scratch_time_table_point(
            &mut context,
            self.inference_code.as_ref().unwrap(),
            &self.parameters,
            &self.updatable_structures,
        )
    }
}

/// Contains functions related to debugging
mod debug {
    use pumpkin_core::proof::InferenceCode;
    use pumpkin_core::propagation::Domains;
    use pumpkin_core::variables::IntegerVariable;

    use crate::cumulative::CumulativeParameters;
    use crate::cumulative::time_table::PerPointTimeTableType;
    use crate::cumulative::time_table::create_time_table_per_point_from_scratch;

    /// Determines whether the provided `time_table` is the same as the one creatd from scratch
    /// using the following checks:
    /// - The time-tables should contain the same number of profiles
    /// - For each profile it should hold that
    ///      - The start times are the same
    ///      - The end times are the same
    ///      - The heights are the same
    ///      - The profile tasks should be the same; note that we do not check whether the order is
    ///        the same!
    pub(crate) fn time_tables_are_the_same_point<
        Var: IntegerVariable + 'static,
        const SYNCHRONISE: bool,
    >(
        context: Domains,
        inference_code: &InferenceCode,
        time_table: &PerPointTimeTableType<Var>,
        parameters: &CumulativeParameters<Var>,
    ) -> bool {
        let time_table_scratch =
            create_time_table_per_point_from_scratch(context, inference_code, parameters)
                .expect("Expected no error");

        if time_table.is_empty() {
            return time_table_scratch.is_empty();
        }

        // First we merge all of the split profiles to ensure that it is the same as the
        // non-incremental time-table
        let time_table = time_table.clone();

        // Then we compare whether the time-tables are the same with the following checks:
        // - The time-tables should contain the same number of profiles
        // - For each profile it should hold that
        //      - The starts are the same
        //      - The ends are the same
        //      - The heights are the same
        //      - The profile tasks of the profiles should be the same; note that we do not check
        //        whether the order is the same!
        time_table.len() == time_table_scratch.len()
            && time_table
                .values()
                .zip(time_table_scratch.values())
                .all(|(actual, expected)| {
                    actual.height == expected.height
                        && actual.start == expected.start
                        && actual.end == expected.end
                        && actual.profile_tasks.len() == expected.profile_tasks.len()
                        && if SYNCHRONISE {
                            actual.profile_tasks == expected.profile_tasks
                        } else {
                            actual
                                .profile_tasks
                                .iter()
                                .all(|task| expected.profile_tasks.contains(task))
                        }
                })
    }
}
