use crate::engine::cp::propagation::contexts::propagation_context::ReadDomains;
use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;

use super::insertion;
use super::removal;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::conjunction;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::DomainEvent;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::variables::IntegerVariable;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagators::create_time_table_over_interval_from_scratch;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::debug;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::check_synchronisation_conflict_explanation_over_interval;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::create_synchronised_conflict_explanation;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::find_synchronised_conflict;
use crate::propagators::cumulative::time_table::over_interval_incremental_propagator::synchronisation::synchronise_time_table;
use crate::propagators::cumulative::time_table::propagation_handler::create_explanation_profile_height;
use crate::propagators::cumulative::time_table::time_table_util::backtrack_update;
use crate::propagators::cumulative::time_table::time_table_util::has_overlap_with_interval;
use crate::propagators::cumulative::time_table::time_table_util::insert_update;
use crate::propagators::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::propagators::cumulative::time_table::time_table_util::should_enqueue;
use crate::propagators::cumulative::time_table::TimeTable;
use crate::propagators::debug_propagate_from_scratch_time_table_interval;
use crate::propagators::util::check_bounds_equal_at_propagation;
use crate::propagators::util::create_tasks;
use crate::propagators::util::register_tasks;
use crate::propagators::util::update_bounds_task;
use crate::propagators::ArgTask;
use crate::propagators::CumulativeParameters;
use crate::propagators::CumulativePropagatorOptions;
use crate::propagators::MandatoryPartAdjustments;
use crate::propagators::OverIntervalTimeTableType;
use crate::propagators::Task;
#[cfg(doc)]
use crate::propagators::TimeTableOverIntervalPropagator;
#[cfg(doc)]
use crate::propagators::TimeTablePerPointPropagator;
use crate::propagators::UpdatableStructures;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;

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
pub(crate) struct TimeTableOverIntervalIncrementalPropagator<
    Var,
    PVar,
    RVar,
    CVar,
    const SYNCHRONISE: bool,
> {
    /// The key `t` (representing a time-point) holds the mandatory resource consumption of
    /// [`Task`]s at that time (stored in a [`ResourceProfile`]); the [`ResourceProfile`]s are
    /// sorted based on start time and they are assumed to be non-overlapping
    time_table: OverIntervalTimeTableType<Var, PVar, RVar>,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var, PVar, RVar, CVar>,
    /// Stores structures which change during the search; either to store bounds or when applying
    /// incrementality
    updatable_structures: UpdatableStructures<Var, PVar, RVar>,
    /// Stores whether the propagator found a conflict in the previous call
    ///
    /// This is stored to deal with the case where the same conflict can be created via two
    /// distinct propagation chains; to the propagator it appears that nothing has changed (since
    /// the bounds on the variables remain the same) but there is still a conflicting profile in
    /// the time-table
    found_previous_conflict: bool,
    /// Indicates whether the current time-table is outdated and should be recalculated from
    /// scratch or not; note that this variable is only used if
    /// [`CumulativePropagatorOptions::incremental_backtracking`] is set to false.
    is_time_table_outdated: bool,

    inference_code: InferenceCode,
}

pub(crate) struct TimeTableOverIntervalIncrementalConstructor<
    Var,
    PVar,
    RVar,
    CVar,
    const SYNCHRONISE: bool,
> {
    tasks: Vec<ArgTask<Var, PVar, RVar>>,
    capacity: CVar,
    cumulative_options: CumulativePropagatorOptions,
    constraint_tag: ConstraintTag,
}

impl<Var, PVar, RVar, CVar, const SYNCHRONISE: bool>
    TimeTableOverIntervalIncrementalConstructor<Var, PVar, RVar, CVar, SYNCHRONISE>
{
    pub(crate) fn new(
        arg_tasks: Vec<ArgTask<Var, PVar, RVar>>,
        capacity: CVar,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self {
            tasks: arg_tasks,
            capacity,
            cumulative_options,
            constraint_tag,
        }
    }
}

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
    const SYNCHRONISE: bool,
> PropagatorConstructor
    for TimeTableOverIntervalIncrementalConstructor<Var, PVar, RVar, CVar, SYNCHRONISE>
{
    type PropagatorImpl =
        TimeTableOverIntervalIncrementalPropagator<Var, PVar, RVar, CVar, SYNCHRONISE>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let inference_code = context.create_inference_code(self.constraint_tag, TimeTable);

        TimeTableOverIntervalIncrementalPropagator::new(
            context,
            &self.tasks,
            self.capacity,
            self.cumulative_options,
            inference_code,
        )
    }
}

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
    const SYNCHRONISE: bool,
> TimeTableOverIntervalIncrementalPropagator<Var, PVar, RVar, CVar, SYNCHRONISE>
{
    fn new(
        mut context: PropagatorConstructorContext,
        arg_tasks: &[ArgTask<Var, PVar, RVar>],
        capacity: CVar,
        cumulative_options: CumulativePropagatorOptions,
        inference_code: InferenceCode,
    ) -> TimeTableOverIntervalIncrementalPropagator<Var, PVar, RVar, CVar, SYNCHRONISE> {
        let tasks = create_tasks(context.as_readonly(), arg_tasks);

        let parameters =
            CumulativeParameters::new(context.as_readonly(), tasks, capacity, cumulative_options);
        register_tasks(
            &parameters.tasks,
            &parameters.capacity,
            context.reborrow(),
            cumulative_options.incremental_backtracking,
        );

        let mut updatable_structures = UpdatableStructures::new(&parameters);
        updatable_structures.initialise_bounds_and_remove_fixed(context.as_readonly(), &parameters);

        TimeTableOverIntervalIncrementalPropagator {
            time_table: Default::default(),
            parameters,
            updatable_structures,
            found_previous_conflict: false,
            is_time_table_outdated: true,
            inference_code,
        }
    }

    /// Adds the added parts in the provided [`MandatoryPartAdjustments`] to the time-table; note
    /// that all of the adjustments are applied even if a conflict is found.
    fn add_to_time_table(
        &mut self,
        context: PropagationContext,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var, PVar, RVar>>,
    ) -> PropagationStatusCP {
        let mut conflict = None;
        // We consider both of the possible update ranges
        // Note that the upper update range is first considered to avoid any issues with the
        // indices when processing the other update range
        for update_range in mandatory_part_adjustments.get_added_parts() {
            // First we attempt to find overlapping profiles
            match determine_profiles_to_update(&self.time_table, &update_range) {
                Ok((start_index, end_index)) => {
                    let result = insertion::insert_profiles_overlapping_with_added_mandatory_part(
                        context,
                        &mut self.time_table,
                        start_index,
                        end_index,
                        &update_range,
                        task,
                        self.parameters.capacity.clone(),
                    );
                    if let Err(conflict_tasks) = result
                        && conflict.is_none()
                    {
                        conflict = Some(Err(create_explanation_profile_height(
                            context,
                            self.inference_code,
                            &conflict_tasks,
                            self.parameters.options.explanation_type,
                        )
                        .into()));
                    }
                }
                Err(index_to_insert) => insertion::insert_profile_new_mandatory_part(
                    context,
                    &mut self.time_table,
                    index_to_insert,
                    &update_range,
                    task,
                ),
            }
        }
        if let Some(conflict) = conflict {
            conflict
        } else {
            Ok(())
        }
    }

    /// Removes the removed parts in the provided [`MandatoryPartAdjustments`] from the time-table
    fn remove_from_time_table(
        &mut self,
        context: PropagationContext,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var, PVar, RVar>>,
    ) {
        // We consider both of the possible update ranges
        // Note that the upper update range is first considered to avoid any issues with the
        // indices when processing the other update range
        for update_range in mandatory_part_adjustments.get_removed_parts() {
            // First we attempt to find overlapping profiles
            match determine_profiles_to_update(&self.time_table, &update_range) {
                Ok((start_index, end_index)) => {
                    removal::reduce_profiles_overlapping_with_added_mandatory_part(
                        context,
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
    fn update_time_table(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        if self.is_time_table_outdated {
            // We create the time-table from scratch (and return an error if it overflows)
            self.time_table = create_time_table_over_interval_from_scratch(
                context,
                &self.parameters,
                self.inference_code,
            )?;

            // Then we note that the time-table is not outdated anymore
            self.is_time_table_outdated = false;

            // And we clear all of the updates since they have now necessarily been processed
            self.updatable_structures
                .reset_all_bounds_and_remove_fixed(context.as_readonly(), &self.parameters);

            return Ok(());
        }

        // We keep track whether a conflict was found
        let mut found_conflict = false;

        // Then we go over all of the updated tasks
        while let Some(updated_task) = self.updatable_structures.pop_next_updated_task() {
            let element = self.updatable_structures.get_update_for_task(&updated_task);

            // We get the adjustments based on the stored updated
            let mandatory_part_adjustments =
                element.get_mandatory_part_adjustments(context.as_readonly());

            // Then we first remove from the time-table (if necessary)
            //
            // This order ensures that there is less of a chance of incorrect overflows being
            // reported
            self.remove_from_time_table(
                context.as_readonly(),
                &mandatory_part_adjustments,
                &updated_task,
            );

            // Then we add to the time-table (if necessary)
            //
            // Note that the inconsistency returned here does not necessarily hold since other
            // updates could remove from the profile
            let result = self.add_to_time_table(
                context.as_readonly(),
                &mandatory_part_adjustments,
                &updated_task,
            );

            // If we have found an overflow then we mark that we need to check the profile
            found_conflict |= result.is_err();

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
                let conflicting_profile = find_synchronised_conflict(
                    context.as_readonly(),
                    &mut self.time_table,
                    &self.parameters,
                );
                // Now we need to find the same explanation as would have been found by
                // the non-incremental propagator
                if let Some(mut conflicting_profile) = conflicting_profile {
                    let synchronised_conflict_explanation =
                        create_synchronised_conflict_explanation(
                            context.as_readonly(),
                            self.inference_code,
                            &mut conflicting_profile,
                            &self.parameters,
                        );
                    pumpkin_assert_extreme!(
                        check_synchronisation_conflict_explanation_over_interval(
                            &synchronised_conflict_explanation,
                            context,
                            &self.parameters,
                            self.inference_code,
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
                let conflicting_profile = self.time_table.iter_mut().find(|profile| {
                    profile.height > context.upper_bound(&self.parameters.capacity)
                });

                // If we have found such a conflict then we return it
                if let Some(conflicting_profile) = conflicting_profile {
                    pumpkin_assert_extreme!(
                        create_time_table_over_interval_from_scratch(
                            context,
                            &self.parameters,
                            self.inference_code,
                        )
                        .is_err(),
                        "Time-table from scratch could not find conflict"
                    );
                    // We have found the previous conflict
                    self.found_previous_conflict = true;

                    return Err(create_explanation_profile_height(
                        context.as_readonly(),
                        self.inference_code,
                        conflicting_profile,
                        self.parameters.options.explanation_type,
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
            synchronise_time_table(&mut self.time_table, context.as_readonly())
        }

        // We check whether there are no non-conflicting profiles in the time-table if we do not
        // report any conflicts
        pumpkin_assert_extreme!(
            self.time_table
                .iter()
                .all(|profile| profile.height <= context.upper_bound(&self.parameters.capacity))
        );
        Ok(())
    }
}

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
    const SYNCHRONISE: bool,
> Propagator for TimeTableOverIntervalIncrementalPropagator<Var, PVar, RVar, CVar, SYNCHRONISE>
{
    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            check_bounds_equal_at_propagation(
                context.as_readonly(),
                &self.parameters.tasks,
                self.updatable_structures.get_stored_bounds(),
            ),
            "Bounds were not equal when propagating"
        );

        if self.parameters.is_infeasible {
            return Err(Inconsistency::Conflict(PropagatorConflict {
                conjunction: conjunction!(),
                inference_code: self.inference_code,
            }));
        }

        self.update_time_table(&mut context)?;

        pumpkin_assert_extreme!(
            debug::time_tables_are_the_same_interval::<Var, PVar, RVar, CVar, SYNCHRONISE>(
                &mut context,
                self.inference_code,
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
            self.inference_code,
            self.time_table.iter(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn notify(
        &mut self,
        context: PropagationContextWithTrailedValues,
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
            &self.updatable_structures,
            &updated_task,
            context.as_readonly(),
            self.time_table.is_empty(),
        );

        // If there is a task which now has a mandatory part then we store it and process it when
        // the `propagate` method is called
        insert_update(&updated_task, &mut self.updatable_structures, result.update);

        update_bounds_task(
            context.as_readonly(),
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if context.is_fixed(&updated_task.start_variable)
            && context.is_fixed(&updated_task.processing_time)
            && context.is_fixed(&updated_task.resource_usage)
        {
            self.updatable_structures.fix_task(&updated_task)
        }

        result.decision
    }

    fn notify_backtrack(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) {
        pumpkin_assert_simple!(self.parameters.options.incremental_backtracking);

        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);

        backtrack_update(context, &mut self.updatable_structures, &updated_task);

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

    fn synchronise(&mut self, context: PropagationContext) {
        // We now recalculate the time-table from scratch if necessary and reset all of the bounds
        // *if* incremental backtracking is disabled
        if !self.parameters.options.incremental_backtracking {
            self.updatable_structures
                .reset_all_bounds_and_remove_fixed(context, &self.parameters);
            // If the time-table is already empty then backtracking will not cause it to become
            // outdated
            if !self.time_table.is_empty() {
                self.is_time_table_outdated = true;
            }
        } else if SYNCHRONISE {
            self.updatable_structures
                .remove_fixed(context, &self.parameters);
        }
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTableOverIntervalIncremental"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        // Use the same debug propagator from `TimeTableOverInterval`
        debug_propagate_from_scratch_time_table_interval(
            &mut context,
            &self.parameters,
            &self.updatable_structures,
            self.inference_code,
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
fn determine_profiles_to_update<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    time_table: &OverIntervalTimeTableType<Var, PVar, RVar>,
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
fn find_overlapping_profile<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
>(
    time_table: &OverIntervalTimeTableType<Var, PVar, RVar>,
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

#[cfg(test)]
mod tests {
    use crate::conjunction;
    use crate::constraint_arguments::CumulativeExplanationType;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::propagators::ArgTask;
    use crate::propagators::CumulativePropagatorOptions;
    use crate::propagators::TimeTableOverIntervalIncrementalConstructor;
    use crate::variables::DomainId;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
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
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(TimeTableOverIntervalIncrementalConstructor::<
            DomainId,
            i32,
            i32,
            i32,
            false,
        >::new(
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
            .collect::<Vec<_>>(),
            1,
            CumulativePropagatorOptions {
                explanation_type: CumulativeExplanationType::Naive,
                ..Default::default()
            },
            constraint_tag,
        ));

        assert!(result.is_err());
        let reason = solver.get_reason_int(Predicate::trivially_false());
        let expected = [
            predicate!(s1 <= 1),
            predicate!(s1 >= 1),
            predicate!(s2 >= 1),
            predicate!(s2 <= 1),
        ];
        assert!(
            expected
                .iter()
                .all(|y| { reason.iter().collect::<Vec<&Predicate>>().contains(&y) })
                && reason.iter().all(|y| expected.contains(y))
        );
    }

    #[test]
    fn propagator_propagates_nothing() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(0, 6);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
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
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        let notification_status = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        let result = solver.propagate(propagator);
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
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason_int(predicate!(s2 <= 3));
        assert_eq!(conjunction!([s2 <= 8] & [s1 >= 6] & [s1 <= 6]), reason);
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
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
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

        let notification_status = solver.increase_lower_bound_and_notify(propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(propagator);
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
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
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

        let notification_status = solver.increase_lower_bound_and_notify(propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason_int(predicate!(s2 >= 5));
        assert_eq!(conjunction!([s2 >= 1] & [s1 >= 1] & [s1 <= 1]), reason);
    }

    #[test]
    fn propagator_propagates_generic_bounds() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(3, 3);
        let s2 = solver.new_variable(5, 5);
        let s3 = solver.new_variable(1, 15);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason_int(predicate!(s3 >= 7));
        assert_eq!(conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 5]), reason);
    }

    #[test]
    fn propagator_propagates_with_holes() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(4, 4);
        let s2 = solver.new_variable(0, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTableOverIntervalIncrementalConstructor::<
                DomainId,
                i32,
                i32,
                i32,
                false,
            >::new(
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    allow_holes_in_domain: true,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 4);
        assert_eq!(solver.upper_bound(s1), 4);

        for removed in 2..8 {
            assert!(!solver.contains(s2, removed));
            let reason = solver.get_reason_int(predicate!(s2 != removed));
            assert_eq!(conjunction!([s1 <= 4] & [s1 >= 4]), reason);
        }
    }
}
