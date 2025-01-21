use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::rc::Rc;

use crate::basic_types::PropagationStatusCP;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::propagation::contexts::StatefulPropagationContext;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::variables::IntegerVariable;
use crate::engine::IntDomainEvent;
use crate::predicates::PropositionalConjunction;
use crate::propagators::create_time_table_per_point_from_scratch;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::check_synchronisation_conflict_explanation_per_point;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::create_synchronised_conflict_explanation;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::find_synchronised_conflict;
use crate::propagators::cumulative::time_table::per_point_incremental_propagator::synchronisation::synchronise_time_table;
use crate::propagators::cumulative::time_table::propagation_handler::create_conflict_explanation;
use crate::propagators::cumulative::time_table::time_table_util::backtrack_update;
use crate::propagators::cumulative::time_table::time_table_util::insert_update;
use crate::propagators::cumulative::time_table::time_table_util::propagate_based_on_timetable;
use crate::propagators::cumulative::time_table::time_table_util::should_enqueue;
use crate::propagators::debug_propagate_from_scratch_time_table_point;
use crate::propagators::util::check_bounds_equal_at_propagation;
use crate::propagators::util::create_tasks;
use crate::propagators::util::register_tasks;
use crate::propagators::util::update_bounds_task;
use crate::propagators::ArgTask;
use crate::propagators::CumulativeParameters;
use crate::propagators::CumulativePropagatorOptions;
use crate::propagators::MandatoryPartAdjustments;
use crate::propagators::PerPointTimeTableType;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
#[cfg(doc)]
use crate::propagators::TimeTablePerPointPropagator;
use crate::propagators::UpdatableStructures;
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

pub(crate) struct TimeTablePerPointIncrementalPropagator<Var, const SYNCHRONISE: bool> {
    /// The key `t` (representing a time-point) holds the mandatory resource consumption of
    /// [`Task`]s at that time (stored in a [`ResourceProfile`]); the [`ResourceProfile`]s are
    /// sorted based on start time and they are assumed to be non-overlapping
    time_table: PerPointTimeTableType<Var>,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var>,
    /// Stores structures which change during the search; either to store bounds or when applying
    /// incrementality
    updatable_structures: UpdatableStructures<Var>,
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
}

impl<Var: IntegerVariable + 'static + Debug, const SYNCHRONISE: bool>
    TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE>
{
    pub(crate) fn new(
        arg_tasks: &[ArgTask<Var>],
        capacity: i32,
        cumulative_options: CumulativePropagatorOptions,
    ) -> TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE> {
        let (tasks, mapping) = create_tasks(arg_tasks);
        let parameters = CumulativeParameters::new(tasks, capacity, cumulative_options, mapping);
        let updatable_structures = UpdatableStructures::new(&parameters);
        TimeTablePerPointIncrementalPropagator {
            time_table: BTreeMap::new(),
            parameters,
            updatable_structures,
            found_previous_conflict: false,
            is_time_table_outdated: false,
        }
    }

    /// Adds the added parts in the provided [`MandatoryPartAdjustments`] to the time-table; note
    /// that all of the adjustments are applied even if a conflict is found.
    fn add_to_time_table(
        &mut self,
        context: PropagationContext,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var>>,
    ) -> PropagationStatusCP {
        // Go over all of the updated tasks and calculate the added mandatory part (we know
        // that for each of these tasks, a mandatory part exists, otherwise it would not
        // have been added (see [`should_propagate`]))
        let mut conflict = None;

        for time_point in mandatory_part_adjustments.get_added_parts().flatten() {
            pumpkin_assert_extreme!(
                        !self.time_table.contains_key(&(time_point as u32))
                        || !self.time_table.get(&(time_point as u32)).unwrap().profile_tasks.iter().any(|profile_task| profile_task.id.unpack() as usize == task.id.unpack() as usize),
                        "Attempted to insert mandatory part where it already exists at time point {time_point} for task {} in time-table per time-point propagator\n", task.id.unpack() as usize);

            // Add the updated profile to the ResourceProfile at time t
            let current_profile: &mut ResourceProfile<Var> = self
                .time_table
                .entry(time_point as u32)
                .or_insert(ResourceProfile::default(time_point));

            current_profile.height += task.resource_usage;
            current_profile.profile_tasks.push(Rc::clone(task));

            if current_profile.height > self.parameters.capacity && conflict.is_none() {
                // The newly introduced mandatory part(s) caused an overflow of the resource
                conflict = Some(Err(create_conflict_explanation(
                    context,
                    current_profile,
                    self.parameters.options.explanation_type,
                )
                .into()));
            }
        }

        // If we have found a conflict then we report it
        if let Some(conflict) = conflict {
            conflict
        } else {
            Ok(())
        }
    }

    /// Removes the removed parts in the provided [`MandatoryPartAdjustments`] from the time-table
    fn remove_from_time_table(
        &mut self,
        mandatory_part_adjustments: &MandatoryPartAdjustments,
        task: &Rc<Task<Var>>,
    ) {
        for time_point in mandatory_part_adjustments.get_removed_parts().flatten() {
            pumpkin_assert_extreme!(
                        self.time_table.contains_key(&(time_point as u32)) && self.time_table.get(&(time_point as u32)).unwrap().profile_tasks.iter().any(|profile_task| profile_task.id.unpack() as usize == task.id.unpack() as usize) ,
                        "Attempted to remove mandatory part where it didn't exist at time point {time_point} for task {} in time-table per time-point propagator", task.id.unpack() as usize);

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
    fn update_time_table(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        if self.is_time_table_outdated {
            // We create the time-table from scratch (and return an error if it overflows)
            self.time_table =
                create_time_table_per_point_from_scratch(context.as_readonly(), &self.parameters)?;

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
                            context.as_readonly(),
                            conflicting_profile,
                            &self.parameters,
                        );

                    pumpkin_assert_extreme!(
                        check_synchronisation_conflict_explanation_per_point(
                            &synchronised_conflict_explanation,
                            context.as_readonly(),
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
                            context.as_readonly(),
                            &self.parameters
                        )
                        .is_err(),
                        "Time-table from scratch could not find conflict"
                    );
                    // We have found the previous conflict
                    self.found_previous_conflict = true;

                    return Err(create_conflict_explanation(
                        context.as_readonly(),
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
            // ensuring that the profile tasks are sorted in the same order
            synchronise_time_table(self.time_table.values_mut());
        }

        // We check whether there are no non-conflicting profiles in the time-table if we do not
        // report any conflicts
        pumpkin_assert_extreme!(self
            .time_table
            .values()
            .all(|profile| profile.height <= self.parameters.capacity));
        Ok(())
    }
}

impl<Var: IntegerVariable + 'static + Debug, const SYNCHRONISE: bool> Propagator
    for TimeTablePerPointIncrementalPropagator<Var, SYNCHRONISE>
{
    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        pumpkin_assert_advanced!(
            check_bounds_equal_at_propagation(
                context.as_readonly(),
                &self.parameters.tasks,
                self.updatable_structures.get_stored_bounds(),
            ),
            "Bound were not equal when propagating"
        );

        // We update the time-table based on the stored updates
        self.update_time_table(&mut context)?;

        pumpkin_assert_extreme!(debug::time_tables_are_the_same_point::<Var, SYNCHRONISE>(
            context.as_readonly(),
            &self.time_table,
            &self.parameters
        ));

        // We pass the entirety of the table to check due to the fact that the propagation of the
        // current profile could lead to the propagation across multiple profiles
        // For example, if we have updated 1 ResourceProfile which caused a propagation then this
        // could cause another propagation by a profile which has not been updated
        propagate_based_on_timetable(
            &mut context,
            self.time_table.values(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn notify(
        &mut self,
        context: StatefulPropagationContext,
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

        if matches!(
            updated_task.start_variable.unpack_event(event),
            IntDomainEvent::Assign
        ) {
            self.updatable_structures.fix_task(&updated_task);
        }

        result.decision
    }

    fn notify_backtrack(
        &mut self,
        context: PropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) {
        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);

        backtrack_update(context, &mut self.updatable_structures, &updated_task);

        update_bounds_task(
            context,
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if matches!(
            updated_task.start_variable.unpack_event(event),
            IntDomainEvent::Assign
        ) {
            // The start variable of the task has been unassigned, we should restore it to unfixed
            self.updatable_structures.unfix_task(updated_task)
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
        "CumulativeTimeTablePerPointIncremental"
    }

    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        register_tasks(&self.parameters.tasks, context, true);
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context.as_readonly(), &self.parameters);

        // Then we do normal propagation
        self.time_table =
            create_time_table_per_point_from_scratch(context.as_readonly(), &self.parameters)?;
        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        // Use the same debug propagator from `TimeTablePerPoint`
        debug_propagate_from_scratch_time_table_point(
            &mut context,
            &self.parameters,
            &self.updatable_structures,
        )
    }
}

/// Contains functions related to debugging
mod debug {

    use crate::engine::propagation::PropagationContext;
    use crate::propagators::create_time_table_per_point_from_scratch;
    use crate::propagators::CumulativeParameters;
    use crate::propagators::PerPointTimeTableType;
    use crate::variables::IntegerVariable;

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
        context: PropagationContext,
        time_table: &PerPointTimeTableType<Var>,
        parameters: &CumulativeParameters<Var>,
    ) -> bool {
        let time_table_scratch = create_time_table_per_point_from_scratch(context, parameters)
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

#[cfg(test)]
mod tests {
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_solver::TestSolver;
    use crate::options::CumulativeExplanationType;
    use crate::predicate;
    use crate::predicates::PredicateConstructor;
    use crate::propagators::ArgTask;
    use crate::propagators::CumulativePropagatorOptions;
    use crate::propagators::TimeTablePerPointIncrementalPropagator;
    use crate::propagators::TimeTablePerPointPropagator;
    use crate::variables::DomainId;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let _ = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
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

        let result = solver.new_propagator(
            TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                &[
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
            ),
        );
        assert!(match result {
            Err(Inconsistency::Conflict(x)) => {
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
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
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
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);

        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
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

        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
            .expect("No conflict");
        let result = solver.propagate_until_fixed_point(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason_int(predicate!(s2 <= 3));
        assert_eq!(conjunction!([s2 <= 5] & [s1 >= 6] & [s1 <= 6]), reason);
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

        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
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

        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
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

        let _ = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason_int(predicate!(s2 >= 5));
        assert_eq!(
            conjunction!([s2 >= 4] & [s1 >= 1] & [s1 <= 1]), /* Note that this not
                                                              * the most general
                                                              * explanation, if s2
                                                              * could have started at
                                                              * 0 then it would still
                                                              * have
                                                              * overlapped with the
                                                              * current interval */
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
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason_int(predicate!(s3 >= 7));
        assert_eq!(
            conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 6]), /* Note that s3 would
                                                              * have been able to
                                                              * propagate
                                                              * this bound even if it
                                                              * started at time 0 */
            reason
        );
    }

    #[test]
    fn propagator_propagates_with_holes() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(4, 4);
        let s2 = solver.new_variable(0, 8);

        let _ = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
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
                ),
            )
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

    #[test]
    fn synchronisation_leads_to_same_conflict_explanation() {
        let mut solver_scratch = TestSolver::default();
        let s1_scratch = solver_scratch.new_variable(5, 5);
        let s2_scratch = solver_scratch.new_variable(1, 10);
        let s3_scratch = solver_scratch.new_variable(1, 10);
        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3_scratch,
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
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 7);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s3_scratch, 7);
        let result_scratch = solver_scratch.propagate(propagator_scratch);

        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(5, 5);
        let s2 = solver.new_variable(1, 10);
        let s3 = solver.new_variable(1, 10);
        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                    &[
                        ArgTask {
                            start_time: s1,
                            processing_time: 2,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 4,
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
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 2, s3, 7);
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 7);
        let result = solver.propagate(propagator);
        assert!({
            let same = if let Err(Inconsistency::Conflict (explanation
            )) =
                &result
            {
                if let Err(Inconsistency::Conflict (explanation_scratch)) =
                    &result_scratch
                {
                    explanation.iter().collect::<Vec<_>>()
                        == explanation_scratch.iter().collect::<Vec<_>>()
                } else {
                    false
                }
            } else {
                false
            };
            same
        }, "The results are different than expected - Expected: {result_scratch:?} but was: {result:?}");
    }

    #[test]
    fn synchronisation_leads_to_same_conflict_after_propagating() {
        let mut solver_scratch = TestSolver::default();
        let s1_scratch = solver_scratch.new_variable(5, 5);
        let s2_scratch = solver_scratch.new_variable(1, 10);
        let s3_scratch = solver_scratch.new_variable(1, 10);
        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3_scratch,
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
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s3_scratch, 7);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 7);

        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(5, 5);
        let s2 = solver.new_variable(1, 10);
        let s3 = solver.new_variable(1, 10);
        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                    &[
                        ArgTask {
                            start_time: s1,
                            processing_time: 2,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 4,
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
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 2, s3, 7);
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 7);
        let result = solver.propagate(propagator);
        assert!(result.is_err());
        let result_scratch = solver_scratch.propagate(propagator_scratch);
        assert!(result_scratch.is_err());
        assert!({
            let same = if let Err(Inconsistency::Conflict(explanation)) = &result {
                if let Err(Inconsistency::Conflict(explanation_scratch)) = &result_scratch {
                    explanation.iter().collect::<Vec<_>>()
                        == explanation_scratch.iter().collect::<Vec<_>>()
                } else {
                    false
                }
            } else {
                false
            };
            same
        });
    }

    #[test]
    fn no_synchronisation_leads_to_different_conflict_explanation() {
        let mut solver_scratch = TestSolver::default();
        let s1_scratch = solver_scratch.new_variable(5, 5);
        let s2_scratch = solver_scratch.new_variable(1, 10);
        let s3_scratch = solver_scratch.new_variable(1, 10);
        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3_scratch,
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
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s3_scratch, 7);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 7);

        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(5, 5);
        let s2 = solver.new_variable(1, 10);
        let s3 = solver.new_variable(1, 10);
        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
                        ArgTask {
                            start_time: s1,
                            processing_time: 2,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 4,
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
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 2, s3, 7);
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 7);
        let result = solver.propagate(propagator);
        let result_scratch = solver_scratch.propagate(propagator_scratch);
        assert!({
            let same = if let Err(Inconsistency::Conflict(explanation)) = &result {
                if let Err(Inconsistency::Conflict(explanation_scratch)) = &result_scratch {
                    explanation.iter().collect::<Vec<_>>()
                        != explanation_scratch.iter().collect::<Vec<_>>()
                } else {
                    false
                }
            } else {
                false
            };
            same
        });
    }

    #[test]
    fn synchronisation_leads_to_same_explanation() {
        let mut solver_scratch = TestSolver::default();
        let s1_scratch = solver_scratch.new_variable(1, 6);
        let s2_scratch = solver_scratch.new_variable(1, 6);
        let s3_scratch = solver_scratch.new_variable(5, 11);
        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                2,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 5);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s1_scratch, 5);

        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 6);
        let s2 = solver.new_variable(1, 6);
        let s3 = solver.new_variable(5, 11);
        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                    &[
                        ArgTask {
                            start_time: s1,
                            processing_time: 2,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 4,
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
                    2,
                    CumulativePropagatorOptions {
                        explanation_type: CumulativeExplanationType::Naive,
                        ..Default::default()
                    },
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 5);
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        let _ = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
        let result_scratch = solver_scratch.propagate(propagator_scratch);
        assert!(result_scratch.is_ok());
        assert_eq!(solver_scratch.lower_bound(s3_scratch), 7);
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s3), 7);
        let reason_scratch = solver_scratch.get_reason_int(s3_scratch.lower_bound_predicate(7));
        let reason = solver.get_reason_int(s3.lower_bound_predicate(7));
        assert_eq!(
            reason_scratch.iter().collect::<Vec<_>>(),
            reason.iter().collect::<Vec<_>>()
        );
    }
    #[test]
    fn no_synchronisation_leads_to_different_explanation() {
        let mut solver_scratch = TestSolver::default();
        let s1_scratch = solver_scratch.new_variable(1, 6);
        let s2_scratch = solver_scratch.new_variable(1, 6);
        let s3_scratch = solver_scratch.new_variable(5, 11);
        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
                2,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s2_scratch, 5);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s1_scratch, 5);
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 6);
        let s2 = solver.new_variable(1, 6);
        let s3 = solver.new_variable(5, 11);
        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
                        ArgTask {
                            start_time: s1,
                            processing_time: 2,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 4,
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
                    2,
                    CumulativePropagatorOptions {
                        explanation_type: CumulativeExplanationType::Naive,
                        ..Default::default()
                    },
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s2, 5);
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        let _ = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);

        let result_scratch = solver_scratch.propagate(propagator_scratch);
        assert!(result_scratch.is_ok());
        assert_eq!(solver_scratch.lower_bound(s3_scratch), 7);

        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s3), 7);

        let reason_scratch = solver_scratch.get_reason_int(s3_scratch.lower_bound_predicate(7));
        let reason = solver.get_reason_int(s3.lower_bound_predicate(7));
        assert_ne!(
            reason_scratch.iter().collect::<Vec<_>>(),
            reason.iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn no_synchronisation_leads_to_different_conflict() {
        let mut solver_scratch = TestSolver::default();
        let s0_scratch = solver_scratch.new_variable(1, 11);
        let s1_scratch = solver_scratch.new_variable(1, 5);
        let s2_scratch = solver_scratch.new_variable(1, 5);

        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s0_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 1,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 1,
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
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s2_scratch, 5);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s1_scratch, 5);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s0_scratch, 5);
        let _ =
            solver_scratch.decrease_upper_bound_and_notify(propagator_scratch, 0, s0_scratch, 5);

        let mut solver = TestSolver::default();
        let s0 = solver.new_variable(1, 11);
        let s1 = solver.new_variable(1, 5);
        let s2 = solver.new_variable(1, 5);

        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, false>::new(
                    &[
                        ArgTask {
                            start_time: s0,
                            processing_time: 4,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s1,
                            processing_time: 1,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 1,
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
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 2, s2, 5);
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s1, 5);
        let _ = solver.increase_lower_bound_and_notify(propagator, 0, s0, 5);
        let _ = solver.decrease_upper_bound_and_notify(propagator, 0, s0, 5);

        let result_scratch = solver_scratch.propagate(propagator_scratch);
        assert!(result_scratch.is_err());
        let result = solver.propagate(propagator);
        assert!(result.is_err());
        if let (
            Err(Inconsistency::Conflict(explanation)),
            Err(Inconsistency::Conflict(explanation_scratch)),
        ) = (result, result_scratch)
        {
            assert_ne!(explanation, explanation_scratch);
            let explanation_vec = explanation.iter().cloned().collect::<Vec<_>>();
            let explanation_scratch_vec = explanation_scratch.iter().cloned().collect::<Vec<_>>();

            println!("{explanation_vec:?}");
            println!("{explanation_scratch_vec:?}");

            assert!(explanation_vec.contains(&s2.lower_bound_predicate(5)));
            assert!(!explanation_scratch_vec.contains(&s2.lower_bound_predicate(5)));
        } else {
            panic!("Incorrect result")
        }
    }

    #[test]
    fn synchronisation_leads_to_same_conflict() {
        let mut solver_scratch = TestSolver::default();
        let s0_scratch = solver_scratch.new_variable(1, 11);
        let s1_scratch = solver_scratch.new_variable(1, 5);
        let s2_scratch = solver_scratch.new_variable(1, 5);

        let propagator_scratch = solver_scratch
            .new_propagator(TimeTablePerPointPropagator::new(
                &[
                    ArgTask {
                        start_time: s0_scratch,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s1_scratch,
                        processing_time: 1,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2_scratch,
                        processing_time: 1,
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
            ))
            .expect("No conflict");
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 2, s2_scratch, 5);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 1, s1_scratch, 5);
        let _ =
            solver_scratch.increase_lower_bound_and_notify(propagator_scratch, 0, s0_scratch, 5);
        let _ =
            solver_scratch.decrease_upper_bound_and_notify(propagator_scratch, 0, s0_scratch, 5);

        let mut solver = TestSolver::default();
        let s0 = solver.new_variable(1, 11);
        let s1 = solver.new_variable(1, 5);
        let s2 = solver.new_variable(1, 5);

        let propagator = solver
            .new_propagator(
                TimeTablePerPointIncrementalPropagator::<DomainId, true>::new(
                    &[
                        ArgTask {
                            start_time: s0,
                            processing_time: 4,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s1,
                            processing_time: 1,
                            resource_usage: 1,
                        },
                        ArgTask {
                            start_time: s2,
                            processing_time: 1,
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
                ),
            )
            .expect("No conflict");
        let _ = solver.increase_lower_bound_and_notify(propagator, 2, s2, 5);
        let _ = solver.increase_lower_bound_and_notify(propagator, 1, s1, 5);
        let _ = solver.increase_lower_bound_and_notify(propagator, 0, s0, 5);
        let _ = solver.decrease_upper_bound_and_notify(propagator, 0, s0, 5);

        let result_scratch = solver_scratch.propagate(propagator_scratch);
        assert!(result_scratch.is_err());
        let result = solver.propagate(propagator);
        assert!(result.is_err());
        if let (
            Err(Inconsistency::Conflict(explanation)),
            Err(Inconsistency::Conflict(explanation_scratch)),
        ) = (result, result_scratch)
        {
            assert_eq!(
                explanation.iter().collect::<Vec<_>>(),
                explanation_scratch.iter().collect::<Vec<_>>()
            );
        } else {
            panic!("Incorrect result")
        }
    }
}
