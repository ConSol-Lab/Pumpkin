use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
    ops::Range,
    rc::Rc,
};

use crate::pumpkin_assert_advanced;
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
};
use crate::{propagators::OverIntervalTimeTableType, pumpkin_assert_extreme};

use super::{
    time_table_propagator::{
        has_overlap_with_interval, should_enqueue, ResourceProfile, TimeTablePropagator,
    },
    OverIntervalIteratorType, TimeTableOverIntervalProp,
};

/// Propagator responsible for using time-table reasoning to propagate the [Cumulative] constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at different time-points -
/// This method creates a [ResourceProfile] over intervals rather than creating per time-point (hence the name).
///
/// See ["Improving Scheduling by Learning - Andreas Schutt (2011)"](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
/// Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 for more information about time-table reasoning.
pub struct TimeTableOverIntervalIncrementalProp<Var> {
    /// Each elements holds the mandatory resource consumption of 1 or more tasks over an interval (stored in a [ResourceProfile]);
    /// the [ResourceProfile]s are sorted based on start time and they are assumed to be non-overlapping
    time_table: OverIntervalTimeTableType<Var>,
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

impl<Var> CPPropagatorConstructor for CumulativeArgs<Var, TimeTableOverIntervalIncrementalProp<Var>>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Propagator = TimeTableOverIntervalIncrementalProp<Var>;

    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let (tasks, horizon) = Util::create_tasks(&self.tasks, context);
        TimeTableOverIntervalIncrementalProp::new(CumulativeParameters::new(
            tasks,
            self.capacity,
            horizon,
        ))
    }
}

impl<Var: IntVar + 'static> TimeTableOverIntervalIncrementalProp<Var> {
    pub fn new(parameters: CumulativeParameters<Var>) -> TimeTableOverIntervalIncrementalProp<Var> {
        let reasons_for_propagation: Vec<HashMap<i32, PropositionalConjunction>> =
            vec![HashMap::new(); parameters.tasks.len()];
        TimeTableOverIntervalIncrementalProp {
            time_table: Default::default(),
            reasons_for_propagation_lower_bound: reasons_for_propagation.to_vec(),
            reasons_for_propagation_upper_bound: reasons_for_propagation,
            parameters,
            time_table_outdated: false,
        }
    }

    /// Performs a binary search on the [time-table][TimeTableOverIntervalIncrementalProp::time_table] to find *an* element which overlaps with the `update_range`.
    /// If such an element can be found then it returns [Ok] containing the index of the overlapping profile.
    /// If no such element could be found, it returns [Err] containing the index at which the element should be inserted to preserve the ordering
    fn find_overlapping_profile(
        time_table: &OverIntervalTimeTableType<Var>,
        update_range: &Range<i32>,
    ) -> Result<usize, usize> {
        time_table.binary_search_by(|profile| {
            if has_overlap_with_interval(
                update_range.start,
                update_range.end - 1,
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

    /// Determines which profiles are required to be updated given a range of times which now include a mandatory part (i.e. determine the profiles which overlap with the update_range).
    /// It returns two indices into [time_table][TimeTableOverIntervalIncrementalProp::time_table] representing the index of the first profile which overlaps with the update_range (inclusive)
    /// and the index of the last profile which overlaps with the update_range (inclusive) or [None] if there are no overlapping profiles
    ///
    /// Note that the lower-bound of the range is inclusive and the upper-bound is exclusive
    fn determine_profiles_to_update(
        time_table: &OverIntervalTimeTableType<Var>,
        update_range: &Range<i32>,
    ) -> Result<(usize, usize), usize> {
        let overlapping_profile = TimeTableOverIntervalIncrementalProp::find_overlapping_profile(
            time_table,
            update_range,
        );

        if overlapping_profile.is_err() {
            // We have not found any profile which overlaps with the update range
            return Err(overlapping_profile.expect_err("Overlapping profile could not be found"));
        }

        // Now we need to find all of the profiles which are adjacent to `overlapping_profile` which also overlap with `update_range`
        // Our starting index is thus the index of `overlapping_profile` and we need to search to the left and the right of the profile to find the other overlapping profiles
        let mut left_most_overlapping_index = overlapping_profile.unwrap();
        let mut right_most_overlapping_index = left_most_overlapping_index;
        if left_most_overlapping_index > 0 {
            // We go to the left of `overlapping_profile`
            for left_profile_index in (0..left_most_overlapping_index).rev() {
                let profile = &time_table[left_profile_index];
                if has_overlap_with_interval(
                    update_range.start,
                    update_range.end - 1,
                    profile.start,
                    profile.end,
                ) {
                    // We now know that the left most overlapping index is either `left_profile_index` or before it
                    left_most_overlapping_index = left_profile_index;
                } else {
                    // We know that no profile on the left of this one will be overlapping with `update_range`
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
                update_range.end - 1,
                profile.start,
                profile.end,
            ) {
                // We now know that the right most overlapping index is either `right_profile_index` or after it
                right_most_overlapping_index = right_profile_index;
            } else {
                // We know that no profile on the right of this one will be overlapping with `update_range`
                break;
            }
        }
        Ok((left_most_overlapping_index, right_most_overlapping_index))
    }

    /// Determines whether 2 profiles are mergeable (i.e. they are next to each other, consist of the same tasks and have the same height);
    /// this method is used when propagating incrementally and maintaining maximal profiles.
    ///
    /// It is assumed that the profile tasks of both profiles do not contain duplicates
    fn are_mergeable(
        first_profile: &ResourceProfile<Var>,
        second_profile: &ResourceProfile<Var>,
    ) -> bool {
        pumpkin_assert_extreme!(
            first_profile
                .profile_tasks
                .iter()
                .collect::<HashSet<_>>()
                .len()
                == first_profile.profile_tasks.len(),
            "The first provided profile had duplicate profile tasks"
        );
        pumpkin_assert_extreme!(
            second_profile
                .profile_tasks
                .iter()
                .collect::<HashSet<_>>()
                .len()
                == second_profile.profile_tasks.len(),
            "The second provided profile had duplicate profile tasks"
        );
        // First we perform the simple checks, determining whether the two profiles are the same height, whether they are next to one another and whether they contain the same number of tasks
        let mergeable = first_profile.height == second_profile.height
            && first_profile.end == second_profile.start - 1
            && first_profile.profile_tasks.len() == second_profile.profile_tasks.len();
        if !mergeable {
            // The tasks have already been found to be not mergeable so we can avoid checking equality of the profile tasks
            mergeable
        } else {
            // We check whether the profile tasks of both profiles are the same
            mergeable
                && first_profile
                    .profile_tasks
                    .iter()
                    .all(|profile| second_profile.profile_tasks.contains(profile))
        }
    }

    /// Merge all mergeable profiles going from start_index to end_index
    fn merge_profiles(
        time_table: &mut OverIntervalTimeTableType<Var>,
        start_index: i32,
        end_index: i32,
    ) {
        let mut current_index = start_index;
        let mut end = end_index;

        // We go over all pairs of profiles, starting from start index until end index
        while current_index < end {
            let first = current_index;
            while current_index < end
                && TimeTableOverIntervalIncrementalProp::are_mergeable(
                    &time_table[current_index as usize],
                    &time_table[(current_index + 1) as usize],
                )
            {
                // We go over all pairs of profiles until we find a profile which cannot be merged with the current profile
                current_index += 1;
            }

            if current_index > first {
                // We have found at least 2 profiles to merge (but perhaps more)
                let start_profile = &time_table[first as usize];
                let end_profile = &time_table[current_index as usize];

                // We create a new profile with the bounds which we have found
                let new_profile = ResourceProfile {
                    start: start_profile.start,
                    end: end_profile.end,
                    profile_tasks: start_profile.profile_tasks.to_owned(),
                    height: start_profile.height,
                };
                // We replace the previously separate profile with the new profile
                time_table.splice(first as usize..(current_index + 1) as usize, [new_profile]);

                // We have removed profiles from the time-table and we thus need to adjust our end-index under consideration by the number of profiles which were removed
                end -= current_index - first;

                // We reset the current index to the index of the new profile and move onto the next profile
                current_index = first;
            }
            current_index += 1;
        }
    }
}

impl<Var: IntVar + 'static + std::fmt::Debug> ConstraintProgrammingPropagator
    for TimeTableOverIntervalIncrementalProp<Var>
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
            // We keep track of the lowest index which can be merged
            let mut lowest_index = u32::MAX;
            for Updated {
                task,
                old_lower_bound,
                old_upper_bound,
                new_lower_bound,
                new_upper_bound,
            } in self.parameters.updated.iter()
            {
                let (lower_update_range, upper_update_range) = generate_update_range(
                    task,
                    *old_lower_bound,
                    *old_upper_bound,
                    *new_lower_bound,
                    *new_upper_bound,
                );
                // We consider both of the possible update ranges
                // Note that the upper update range is first considered to avoid any issues with the indices when processing the other update range
                for update_range in
                    std::iter::once(upper_update_range).chain(std::iter::once(lower_update_range))
                {
                    if update_range.is_empty() {
                        // This case only occurs if a fully new mandatory part is added
                        continue;
                    }

                    // Keep track of the profiles which need to be added
                    let mut to_add: Vec<ResourceProfile<Var>> = Vec::new();

                    // First we attempt to find overlapping profiles
                    match TimeTableOverIntervalIncrementalProp::determine_profiles_to_update(
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

                            // Go over all indices of the profiles which overlap with the updated one and determine which one need to be updated
                            for current_index in start_index..=end_index {
                                let profile = &self.time_table[current_index];

                                if current_index == start_index
                                    && update_range.start < profile.start
                                {
                                    // We are considering the first overlapping profile and there is a part before the start of this profile
                                    // This means we need to add a new mandatory part before the first element
                                    to_add.push(ResourceProfile {
                                        start: update_range.start,
                                        end: profile.start - 1, // Note that this profile needs to end before the start of the current profile, hence the -1
                                        profile_tasks: vec![Rc::clone(task)],
                                        height: task.resource_usage,
                                    })
                                }
                                if current_index != start_index && current_index != 0 {
                                    // We are not considering the first profile and there could be a new profile between the current profile and the previous one caused by the updated task
                                    let previous_profile = &self.time_table[current_index - 1];
                                    if previous_profile.end < profile.start - 1 // There is empty space between the current profile and the previous profile
                                    && update_range.start <= previous_profile.end + 1 // The update range starts before the end of the previous profile
                                    && update_range.end > profile.start - 1
                                    // The update range ends after the start of the current profile
                                    {
                                        // There is empty space between the current profile and the previous one, we should insert a new profile
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
                                    // This if-statement takes care of creating a new (smaller) profile which represents the previous profile up and until it is split by the update range
                                    to_add.push(ResourceProfile {
                                        start: profile.start,
                                        end: min(update_range.start - 1, profile.end), // It could be that the update range extends past the profile in which case we should create a profile until the end of the profile
                                        profile_tasks: profile.profile_tasks.clone(),
                                        height: profile.height,
                                    })
                                }

                                // Now we create a new profile which consists of the part of the profile covered by the update range
                                // This means that we are adding the contribution of the updated task to the profile and adjusting the bounds appropriately

                                // Either the new profile starts at the start of the profile (in case the update range starts before the profile start)
                                // or the new profile starts at the start of the update range (since we are only looking at the part where there is overlap between the current profile and the update range)
                                let new_profile_lower_bound =
                                    max(profile.start, update_range.start);

                                // Either the new profile ends at the end of the profile (in case the update range ends after the profile end)
                                // or the new profile ends at the end of the update range (since we are only looking at the part where there is overlap between the current profile and the update range)
                                let new_profile_upper_bound =
                                    min(profile.end, update_range.end - 1); // Note that the end of the update_range is exclusive (hence the -1)
                                let mut new_profile_tasks = profile.profile_tasks.clone();
                                new_profile_tasks.push(Rc::clone(task));
                                if new_profile_upper_bound >= new_profile_lower_bound {
                                    // A sanity check, there is a new profile to create consisting of a combination of the previous profile and the updated task
                                    if profile.height + task.resource_usage
                                        > self.parameters.capacity
                                    {
                                        // The addition of the new mandatory part to the profile caused an overflow of the resource
                                        return Util::create_error_clause(
                                            context,
                                            &new_profile_tasks,
                                        );
                                    }

                                    // We thus create a new profile consisting of the combination of the previous profile and the updated task under consideration
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
                                    // This if-statement takes care of creating a new (smaller) profile which represents the previous profile after it is split by the update range
                                    to_add.push(ResourceProfile {
                                        start: max(update_range.end, profile.start),
                                        end: profile.end,
                                        profile_tasks: profile.profile_tasks.clone(),
                                        height: profile.height,
                                    })
                                }
                                if current_index == end_index && update_range.end > profile.end + 1
                                {
                                    // We are considering the last overlapping profile and there is a part after the end of this profile
                                    // This means we need to add a new mandatory part after the last element
                                    to_add.push(ResourceProfile {
                                        start: profile.end + 1,
                                        end: update_range.end - 1,
                                        profile_tasks: vec![Rc::clone(task)],
                                        height: task.resource_usage,
                                    })
                                }
                            }
                            // We now update the time-table to insert the newly created profiles at the right place to ensure the ordering invariant
                            self.time_table.splice(start_index..end_index + 1, to_add);
                        }
                        Err(index_to_insert) => {
                            //Update the lowest index which we have found so far which has been updated
                            lowest_index = min(
                                lowest_index,
                                if index_to_insert == 0 {
                                    0
                                } else {
                                    index_to_insert as u32 - 1
                                },
                            );

                            //Insert the new profile at its index
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
            if lowest_index != u32::MAX {
                // We have updated at least 1 of the profiles, we can now merge the profiles which are adjacent to one another
                // We start at the lowest index which we have found and continue from there
                let time_table_len = self.time_table.len();
                TimeTableOverIntervalIncrementalProp::merge_profiles(
                    &mut self.time_table,
                    lowest_index as i32,
                    (time_table_len - 1) as i32,
                );
            }
        }

        pumpkin_assert_extreme!({
            let time_table_scratch = TimeTableOverIntervalIncrementalProp::create_time_table(
                context,
                self.get_parameters(),
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
        "CumulativeTimeTableOverIntervalIncremental"
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
        TimeTableOverIntervalProp::debug_propagate_from_scratch_time_table_interval(
            context,
            self.get_parameters(),
        )
    }
}

impl<Var: IntVar + 'static> TimeTablePropagator<Var> for TimeTableOverIntervalIncrementalProp<Var> {
    type TimeTableType = OverIntervalTimeTableType<Var>;
    type TimeTableIteratorType<'a> = OverIntervalIteratorType<'a, Var>;

    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
    ) -> Result<(), Vec<Rc<Task<Var>>>> {
        match <TimeTableOverIntervalIncrementalProp<Var> as TimeTablePropagator<Var>>::create_time_table(
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
        TimeTableOverIntervalProp::create_time_table_over_interval_from_scratch(context, parameters)
    }

    fn get_parameters(&self) -> &CumulativeParameters<Var> {
        &self.parameters
    }

    fn get_time_table(&self) -> Self::TimeTableIteratorType<'_> {
        self.time_table.iter()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::{
            ConflictInfo, Inconsistency, Predicate, PredicateConstructor, PropositionalConjunction,
        },
        engine::{test_helper::TestSolver, Delta, DomainChange, EnqueueDecision, LocalId},
        propagators::{ArgTask, TimeTableOverIntervalIncremental},
    };

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        solver
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
            ))
            .expect("No conflict");
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
                s2.upper_bound_predicate(9),
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
                s2.lower_bound_predicate(0),
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
                s3.lower_bound_predicate(2),
            ]),
            reason
        );
    }
}
