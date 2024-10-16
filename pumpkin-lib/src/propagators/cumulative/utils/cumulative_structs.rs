//! Stores structures related for the Cumulative constraint such as the [`Task`]s or the
//! [`CumulativeParameters`].
use std::cmp::Ordering;
use std::hash::Hash;
use std::ops::Range;
use std::rc::Rc;

use super::SparseSet;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::variables::IntegerVariable;
use crate::propagators::CumulativePropagatorOptions;
use crate::pumpkin_assert_moderate;

/// Structure which stores the variables related to a task; for now, only the start times are
/// assumed to be variable
#[derive(Debug)]
pub(crate) struct Task<Var> {
    /// The variable representing the start time of a task
    pub(crate) start_variable: Var,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub(crate) processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: i32,
    /// The [`LocalId`] of the task
    pub(crate) id: LocalId,
}

impl<Var: IntegerVariable + 'static> Task<Var> {
    pub(crate) fn get_id(task: &Rc<Task<Var>>) -> usize {
        task.id.unpack() as usize
    }
}

impl<Var: IntegerVariable + 'static> Hash for Task<Var> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<Var: IntegerVariable + 'static> PartialEq for Task<Var> {
    fn eq(&self, other: &Self) -> bool {
        self.id.unpack() == other.id.unpack()
    }
}

impl<Var: IntegerVariable + 'static> Eq for Task<Var> {}

/// The task which is passed as argument
#[derive(Clone, Debug)]
pub(crate) struct ArgTask<Var> {
    /// The [`IntegerVariable`] representing the start time of a task
    pub(crate) start_time: Var,
    /// The processing time of the [`start_time`][ArgTask::start_time] (also referred to as
    /// duration of a task)
    pub(crate) processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: i32,
}

/// Stores the information of an updated task; for example in the context of
/// [`TimeTablePerPointPropagator`] this is a task whose mandatory part has changed.
#[derive(Debug, Clone)]
pub(crate) struct UpdatedTaskInfo<Var> {
    /// The task which has been updated (where "updated" is according to some context-dependent
    /// definition)
    pub(crate) task: Rc<Task<Var>>,
    /// The lower-bound of the [`Task`] before the update
    pub(crate) old_lower_bound: i32,
    /// The upper-bound of the [`Task`] before the update
    pub(crate) old_upper_bound: i32,
    /// The lower-bound of the [`Task`] after the update
    pub(crate) new_lower_bound: i32,
    /// The upper-bound of the [`Task`] after the update
    pub(crate) new_upper_bound: i32,
}

/// Represents adjustments to a mandatory part due to bound changes.
///
/// It contains both the additions to the mandatory part (stored in
/// [`MandatoryPartAdjustments::added_parts`]) and the removals from the mandatory part
/// [`MandatoryPartAdjustments::removed_parts`].
pub(crate) struct MandatoryPartAdjustments {
    /// The additions to the mandatory part
    added_parts: Vec<Range<i32>>,
    /// The removals from the mandatory part
    removed_parts: Vec<Range<i32>>,
}

impl MandatoryPartAdjustments {
    fn new(added_parts: Vec<Range<i32>>, removed_parts: Vec<Range<i32>>) -> Self {
        Self {
            added_parts,
            removed_parts,
        }
    }

    /// Returns an iterator over the removed ranges of the mandatory part; only returns non-empty
    /// intervals.
    pub(crate) fn get_removed_parts(&self) -> impl Iterator<Item = Range<i32>> + '_ {
        self.removed_parts
            .iter()
            .filter(|range| !range.is_empty())
            .cloned()
    }

    /// Returns an iterator over the added ranges of the mandatory part; only returns non-mepty
    /// intervals.
    pub(crate) fn get_added_parts(&self) -> impl Iterator<Item = Range<i32>> + '_ {
        self.added_parts
            .iter()
            .filter(|range| !range.is_empty())
            .cloned()
    }

    /// Creates an empty [`MandatoryPartAdjustments`] (i.e. with no added parts and with no removed
    /// parts).
    fn empty() -> Self {
        Self {
            added_parts: vec![],
            removed_parts: vec![],
        }
    }

    /// Creates a [`MandatoryPartAdjustments`] containing a single added part.
    fn from_added_part(added_part: Range<i32>) -> Self {
        Self {
            added_parts: vec![added_part],
            removed_parts: vec![],
        }
    }

    /// Creates a [`MandatoryPartAdjustments`] containing a single removed part.
    fn from_removed_part(removed_part: Range<i32>) -> Self {
        Self {
            added_parts: vec![],
            removed_parts: vec![removed_part],
        }
    }

    /// Creates a [`MandatoryPartAdjustments`] containing a single added part and a single removed
    /// part.
    fn from_added_and_removed_part(added_part: Range<i32>, removed_part: Range<i32>) -> Self {
        Self {
            added_parts: vec![added_part],
            removed_parts: vec![removed_part],
        }
    }
}

impl<Var> UpdatedTaskInfo<Var> {
    /// Returns the adjustments which need to be made to the time-table in the form of a
    /// [`MandatoryPartAdjustments`].
    pub(crate) fn get_mandatory_part_adjustments(&self) -> MandatoryPartAdjustments {
        // We get the previous mandatory part
        let previous_mandatory_part =
            self.old_upper_bound..self.old_lower_bound + self.task.processing_time;
        // We also get the new mandatory part
        let new_mandatory_part =
            self.new_upper_bound..self.new_lower_bound + self.task.processing_time;

        if previous_mandatory_part.is_empty() && new_mandatory_part.is_empty() {
            // If both are empty then no adjustments should be made
            return MandatoryPartAdjustments::empty();
        }

        if previous_mandatory_part.is_empty() {
            // There is no previous mandatory part, simply add the new one
            return MandatoryPartAdjustments::from_added_part(new_mandatory_part);
        }
        if new_mandatory_part.is_empty() {
            // There is no new mandatory part, simply remove the old mandatory part
            return MandatoryPartAdjustments::from_removed_part(previous_mandatory_part);
        }

        if previous_mandatory_part.start >= new_mandatory_part.end
            || new_mandatory_part.start >= previous_mandatory_part.end
        {
            // There is no overlap between the parts, we remove the old mandatory part and add the
            // new
            return MandatoryPartAdjustments::from_added_and_removed_part(
                new_mandatory_part,
                previous_mandatory_part,
            );
        }

        let mut removed_parts = vec![];
        let mut added_parts = vec![];

        // We first check the adjustments which need to be made based on the new end time of the
        // mandatory part
        match new_mandatory_part.end.cmp(&previous_mandatory_part.end) {
            Ordering::Less => {
                // The new mandatory parts ends before the previous mandatory part
                // This means that we need to remove from the time-table
                removed_parts.push(new_mandatory_part.end..previous_mandatory_part.end)
            }
            Ordering::Equal => {
                // Do nothing in case they are equal
            }
            Ordering::Greater => {
                // The new mandatory parts ends after the previous mandatory part
                // This means that we need to add to the time-table
                added_parts.push(previous_mandatory_part.end..new_mandatory_part.end);
            }
        }

        // Then we check the adjustments which need to be made based on the new start time of the
        // mandatory part
        match new_mandatory_part.start.cmp(&previous_mandatory_part.start) {
            Ordering::Less => {
                // The new mandatory part starts before the previous mandatory part
                // This means that we need to add to the time-table
                added_parts.push(new_mandatory_part.start..previous_mandatory_part.start);
            }
            Ordering::Equal => {

                // Do nothing in case they are equal
            }
            Ordering::Greater => {
                // The new mandatory part starts later than the previous mandatory part
                // This means that we need to remove from the time-table
                removed_parts.push(previous_mandatory_part.start..new_mandatory_part.start);
            }
        }
        MandatoryPartAdjustments::new(added_parts, removed_parts)
    }
}

/// Structures which are adjusted during search; either due to incrementality or to keep track of
/// bounds.
#[derive(Debug, Clone)]
pub(crate) struct UpdatableStructures<Var> {
    /// The current known bounds of the different [tasks][CumulativeParameters::tasks]; stored as
    /// (lower bound, upper bound)
    ///
    /// `bounds[i]` represents the currently known bounds of task i
    bounds: Vec<(i32, i32)>,
    /// The [`Task`]s which have been updated since the last round of propagation, this structure
    /// is updated by the (incremental) propagator
    updates: Vec<UpdatedTaskInfo<Var>>,
    /// The tasks which have been updated since the last iteration
    updated_tasks: SparseSet<Rc<Task<Var>>>,
    /// The tasks which are unfixed
    unfixed_tasks: SparseSet<Rc<Task<Var>>>,
}

impl<Var: IntegerVariable + 'static> UpdatableStructures<Var> {
    pub(crate) fn new(parameters: &CumulativeParameters<Var>) -> Self {
        let mut updated_tasks = SparseSet::new(parameters.tasks.to_vec(), Task::get_id);
        updated_tasks.set_to_empty();

        let unfixed_tasks = SparseSet::new(parameters.tasks.to_vec(), Task::get_id);
        Self {
            bounds: vec![],
            updates: vec![],
            updated_tasks,
            unfixed_tasks,
        }
    }

    /// Returns whether there are any updates stored which have not been processed
    pub(crate) fn has_updates(&self) -> bool {
        !self.updated_tasks.is_empty()
    }

    /// Returns the next updated task and removes it from the updated list
    pub(crate) fn pop_next_updated_task(&mut self) -> Option<Rc<Task<Var>>> {
        if self.updated_tasks.is_empty() {
            return None;
        }
        let updated_task = Rc::clone(self.updated_tasks.get(0));
        self.updated_tasks.remove(&updated_task);
        Some(updated_task)
    }

    /// Get the update info for the provided task (note that this method does not actually check
    /// whether the updated task was actually updated).
    pub(crate) fn get_update_for_task(
        &mut self,
        updated_task: &Rc<Task<Var>>,
    ) -> UpdatedTaskInfo<Var> {
        self.updates[updated_task.id.unpack() as usize].clone()
    }

    /// Resets the stored update for the current task to be equal to the current scenario; i.e.
    /// resets the old bounds to be equal to the new bounds
    pub(crate) fn reset_update_for_task(&mut self, updated_task: &Rc<Task<Var>>) {
        let update = &mut self.updates[updated_task.id.unpack() as usize];

        update.old_lower_bound = update.new_lower_bound;
        update.old_upper_bound = update.new_upper_bound;
    }

    /// Returns the bounds which are stored for each tasks.
    pub(crate) fn get_stored_bounds(&self) -> &[(i32, i32)] {
        &self.bounds
    }

    /// Returns a mutable reference to the bounds which are stored for each task.
    pub(crate) fn get_stored_bounds_mut(&mut self) -> &mut [(i32, i32)] {
        &mut self.bounds
    }

    /// Returns the stored lower-bound for a task.
    pub(crate) fn get_stored_lower_bound(&self, task: &Rc<Task<Var>>) -> i32 {
        self.bounds[task.id.unpack() as usize].0
    }

    /// Returns the stored upper-bound for a task.
    pub(crate) fn get_stored_upper_bound(&self, task: &Rc<Task<Var>>) -> i32 {
        self.bounds[task.id.unpack() as usize].1
    }

    /// Fixes a task in the internal structure(s).
    pub(crate) fn fix_task(&mut self, updated_task: &Rc<Task<Var>>) {
        self.unfixed_tasks.remove(updated_task);
    }

    /// Unfixes a task in the internal structure(s).
    pub(crate) fn unfix_task(&mut self, updated_task: Rc<Task<Var>>) {
        self.unfixed_tasks.insert(updated_task);
    }

    /// Resets all of the bounds to the current values in the context and removes all of the fixed
    /// tasks from the internal structure(s).
    pub(crate) fn reset_all_bounds_and_remove_fixed<Context: ReadDomains>(
        &mut self,
        context: &Context,
        parameters: &CumulativeParameters<Var>,
    ) {
        for task in parameters.tasks.iter() {
            if self.updates.len() <= task.id.unpack() as usize {
                // If have not stored an update for it before then we create it now
                pumpkin_assert_moderate!(task.id.unpack() as usize == self.updates.len());
                self.updates.push(UpdatedTaskInfo {
                    task: Rc::clone(task),
                    old_lower_bound: context.lower_bound(&task.start_variable),
                    old_upper_bound: context.upper_bound(&task.start_variable),
                    new_lower_bound: context.lower_bound(&task.start_variable),
                    new_upper_bound: context.upper_bound(&task.start_variable),
                });
            } else {
                // Otherwise we simply update the bounds to be equal to the current bounds
                let update = &mut self.updates[task.id.unpack() as usize];
                update.new_lower_bound = context.lower_bound(&task.start_variable);
                update.new_upper_bound = context.upper_bound(&task.start_variable);
                update.old_lower_bound = context.lower_bound(&task.start_variable);
                update.old_upper_bound = context.upper_bound(&task.start_variable);
            }

            if self.bounds.len() <= task.id.unpack() as usize {
                // If we have not stored the bound then we add it
                pumpkin_assert_moderate!(task.id.unpack() as usize == self.bounds.len());
                self.bounds.push((
                    context.lower_bound(&task.start_variable),
                    context.upper_bound(&task.start_variable),
                ));
            } else {
                // Otherwise we simply store the current bounds
                self.bounds[task.id.unpack() as usize] = (
                    context.lower_bound(&task.start_variable),
                    context.upper_bound(&task.start_variable),
                );
            }

            // If the task is fixed then we remove it, otherwise we insert it as an unfixed task
            if context.is_fixed(&task.start_variable) {
                self.unfixed_tasks.remove(task);
            } else {
                self.unfixed_tasks.insert(Rc::clone(task));
            }
        }
    }

    // Initialises all stored bounds to their current values and removes any tasks which are fixed
    pub(crate) fn initialise_bounds_and_remove_fixed<Context: ReadDomains>(
        &mut self,
        context: &Context,
        parameters: &CumulativeParameters<Var>,
    ) {
        for task in parameters.tasks.iter() {
            self.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ));
            if context.is_fixed(&task.start_variable) {
                self.fix_task(task);
            }
        }
    }

    /// Returns all of the tasks which are not currently fixed
    pub(crate) fn get_unfixed_tasks(&self) -> impl Iterator<Item = &Rc<Task<Var>>> {
        self.unfixed_tasks.iter()
    }

    // Returns all of the tasks which are currently fixed
    pub(crate) fn get_fixed_tasks(&self) -> impl Iterator<Item = &Rc<Task<Var>>> {
        self.unfixed_tasks.out_of_domain()
    }

    // Returns the number of unfixed tasks
    pub(crate) fn number_of_unfixed_tasks(&self) -> usize {
        self.unfixed_tasks.len()
    }

    // Returns whether there are no unfixed tasks
    pub(crate) fn has_no_unfixed_tasks(&self) -> bool {
        self.unfixed_tasks.is_empty()
    }

    // Temporarily removes a task from the set of unfixed tasks
    pub(crate) fn temporarily_remove_task_from_unfixed(&mut self, task: &Rc<Task<Var>>) {
        self.unfixed_tasks.remove_temporarily(task)
    }

    // Restore all of the temporarily removed tasks
    pub(crate) fn restore_temporarily_removed(&mut self) {
        self.unfixed_tasks.restore_temporarily_removed()
    }

    // Returns the unfixed task at the specified index
    pub(crate) fn get_unfixed_task_at_index(&self, index: usize) -> Rc<Task<Var>> {
        Rc::clone(self.unfixed_tasks.get(index))
    }

    // Marks a task as updated in the internal structure(s)
    pub(crate) fn task_has_been_updated(&mut self, task: &Rc<Task<Var>>) {
        self.updated_tasks.insert(Rc::clone(task))
    }

    // Insert the provided update for a specific task; this means that the new bounds of the tasks
    // are updated to the ones provided in the update
    pub(crate) fn insert_update_for_task(
        &mut self,
        task: &Rc<Task<Var>>,
        updated_task_info: UpdatedTaskInfo<Var>,
    ) {
        let stored_updated_task_info = &mut self.updates[task.id.unpack() as usize];

        stored_updated_task_info.new_lower_bound = updated_task_info.new_lower_bound;
        stored_updated_task_info.new_upper_bound = updated_task_info.new_upper_bound;
    }

    /// Used for creating the dynamic structures from the provided context
    pub(crate) fn recreate_from_context(
        &self,
        context: &PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) -> Self {
        let mut other = self.clone();

        parameters
            .tasks
            .iter()
            .for_each(|task| other.unfix_task(Rc::clone(task)));
        other.reset_all_bounds_and_remove_fixed(context, parameters);

        other
    }
}

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The options for propagating the cumulative constraint
#[derive(Debug, Clone)]
pub(crate) struct CumulativeParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: i32,
    /// The [`CumulativeOptions`] which influence the behaviour of the cumulative propagator(s).
    pub(crate) options: CumulativePropagatorOptions,
}

impl<Var: IntegerVariable + 'static> CumulativeParameters<Var> {
    pub(crate) fn new(
        tasks: Vec<Task<Var>>,
        capacity: i32,
        options: CumulativePropagatorOptions,
    ) -> CumulativeParameters<Var> {
        let tasks = tasks
            .into_iter()
            .map(Rc::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();

        CumulativeParameters {
            tasks: tasks.clone(),
            capacity,
            options,
        }
    }
}
