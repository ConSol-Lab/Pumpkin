use std::rc::Rc;

use super::CumulativeParameters;
use super::Task;
use super::UpdatedTaskInfo;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::containers::SparseSet;
use crate::create_statistics_struct;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::ReadDomains;
use crate::pumpkin_assert_moderate;
use crate::variables::IntegerVariable;

create_statistics_struct!(CumulativeStatistics {
    number_of_propagation_calls: usize,
    number_of_tasks_traversed: usize,
    number_of_profiles_traversed: usize,
    average_size_of_time_table: CumulativeMovingAverage<usize>,
    average_fragmentation_ratio: CumulativeMovingAverage<f64>,
    number_of_merges: usize,
    average_reduction_after_merge: CumulativeMovingAverage<f64>
});

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
    pub(crate) unfixed_tasks: SparseSet<Rc<Task<Var>>>,
    pub(crate) statistics: CumulativeStatistics,
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
            statistics: CumulativeStatistics::default(),
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

    /// Removes the fixed tasks from the internal structure(s).
    pub(crate) fn remove_fixed(
        &mut self,
        context: PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) {
        for task in parameters.tasks.iter() {
            // If the task is fixed then we remove it, otherwise we insert it as an unfixed task
            if context.is_fixed(&task.start_variable) {
                self.unfixed_tasks.remove(task);
            } else {
                self.unfixed_tasks.insert(Rc::clone(task));
            }
        }
    }

    /// Resets all of the bounds to the current values in the context and removes all of the fixed
    /// tasks from the internal structure(s).
    pub(crate) fn reset_all_bounds_and_remove_fixed(
        &mut self,
        context: PropagationContext,
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
    pub(crate) fn initialise_bounds_and_remove_fixed(
        &mut self,
        context: PropagationContext,
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
        context: PropagationContext,
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
