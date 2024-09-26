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
/// [`TimeTablePerPointPropagator`] this is a task who's mandatory part has changed.
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

impl<Var> UpdatedTaskInfo<Var> {
    pub(crate) fn get_removed_and_added_mandatory_parts(
        &self,
    ) -> (Vec<Range<i32>>, Vec<Range<i32>>) {
        let previous_mandatory_part =
            self.old_upper_bound..self.old_lower_bound + self.task.processing_time;
        let new_mandatory_part =
            self.new_upper_bound..self.new_lower_bound + self.task.processing_time;

        if previous_mandatory_part.is_empty() {
            // There is no previous mandatory part, simply add the new one
            return (vec![], vec![new_mandatory_part]);
        }
        if new_mandatory_part.is_empty() {
            // There is no new mandatory part, simply remove the old mandatory part
            return (vec![previous_mandatory_part], vec![]);
        }

        if previous_mandatory_part.start >= new_mandatory_part.end
            || new_mandatory_part.start >= previous_mandatory_part.end
        {
            // There is no overlap between the parts, we remove the old mandatory part and add the
            // new
            return (vec![previous_mandatory_part], vec![new_mandatory_part]);
        }

        let mut removed_parts = vec![];
        let mut added_parts = vec![];

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
        (removed_parts, added_parts)
    }
}

/// Structures which are adjusted during search; either due to incrementality or to keep track of
/// bounds.
#[derive(Debug, Clone)]
pub(crate) struct DynamicStructures<Var> {
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

impl<Var: IntegerVariable + 'static> DynamicStructures<Var> {
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

    pub(crate) fn has_updates(&self) -> bool {
        !self.updates.is_empty()
    }

    pub(crate) fn pop_next_updated_task(&mut self) -> Option<Rc<Task<Var>>> {
        if self.updated_tasks.is_empty() {
            return None;
        }
        let updated_task = Rc::clone(self.updated_tasks.get(0));
        self.updated_tasks.remove(&updated_task);
        Some(updated_task)
    }

    pub(crate) fn get_update_for_task(
        &mut self,
        updated_task: &Rc<Task<Var>>,
    ) -> UpdatedTaskInfo<Var> {
        self.updates[updated_task.id.unpack() as usize].clone()
    }

    pub(crate) fn reset_update_for_task(&mut self, updated_task: &Rc<Task<Var>>) {
        let update = &mut self.updates[updated_task.id.unpack() as usize];

        update.old_lower_bound = update.new_lower_bound;
        update.old_upper_bound = update.new_upper_bound;
    }

    pub(crate) fn get_stored_bounds(&self) -> &[(i32, i32)] {
        &self.bounds
    }

    pub(crate) fn get_stored_bounds_mut(&mut self) -> &mut [(i32, i32)] {
        &mut self.bounds
    }

    pub(crate) fn get_stored_lower_bound(&self, task: &Rc<Task<Var>>) -> i32 {
        self.bounds[task.id.unpack() as usize].0
    }

    pub(crate) fn get_stored_upper_bound(&self, task: &Rc<Task<Var>>) -> i32 {
        self.bounds[task.id.unpack() as usize].1
    }

    pub(crate) fn fix_task(&mut self, updated_task: &Rc<Task<Var>>) {
        self.unfixed_tasks.remove(updated_task);
    }

    pub(crate) fn unfix_task(&mut self, updated_task: Rc<Task<Var>>) {
        self.unfixed_tasks.insert(updated_task);
    }

    pub(crate) fn reset_all_bounds_and_remove_fixed<Context: ReadDomains>(
        &mut self,
        context: &Context,
        parameters: &CumulativeParameters<Var>,
    ) {
        for task in parameters.tasks.iter() {
            if self.updates.len() <= task.id.unpack() as usize {
                self.updates.push(UpdatedTaskInfo {
                    task: Rc::clone(task),
                    old_lower_bound: context.lower_bound(&task.start_variable),
                    old_upper_bound: context.upper_bound(&task.start_variable),
                    new_lower_bound: context.lower_bound(&task.start_variable),
                    new_upper_bound: context.upper_bound(&task.start_variable),
                })
            } else {
                let update = &mut self.updates[task.id.unpack() as usize];
                update.new_lower_bound = context.lower_bound(&task.start_variable);
                update.new_upper_bound = context.upper_bound(&task.start_variable);
                update.old_lower_bound = context.lower_bound(&task.start_variable);
                update.old_upper_bound = context.upper_bound(&task.start_variable);
            }

            self.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ));
            if context.is_fixed(&task.start_variable) {
                self.unfixed_tasks.remove(task);
            }
        }
    }

    pub(crate) fn reset_all_bounds<Context: ReadDomains>(
        &mut self,
        context: &Context,
        parameters: &CumulativeParameters<Var>,
    ) {
        for task in parameters.tasks.iter() {
            self.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ));
        }
    }

    pub(crate) fn get_unfixed_tasks(&self) -> impl Iterator<Item = &Rc<Task<Var>>> {
        self.unfixed_tasks.iter()
    }

    pub(crate) fn number_of_unfixed_tasks(&self) -> usize {
        self.unfixed_tasks.len()
    }

    pub(crate) fn has_no_unfixed_tasks(&self) -> bool {
        self.unfixed_tasks.is_empty()
    }

    pub(crate) fn temporarily_remove_task_from_unfixed(&mut self, task: &Rc<Task<Var>>) {
        self.unfixed_tasks.remove_temporarily(task)
    }

    pub(crate) fn restore_temporarily_removed(&mut self) {
        self.unfixed_tasks.restore_temporarily_removed()
    }

    pub(crate) fn get_unfixed_task_at_index(&self, index: usize) -> Rc<Task<Var>> {
        Rc::clone(self.unfixed_tasks.get(index))
    }

    pub(crate) fn task_has_been_updated(&mut self, task: &Rc<Task<Var>>) {
        self.updated_tasks.insert(Rc::clone(task))
    }

    pub(crate) fn insert_update_for_task(
        &mut self,
        task: &Rc<Task<Var>>,
        updated_task_info: UpdatedTaskInfo<Var>,
    ) {
        let stored_updated_task_info = &mut self.updates[task.id.unpack() as usize];

        stored_updated_task_info.new_lower_bound = updated_task_info.new_lower_bound;
        stored_updated_task_info.new_upper_bound = updated_task_info.new_upper_bound;
    }

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
