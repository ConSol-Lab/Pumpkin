//! Stores structures related for the Cumulative constraint such as the [`Task`]s or the
//! [`CumulativeParameters`].
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

use super::SparseSet;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::variables::IntegerVariable;
use crate::propagators::TimeTableOverIntervalIncrementalPropagator;
use crate::propagators::TimeTableOverIntervalPropagator;
use crate::propagators::TimeTablePerPointIncrementalPropagator;
use crate::propagators::TimeTablePerPointPropagator;

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

/// The arguments which are required to create the constraint/propagators
#[derive(Debug, Clone)]
pub(crate) struct CumulativeConstructor<Var, T> {
    /// A box containing all of the [`ArgTask`]s
    pub(crate) tasks: Box<[ArgTask<Var>]>,
    /// The capacity of the resource
    pub(crate) capacity: i32,
    /// We use [`PhantomData`] to differentiate between the different types of propagators;
    /// without this field we would need to create a new argument struct for each cumulative
    /// propagator
    propagator_type: PhantomData<T>,
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub(crate) allow_holes_in_domain: bool,
}

impl<Var, T> CumulativeConstructor<Var, T> {
    pub(crate) fn new(
        tasks: Box<[ArgTask<Var>]>,
        capacity: i32,
        allow_holes_in_domain: bool,
    ) -> Self {
        CumulativeConstructor {
            tasks,
            capacity,
            propagator_type: PhantomData,
            allow_holes_in_domain,
        }
    }
}

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTablePerPointPropagator`]; this is used to prevent creating a different `new`
/// method for each type `T`
#[allow(unused)]
pub(crate) type TimeTablePerPoint<Var> =
    CumulativeConstructor<Var, TimeTablePerPointPropagator<Var>>;

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTablePerPointIncrementalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
#[allow(unused)]
pub(crate) type TimeTablePerPointIncremental<Var> =
    CumulativeConstructor<Var, TimeTablePerPointIncrementalPropagator<Var>>;

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTableOverIntervalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
#[allow(unused)]
pub(crate) type TimeTableOverInterval<Var> =
    CumulativeConstructor<Var, TimeTableOverIntervalPropagator<Var>>;

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTableOverIntervalIncrementalPropagator`]; this is used to prevent creating a
/// different `new` method for each type `T`
pub(crate) type TimeTableOverIntervalIncremental<Var> =
    CumulativeConstructor<Var, TimeTableOverIntervalIncrementalPropagator<Var>>;

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
    updates: Vec<Vec<UpdateType<Var>>>,
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
            updates: vec![vec![]; parameters.tasks.len()],
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

    pub(crate) fn pop_next_update_for_task(
        &mut self,
        updated_task: &Rc<Task<Var>>,
    ) -> Option<UpdateType<Var>> {
        // TODO: this could take quadratic time, refactor
        if self.updates[updated_task.id.unpack() as usize].is_empty() {
            return None;
        }
        Some(self.updates[updated_task.id.unpack() as usize].remove(0))
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

    pub(crate) fn reset_all_bounds_and_remove_fixed(
        &mut self,
        context: &PropagationContext,
        parameters: &CumulativeParameters<Var>,
    ) {
        for task in parameters.tasks.iter() {
            self.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ));
            if context.is_fixed(&task.start_variable) {
                self.unfixed_tasks.remove(task);
            }
        }
    }

    pub(crate) fn reset_all_bounds(
        &mut self,
        context: &PropagationContext,
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

    pub(crate) fn insert_update_for_task(&mut self, task: &Rc<Task<Var>>, update: UpdateType<Var>) {
        self.updates[task.id.unpack() as usize].push(update);
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

    pub(crate) fn clean_updated(&mut self) {
        while let Some(updated_task) = self.pop_next_updated_task() {
            self.updates[updated_task.id.unpack() as usize].clear();
        }
    }
}

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The known bounds
/// - The values which have been updated since the previous propagation
#[derive(Debug, Clone)]
pub(crate) struct CumulativeParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: i32,
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub(crate) allow_holes_in_domain: bool,
}

#[derive(Debug, Clone)]
pub(crate) enum UpdateType<Var> {
    Addition(UpdatedTaskInfo<Var>),
    Removal(UpdatedTaskInfo<Var>),
}

impl<Var: IntegerVariable + 'static> CumulativeParameters<Var> {
    pub(crate) fn new(
        tasks: Vec<Task<Var>>,
        capacity: i32,
        allow_holes_in_domain: bool,
    ) -> CumulativeParameters<Var> {
        let tasks = tasks
            .into_iter()
            .map(Rc::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();

        CumulativeParameters {
            tasks: tasks.clone(),
            capacity,
            allow_holes_in_domain,
        }
    }
}
