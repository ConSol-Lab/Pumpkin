use std::rc::Rc;

use crate::{
    containers::{KeyedVec, SparseSet},
    engine::propagation::LocalId,
    propagators::{CumulativePropagatorOptions, CumulativeStatistics},
};

use super::variable_task::VariableTask;

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The options for propagating the cumulative constraint
#[derive(Debug, Clone)]
pub(crate) struct VariableCumulativeParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<VariableTask<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: Var,
    /// The [`CumulativeOptions`] which influence the behaviour of the cumulative propagator(s).
    pub(crate) options: CumulativePropagatorOptions,
    pub(crate) mapping: KeyedVec<LocalId, usize>,
}

/// Structures which are adjusted during search; either due to incrementality or to keep track of
/// bounds.
#[derive(Debug, Clone)]
pub(crate) struct VariableUpdatableStructures<Var> {
    /// The current known bounds of the different [tasks][CumulativeParameters::tasks]; stored as
    /// (lower bound, upper bound)
    ///
    /// `bounds[i]` represents the currently known bounds of task i
    bounds: Vec<(i32, i32)>,
    pub(crate) unfixed_tasks: SparseSet<Rc<VariableTask<Var>>>,

    pub(crate) statistics: CumulativeStatistics,
}
