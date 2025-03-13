use enum_map::Enum;

use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
#[cfg(doc)]
use crate::basic_types::Random;
use crate::basic_types::SolutionReference;
#[cfg(doc)]
use crate::branching;
#[cfg(doc)]
use crate::branching::branchers::dynamic_brancher::DynamicBrancher;
#[cfg(doc)]
use crate::branching::value_selection::ValueSelector;
#[cfg(doc)]
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
#[cfg(doc)]
use crate::create_statistics_struct;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::engine::WatchListManager;
#[cfg(doc)]
use crate::results::solution_iterator::SolutionIterator;
use crate::statistics::StatisticLogger;
#[cfg(doc)]
use crate::Solver;

/// A trait for definining a branching strategy (oftentimes utilising a [`VariableSelector`] and a
/// [`ValueSelector`]).
///
/// In general, implementations of this trait define how the search of the solver proceeds (i.e. it
/// controls how the solver determines which part of the search space to explore). It is required
/// that the resulting decision creates a smaller domain for at least 1 of the variables (and more
/// domains can be affected due to subsequent inference). See [`branching`] for
/// example usages.
///
/// If the [`Brancher`] (or any component thereof) is implemented incorrectly then the
/// behaviour of the solver is undefined.
pub trait Brancher {
    /// Logs statistics of the brancher using the provided [`StatisticLogger`].
    ///
    /// It is recommended to create a struct through the [`create_statistics_struct!`] macro!
    fn log_statistics(&self, _statistic_logger: StatisticLogger) {}

    /// Returns the next decision concerning a single variable and value; it returns the
    /// [`Predicate`] corresponding to this decision (or [`None`] if all variables under
    /// consideration are assigned).
    ///
    /// Note that this method **cannot** perform the assignment of the decision, it should only
    /// return a suggestion in the form of a [`Predicate`]; the [`SelectionContext`] is
    /// only mutable to account for the usage of random generators (e.g. see [`Random`]).
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate>;

    /// A function which is called after a conflict has been found and processed but (currently)
    /// does not provide any additional information.
    ///
    /// To receive information about this event, use [`BrancherEvent::Conflict`] in
    /// [`Self::subscribe_to_events`]
    fn on_conflict(&mut self) {}

    /// A function which is called whenever a backtrack occurs in the [`Solver`].
    ///
    /// To receive information about this event, use [`BrancherEvent::Backtrack`] in
    /// [`Self::subscribe_to_events`]
    fn on_backtrack(&mut self) {}

    /// This method is called when a solution is found; this will either be called when a new
    /// incumbent solution is found (i.e. a solution with a better objective value than previously
    /// known) or when a new solution is found when iterating over solutions using
    /// [`SolutionIterator`].
    ///
    /// To receive information about this event, use [`BrancherEvent::Solution`] in
    /// [`Self::subscribe_to_events`]
    fn on_solution(&mut self, _solution: SolutionReference) {}

    /// A function which is called after a [`DomainId`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `variable` which is the
    /// [`DomainId`] which has been reset and `value` which is the value to which the variable was
    /// previously fixed. This method could thus be called multiple times in a single
    /// backtracking operation by the solver.
    ///
    /// To receive information about this event, use [`BrancherEvent::UnassignInteger`] in
    /// [`Self::subscribe_to_events`]
    fn on_unassign_integer(&mut self, _variable: DomainId, _value: i32) {}

    /// A function which is called when a [`Predicate`] appears in a conflict during conflict
    /// analysis.
    ///
    /// To receive information about this event, use
    /// [`BrancherEvent::AppearanceInConflictPredicate`] in [`Self::subscribe_to_events`]
    fn on_appearance_in_conflict_predicate(
        &mut self,
        _predicate: Predicate,
        _watch_lists: &mut WatchListManager,
        _predicate_id_generator: &mut PredicateIdGenerator,
    ) {
    }

    /// This method is called whenever a restart is performed.
    /// To receive information about this event, use [`BrancherEvent::Restart`] in
    /// [`Self::subscribe_to_events`]
    fn on_restart(&mut self) {}

    /// Called after backtracking.
    /// Used to reset internal data structures to account for the backtrack.
    ///
    /// To receive information about this event, use [`BrancherEvent::Synchronise`] in
    /// [`Self::subscribe_to_events`]
    fn synchronise(
        &mut self,
        _assignments: &Assignments,
        _predicate_id_generator: &mut PredicateIdGenerator,
    ) {
    }

    /// This method returns whether a restart is *currently* pointless for the [`Brancher`].
    ///
    /// For example, if a [`Brancher`] is using a static search strategy then a restart is
    /// pointless; however, if a [`Brancher`] is using a variable selector which
    /// changes throughout the search process then restarting is not pointless.
    ///
    /// Note that even if the [`Brancher`] has indicated that a restart is pointless, it could be
    /// that the restart is still performed (e.g. if this [`Brancher`] is a subcomponent of another
    /// [`Brancher`] and it is not the only `is_restart_pointless` response which is taken into
    /// account).
    fn is_restart_pointless(&mut self) -> bool {
        true
    }

    fn notify_predicate(
        &mut self,
        _predicate: PredicateId,
        _predicate_id_generator: &mut PredicateIdGenerator,
        _value: bool,
    ) {
    }

    /// Indicates which [`BrancherEvent`] are relevant for this particular [`Brancher`].
    ///
    /// This can be used by [`Brancher::subscribe_to_events`] to determine upon which
    /// events which [`VariableSelector`] should be called.
    fn subscribe_to_events(&self) -> Vec<BrancherEvent>;
}

impl<T: Brancher> Brancher for Option<T> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        if let Some(brancher) = self {
            brancher.next_decision(context)
        } else {
            None
        }
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        if let Some(brancher) = self {
            brancher.subscribe_to_events()
        } else {
            vec![]
        }
    }

    fn on_conflict(&mut self) {
        if let Some(brancher) = self {
            brancher.on_conflict()
        }
    }

    fn on_backtrack(&mut self) {
        if let Some(brancher) = self {
            brancher.on_backtrack()
        }
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        if let Some(brancher) = self {
            brancher.on_solution(_solution)
        }
    }

    fn on_unassign_integer(&mut self, _variable: DomainId, _value: i32) {
        if let Some(brancher) = self {
            brancher.on_unassign_integer(_variable, _value)
        }
    }

    fn on_appearance_in_conflict_predicate(
        &mut self,
        _predicate: Predicate,
        _watch_lists: &mut WatchListManager,
        _predicate_id_generator: &mut PredicateIdGenerator,
    ) {
        if let Some(brancher) = self {
            brancher.on_appearance_in_conflict_predicate(
                _predicate,
                _watch_lists,
                _predicate_id_generator,
            );
        }
    }

    fn on_restart(&mut self) {
        if let Some(brancher) = self {
            brancher.on_restart()
        }
    }

    fn synchronise(
        &mut self,
        _assignments: &Assignments,
        _predicate_id_generator: &mut PredicateIdGenerator,
    ) {
        if let Some(brancher) = self {
            brancher.synchronise(_assignments, _predicate_id_generator)
        }
    }

    fn is_restart_pointless(&mut self) -> bool {
        if let Some(brancher) = self {
            brancher.is_restart_pointless()
        } else {
            true
        }
    }

    fn notify_predicate(
        &mut self,
        _predicate: PredicateId,
        _predicate_id_generator: &mut PredicateIdGenerator,
        _value: bool,
    ) {
        if let Some(brancher) = self {
            brancher.notify_predicate(_predicate, _predicate_id_generator, _value)
        }
    }
}

/// The events which can occur for a [`Brancher`]. Used for returning which events are relevant in
/// [`Brancher::subscribe_to_events`], [`VariableSelector::subscribe_to_events`],
/// and [`ValueSelector::subscribe_to_events`].
#[derive(Debug, Clone, Copy, Enum, Hash, PartialEq, Eq)]
pub enum BrancherEvent {
    /// Event for when a conflict is detected
    Conflict,
    /// Event for when a backtrack is performed
    Backtrack,
    /// Event for when a solution has been found
    Solution,
    /// Event for when an integer variable has become unassigned
    UnassignInteger,
    /// Event for when a predicate appears during conflict analysis
    AppearanceInConflictPredicate,
    /// Event for when a restart occurs
    Restart,
    /// Event which is called with the new state after a backtrack has occurred
    Synchronise,
    NotifyPredicate,
}
