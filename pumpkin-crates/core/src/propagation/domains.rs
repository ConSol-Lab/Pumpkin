use crate::engine::Assignments;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::ExplanationContext;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

/// Provides access to domain information to propagators.
///
/// Implements [`ReadDomains`] to expose information about the current variable domains such as the
/// lower-bound of a particular variable.
#[derive(Debug)]
pub struct Domains<'a> {
    pub(crate) assignments: &'a Assignments,
    trailed_values: &'a mut TrailedValues,
}

impl<'a> Domains<'a> {
    pub fn new(assignments: &'a Assignments, trailed_values: &'a mut TrailedValues) -> Self {
        Domains {
            assignments,
            trailed_values,
        }
    }

    pub fn reborrow(&mut self) -> Domains<'_> {
        Domains::new(self.assignments, self.trailed_values)
    }
}

/// A helper-trait for implementing [`ReadDomains`], which exposes the assignment.
pub(crate) trait HasAssignments {
    fn assignments(&self) -> &Assignments;
    fn trailed_values(&self) -> &TrailedValues;
    fn trailed_values_mut(&mut self) -> &mut TrailedValues;
}

impl HasAssignments for Domains<'_> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }

    fn trailed_values(&self) -> &TrailedValues {
        self.trailed_values
    }

    fn trailed_values_mut(&mut self) -> &mut TrailedValues {
        self.trailed_values
    }
}

/// A trait defining functions for retrieving information about the current domains.
pub trait ReadDomains {
    /// Returns whether the provided [`Predicate`] is assigned (either true or false) or is
    /// currently unassigned.
    fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool>;

    /// Returns whether the provided [`Literal`] is assigned (either true or false) or is
    /// currently unassigned.
    fn evaluate_literal(&self, literal: Literal) -> Option<bool>;

    /// Returns the holes in the domain which were created on the current checkpoint.
    fn get_holes_at_current_checkpoint<Var: IntegerVariable>(
        &self,
        var: &Var,
    ) -> impl Iterator<Item = i32>;

    /// Returns all of the holes (currently) in the domain of `var` (including ones which were
    /// created at previous decision levels).
    fn get_holes<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32>;

    /// Returns `true` if the domain of the given variable is singleton (i.e., the variable is
    /// fixed).
    fn is_fixed<Var: IntegerVariable>(&self, var: &Var) -> bool;

    /// Returns the lowest value in the domain of `var`.
    fn lower_bound<Var: IntegerVariable>(&self, var: &Var) -> i32;

    /// Returns the lowest value in the domain of `var` at the given `trail_position`.
    ///
    /// The trail position can be retrieved when generating lazy explanations using
    /// [`ExplanationContext::get_trail_position`].
    fn lower_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32;

    /// Returns the highest value in the domain of `var`.
    fn upper_bound<Var: IntegerVariable>(&self, var: &Var) -> i32;

    /// Returns the highest value in the domain of `var` at the given `trail_position`.
    ///
    /// The trail position can be retrieved when generating lazy explanations using
    /// [`ExplanationContext::get_trail_position`].
    fn upper_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32;

    /// Returns whether the provided `value` is in the domain of `var`.
    fn contains<Var: IntegerVariable>(&self, var: &Var, value: i32) -> bool;

    /// Returns whether the provided `value` is in the domain of `var` at the given
    /// `trail_position`.
    ///
    /// The trail position can be retrieved when generating lazy explanations using
    /// [`ExplanationContext::get_trail_position`].
    fn contains_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        value: i32,
        trail_position: usize,
    ) -> bool;

    /// Returns an [`Iterator`] over the values in the domain of the provided `var` (including the
    /// lower-bound and upper-bound values).
    fn iterate_domain<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32>;

    /// Returns whether the provided [`Predicate`] was posted as a decision (i.e., it was posted as
    /// a [`Predicate`] without a reason).
    fn is_decision_predicate(&self, predicate: Predicate) -> bool;

    /// If the provided [`Predicate`] is true, then this method returns the checkpoint at which it
    /// first become true; otherwise, it returns [`None`].
    fn get_checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize>;

    /// Returns the current value of the provided [`TrailedInteger`].
    fn read_trailed_integer(&self, trailed_integer: TrailedInteger) -> i64;

    /// Creates a new [`TrailedInteger`] assigned to the provided `initial_value`.
    fn new_trailed_integer(&mut self, initial_value: i64) -> TrailedInteger;

    /// Assigns the provided [`TrailedInteger`] to the provided `value`.
    fn write_trailed_integer(&mut self, trailed_integer: TrailedInteger, value: i64);

    /// Returns the current checkpoint.
    fn get_checkpoint(&self) -> usize;
}

impl<T: HasAssignments> ReadDomains for T {
    fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool> {
        self.assignments().evaluate_predicate(predicate)
    }

    fn evaluate_literal(&self, literal: Literal) -> Option<bool> {
        self.evaluate_predicate(literal.get_true_predicate())
    }

    fn get_holes_at_current_checkpoint<Var: IntegerVariable>(
        &self,
        var: &Var,
    ) -> impl Iterator<Item = i32> {
        var.get_holes_at_current_checkpoint(self.assignments())
    }

    fn get_holes<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32> {
        var.get_holes(self.assignments())
    }

    fn is_fixed<Var: IntegerVariable>(&self, var: &Var) -> bool {
        self.lower_bound(var) == self.upper_bound(var)
    }

    fn lower_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.lower_bound(self.assignments())
    }

    fn lower_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32 {
        var.lower_bound_at_trail_position(self.assignments(), trail_position)
    }

    fn upper_bound<Var: IntegerVariable>(&self, var: &Var) -> i32 {
        var.upper_bound(self.assignments())
    }

    fn upper_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32 {
        var.upper_bound_at_trail_position(self.assignments(), trail_position)
    }

    fn contains<Var: IntegerVariable>(&self, var: &Var, value: i32) -> bool {
        var.contains(self.assignments(), value)
    }

    fn contains_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        value: i32,
        trail_position: usize,
    ) -> bool {
        var.contains_at_trail_position(self.assignments(), value, trail_position)
    }

    fn iterate_domain<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32> {
        var.iterate_domain(self.assignments())
    }

    fn is_decision_predicate(&self, predicate: Predicate) -> bool {
        self.assignments().is_decision_predicate(&predicate)
    }

    fn get_checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize> {
        self.assignments().get_checkpoint_for_predicate(&predicate)
    }

    fn read_trailed_integer(&self, trailed_integer: TrailedInteger) -> i64 {
        self.trailed_values().read(trailed_integer)
    }

    fn new_trailed_integer(&mut self, initial_value: i64) -> TrailedInteger {
        self.trailed_values_mut().grow(initial_value)
    }

    fn write_trailed_integer(&mut self, trailed_integer: TrailedInteger, value: i64) {
        self.trailed_values_mut().assign(trailed_integer, value);
    }

    fn get_checkpoint(&self) -> usize {
        self.assignments().get_checkpoint()
    }
}
