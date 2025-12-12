use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

/// Provides access to domain information to propagators.
///
/// Implements [`ReadDomains`] to expose information about the current variable domains such as the
/// lower-bound of a particular variable.
#[derive(Clone, Copy, Debug)]
pub struct Domains<'a> {
    pub(crate) assignments: &'a Assignments,
}

impl<'a> Domains<'a> {
    pub(crate) fn new(assignments: &'a Assignments) -> Self {
        Domains { assignments }
    }
}

/// A helper-trait for implementing [`ReadDomains`], which exposes the assignment.
pub(crate) trait HasAssignments {
    fn assignments(&self) -> &Assignments;
}

impl HasAssignments for Domains<'_> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }
}

pub trait ReadDomains {
    fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool>;

    fn is_predicate_satisfied(&self, predicate: Predicate) -> bool;

    fn is_predicate_falsified(&self, predicate: Predicate) -> bool;

    fn is_literal_true(&self, literal: &Literal) -> bool;

    fn is_literal_false(&self, literal: &Literal) -> bool;

    fn is_literal_fixed(&self, literal: &Literal) -> bool;

    /// Returns the holes which were created on the current decision level.
    fn get_holes_at_current_checkpoint<Var: IntegerVariable>(
        &self,
        var: &Var,
    ) -> impl Iterator<Item = i32>;

    /// Returns all of the holes (currently) in the domain of `var` (including ones which were
    /// created at previous decision levels).
    fn get_holes<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32>;

    /// Returns `true` if the domain of the given variable is singleton.
    fn is_fixed<Var: IntegerVariable>(&self, var: &Var) -> bool;

    fn lower_bound<Var: IntegerVariable>(&self, var: &Var) -> i32;

    fn lower_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32;

    fn upper_bound<Var: IntegerVariable>(&self, var: &Var) -> i32;

    fn upper_bound_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        trail_position: usize,
    ) -> i32;

    fn contains<Var: IntegerVariable>(&self, var: &Var, value: i32) -> bool;

    fn contains_at_trail_position<Var: IntegerVariable>(
        &self,
        var: &Var,
        value: i32,
        trail_position: usize,
    ) -> bool;

    fn iterate_domain<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32>;
}

impl<T: HasAssignments> ReadDomains for T {
    fn evaluate_predicate(&self, predicate: Predicate) -> Option<bool> {
        self.assignments().evaluate_predicate(predicate)
    }

    fn is_predicate_satisfied(&self, predicate: Predicate) -> bool {
        self.assignments()
            .evaluate_predicate(predicate)
            .is_some_and(|truth_value| truth_value)
    }

    fn is_predicate_falsified(&self, predicate: Predicate) -> bool {
        self.assignments()
            .evaluate_predicate(predicate)
            .is_some_and(|truth_value| !truth_value)
    }

    fn is_literal_true(&self, literal: &Literal) -> bool {
        literal
            .get_integer_variable()
            .lower_bound(self.assignments())
            == 1
    }

    fn is_literal_false(&self, literal: &Literal) -> bool {
        literal
            .get_integer_variable()
            .upper_bound(self.assignments())
            == 0
    }

    fn is_literal_fixed(&self, literal: &Literal) -> bool {
        self.is_fixed(literal)
    }

    /// Returns the holes which were created on the current decision level.
    fn get_holes_at_current_checkpoint<Var: IntegerVariable>(
        &self,
        var: &Var,
    ) -> impl Iterator<Item = i32> {
        var.get_holes_at_current_checkpoint(self.assignments())
    }

    /// Returns all of the holes (currently) in the domain of `var` (including ones which were
    /// created at previous decision levels).
    fn get_holes<Var: IntegerVariable>(&self, var: &Var) -> impl Iterator<Item = i32> {
        var.get_holes(self.assignments())
    }

    /// Returns `true` if the domain of the given variable is singleton.
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
}
