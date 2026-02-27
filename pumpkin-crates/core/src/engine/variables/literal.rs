use std::ops::Not;

use enumset::EnumSet;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use super::DomainId;
use super::IntegerVariable;
use super::TransformableVariable;
use crate::engine::Assignments;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::variables::AffineView;
use crate::propagation::checkers::ValueToWitness;
use crate::propagation::checkers::WitnessedVariable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Literal {
    integer_variable: AffineView<DomainId>,
}

impl Literal {
    /// Creates a new literal wrapping the provided [`DomainId`].
    ///
    /// Note: the provided `domain_id` should have a domain between 0 and 1.
    pub fn new(domain_id: DomainId) -> Literal {
        Literal {
            integer_variable: domain_id.scaled(1),
        }
    }

    pub fn get_integer_variable(&self) -> AffineView<DomainId> {
        self.integer_variable
    }

    pub fn get_true_predicate(&self) -> Predicate {
        self.lower_bound_predicate(1)
    }

    pub fn get_false_predicate(&self) -> Predicate {
        self.upper_bound_predicate(0)
    }
}

impl Not for Literal {
    type Output = Literal;

    fn not(self) -> Self::Output {
        Literal {
            integer_variable: self.integer_variable.scaled(-1).offset(1),
        }
    }
}

/// Forwards a function implementation to the field on self.
macro_rules! forward {
    (
        $field:ident,
        fn $(<$($lt:lifetime),+>)? $name:ident(
            & $($lt_self:lifetime)? self,
            $($param_name:ident : $param_type:ty),*
        ) -> $return_type:ty
        $(where $($where_clause:tt)*)?
    ) => {
        fn $name$(<$($lt),+>)?(
            & $($lt_self)? self,
            $($param_name: $param_type),*
        ) -> $return_type $(where $($where_clause)*)? {
            self.$field.$name($($param_name),*)
        }
    }
}

impl CheckerVariable<Predicate> for Literal {
    forward!(integer_variable, fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool);
    forward!(integer_variable, fn atomic_less_than(&self, value: i32) -> Predicate);
    forward!(integer_variable, fn atomic_greater_than(&self, value: i32) -> Predicate);
    forward!(integer_variable, fn atomic_not_equal(&self, value: i32) -> Predicate);
    forward!(integer_variable, fn atomic_equal(&self, value: i32) -> Predicate);

    forward!(integer_variable, fn induced_lower_bound(&self, variable_state: &VariableState<Predicate>) -> IntExt);
    forward!(integer_variable, fn induced_upper_bound(&self, variable_state: &VariableState<Predicate>) -> IntExt);
    forward!(integer_variable, fn induced_fixed_value(&self, variable_state: &VariableState<Predicate>) -> Option<i32>);
    forward!(integer_variable, fn induced_domain_contains(&self, variable_state: &VariableState<Predicate>, value: i32) -> bool);
    forward!(
        integer_variable,
        fn <'this, 'state> induced_holes(
            &'this self,
            variable_state: &'state VariableState<Predicate>
        ) -> impl Iterator<Item = i32> + 'state
        where
            'this: 'state,
    );
    forward!(
        integer_variable,
        fn <'this, 'state> iter_induced_domain(
            &'this self,
            variable_state: &'state VariableState<Predicate>
        ) -> Option<impl Iterator<Item = i32> + 'state>
        where
            'this: 'state,
    );
}

impl IntegerVariable for Literal {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    /// Literals that evaluate to true have a lower bound of 1.
    /// Literal that evaluate to false have a lower bound of 0.
    /// Unassigned literals have a lower bound of 0.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        self.integer_variable.lower_bound(assignment)
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        self.integer_variable
            .lower_bound_at_trail_position(assignment, trail_position)
    }

    /// Returns the upper bound represented as a 0-1 value.
    /// Literals that evaluate to true have an upper bound of 1.
    /// Literal that evaluate to false have a upper bound of 0.
    /// Unassigned literals have a upper bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        self.integer_variable.upper_bound(assignment)
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        self.integer_variable
            .upper_bound_at_trail_position(assignment, trail_position)
    }

    /// Returns whether the input value, when interpreted as a bool,
    /// can be considered for the literal.
    /// Literals that evaluate to true only contain value 1.
    /// Literals that evaluate to false only contain value 0.
    /// Unassigned literals contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        self.integer_variable.contains(assignment, value)
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        self.integer_variable
            .contains_at_trail_position(assignment, value, trail_position)
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        self.integer_variable.iterate_domain(assignment)
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        self.integer_variable.watch_all(watchers, events)
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        self.integer_variable.unwatch_all(watchers)
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        self.integer_variable.unpack_event(event)
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        self.integer_variable.watch_all_backtrack(watchers, events)
    }

    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        self.integer_variable
            .get_holes_at_current_checkpoint(assignments)
    }

    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32> {
        self.integer_variable.get_holes(assignments)
    }
}

impl PredicateConstructor for Literal {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        self.integer_variable.lower_bound_predicate(bound)
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        self.integer_variable.upper_bound_predicate(bound)
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        self.integer_variable.equality_predicate(bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        self.integer_variable.disequality_predicate(bound)
    }
}

impl TransformableVariable<AffineView<Literal>> for Literal {
    fn scaled(&self, scale: i32) -> AffineView<Literal> {
        AffineView::new(*self, scale, 0)
    }

    fn offset(&self, offset: i32) -> AffineView<Literal> {
        AffineView::new(*self, 1, offset)
    }
}

impl WitnessedVariable for Literal {
    fn unpack_value(&self, value: ValueToWitness) -> i32 {
        self.integer_variable.unpack_value(value)
    }

    fn assign(&self, value: i32) -> crate::propagation::checkers::SingleVariableAssignment {
        self.integer_variable.assign(value)
    }
}
