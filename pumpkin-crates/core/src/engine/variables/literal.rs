use std::fmt::Debug;
use std::ops::Not;

use enumset::EnumSet;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use super::IntegerVariable;
use super::TransformableVariable;
use crate::engine::Assignments;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::variables::AffineView;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Literal {
    pub(crate) inner: Predicate,
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl Literal {
    /// Creates a new literal wrapping the provided [`Predicate`].
    ///
    /// Note: the provided `domain_id` should have a domain between 0 and 1.
    pub fn new(predicate: Predicate) -> Literal {
        Literal { inner: predicate }
    }

    pub fn get_true_predicate(&self) -> Predicate {
        self.inner
    }

    pub fn get_false_predicate(&self) -> Predicate {
        !self.inner
    }
}

impl Not for Literal {
    type Output = Literal;

    fn not(self) -> Self::Output {
        Literal { inner: !self.inner }
    }
}

impl CheckerVariable<Predicate> for Literal {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        atomic.get_domain() == self.inner.get_domain()
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        if value == 0 {
            !self.inner
        } else if value >= 1 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        if value == 1 {
            !self.inner
        } else if value <= 0 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        if value == 1 {
            self.inner
        } else if value == 0 {
            !self.inner
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        if value == 1 {
            !self.inner
        } else if value == 0 {
            self.inner
        } else {
            Predicate::trivially_true()
        }
    }

    fn induced_lower_bound(&self, variable_state: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(if variable_state.is_true(&self.inner) {
            1
        } else {
            Default::default()
        })
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(if variable_state.is_true(&!self.inner) {
            0
        } else {
            1
        })
    }

    fn induced_fixed_value(&self, variable_state: &VariableState<Predicate>) -> Option<i32> {
        if variable_state.is_true(&self.inner) {
            Some(1)
        } else if variable_state.is_true(&!self.inner) {
            Some(0)
        } else {
            None
        }
    }

    fn induced_domain_contains(
        &self,
        variable_state: &VariableState<Predicate>,
        value: i32,
    ) -> bool {
        if value == 1 && !variable_state.is_true(&!self.inner) {
            true
        } else {
            value == 0 && !variable_state.is_true(&self.inner)
        }
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        _variable_state: &'state VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        std::iter::empty()
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        Some((0..=1).filter(|&value| {
            if value == 0 {
                !variable_state.is_true(&self.inner)
            } else if value == 1 {
                !variable_state.is_true(&!self.inner)
            } else {
                unreachable!()
            }
        }))
    }
}

impl IntegerVariable for Literal {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    /// Literals that evaluate to true have a lower bound of 1.
    /// Literal that evaluate to false have a lower bound of 0.
    /// Unassigned literals have a lower bound of 0.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        if assignment.is_predicate_satisfied(self.inner) {
            1
        } else {
            0
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if assignment.is_predicate_satisfied_at_trail_position(self.inner, trail_position) {
            1
        } else {
            0
        }
    }

    /// Returns the upper bound represented as a 0-1 value.
    /// Literals that evaluate to true have an upper bound of 1.
    /// Literal that evaluate to false have a upper bound of 0.
    /// Unassigned literals have a upper bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        if assignment.is_predicate_satisfied(!self.inner) {
            0
        } else {
            1
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if assignment.is_predicate_satisfied_at_trail_position(!self.inner, trail_position) {
            0
        } else {
            1
        }
    }

    /// Returns whether the input value, when interpreted as a bool,
    /// can be considered for the literal.
    /// Literals that evaluate to true only contain value 1.
    /// Literals that evaluate to false only contain value 0.
    /// Unassigned literals contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        if value == 0 {
            !assignment.is_predicate_satisfied(self.inner)
        } else if value == 1 {
            !assignment.is_predicate_satisfied(!self.inner)
        } else {
            false
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        if value == 0 {
            !assignment.is_predicate_satisfied_at_trail_position(self.inner, trail_position)
        } else if value == 1 {
            !assignment.is_predicate_satisfied_at_trail_position(!self.inner, trail_position)
        } else {
            false
        }
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        (0..=1).filter(|&value| {
            if value == 0 {
                !assignment.is_predicate_satisfied(self.inner)
            } else if value == 1 {
                !assignment.is_predicate_satisfied(!self.inner)
            } else {
                unreachable!()
            }
        })
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_literal(*self, events)
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        watchers.unwatch_predicate(self.inner);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        event.unwrap()
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_literal_backtrack(*self, events)
    }

    fn get_holes_at_current_checkpoint(
        &self,
        _assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }

    fn get_holes(&self, _assignments: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }
}

impl PredicateConstructor for Literal {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 1 {
            self.inner
        } else if bound < 1 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 0 {
            !self.inner
        } else if bound > 0 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 0 {
            !self.inner
        } else if bound == 1 {
            self.inner
        } else {
            Predicate::trivially_true()
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 0 {
            self.inner
        } else if bound == 1 {
            !self.inner
        } else {
            Predicate::trivially_true()
        }
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
