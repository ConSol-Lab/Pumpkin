use std::ops::Not;

use enumset::EnumSet;

use super::DomainId;
use super::IntegerVariable;
use super::TransformableVariable;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::reason::ReasonRef;
use crate::engine::variables::AffineView;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::IntDomainEvent;
use crate::engine::Watchers;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Literal {
    integer_variable: AffineView<DomainId>,
}

impl Literal {
    pub(crate) fn new(domain_id: DomainId) -> Literal {
        Literal {
            integer_variable: domain_id.scaled(1),
        }
    }

    pub fn domain_id(&self) -> DomainId {
        self.integer_variable.inner
    }

    #[cfg(test)]
    pub fn test_new(domain_id: DomainId) -> Literal {
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

    fn remove(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        self.integer_variable.remove(assignment, value, reason)
    }

    fn set_lower_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        self.integer_variable
            .set_lower_bound(assignment, value, reason)
    }

    fn set_upper_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        self.integer_variable
            .set_upper_bound(assignment, value, reason)
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>) {
        self.integer_variable.watch_all(watchers, events)
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent {
        self.integer_variable.unpack_event(event)
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>) {
        self.integer_variable.watch_all_backtrack(watchers, events)
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
