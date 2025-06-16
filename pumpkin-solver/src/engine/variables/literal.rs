use std::ops::Not;

use enumset::EnumSet;

use super::DomainId;
use super::IntegerVariable;
use super::TransformableVariable;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::variables::AffineView;
use crate::engine::Assignments;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Literal(Predicate);

impl Literal {
    pub(crate) fn new(predicate: Predicate) -> Literal {
        Literal(predicate)
    }

    #[cfg(test)]
    pub fn test_new(domain_id: DomainId) -> Literal {
        todo!()
    }

    /// Get the underlying [`Predicate`].
    pub fn to_predicate(self) -> Predicate {
        self.0
    }
}

impl Not for Literal {
    type Output = Literal;

    fn not(self) -> Self::Output {
        Literal(!self.0)
    }
}

impl From<Predicate> for Literal {
    fn from(value: Predicate) -> Self {
        Literal(value)
    }
}

impl IntegerVariable for Literal {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    /// Literals that evaluate to true have a lower bound of 1.
    /// Literal that evaluate to false have a lower bound of 0.
    /// Unassigned literals have a lower bound of 0.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        let value = assignment.evaluate_predicate(self.0);

        match value {
            Some(true) => 1,
            Some(false) | None => 0,
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        let value = assignment.evaluate_predicate_at_trail_position(self.0, trail_position);

        match value {
            Some(true) => 1,
            Some(false) | None => 0,
        }
    }

    /// Returns the upper bound represented as a 0-1 value.
    /// Literals that evaluate to true have an upper bound of 1.
    /// Literal that evaluate to false have a upper bound of 0.
    /// Unassigned literals have a upper bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        let value = assignment.evaluate_predicate(self.0);

        match value {
            Some(false) => 0,
            Some(true) | None => 1,
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        let value = assignment.evaluate_predicate_at_trail_position(self.0, trail_position);

        match value {
            Some(false) => 0,
            Some(true) | None => 1,
        }
    }

    /// Returns whether the input value, when interpreted as a bool,
    /// can be considered for the literal.
    /// Literals that evaluate to true only contain value 1.
    /// Literals that evaluate to false only contain value 0.
    /// Unassigned literals contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        let truth_value = assignment.evaluate_predicate(self.0);

        match truth_value {
            Some(true) => value == 1,
            Some(false) => value == 0,
            None => value == 0 || value == 1,
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        let truth_value = assignment.evaluate_predicate_at_trail_position(self.0, trail_position);

        match truth_value {
            Some(true) => value == 1,
            Some(false) => value == 0,
            None => value == 0 || value == 1,
        }
    }

    fn iterate_domain<'a>(&self, assignment: &'a Assignments) -> impl Iterator<Item = i32> + 'a {
        let truth_value = assignment.evaluate_predicate(self.0);

        LiteralDomainIterator::new(truth_value)
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_predicate(self.0);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        todo!()
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        todo!()
    }
}

impl PredicateConstructor for Literal {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        todo!()
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        todo!()
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        todo!()
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        todo!()
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

struct LiteralDomainIterator {
    truth_value: Option<bool>,
    next_value: i32,
}

impl LiteralDomainIterator {
    fn new(truth_value: Option<bool>) -> Self {
        LiteralDomainIterator {
            truth_value,
            next_value: 0,
        }
    }
}

impl Iterator for LiteralDomainIterator {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
