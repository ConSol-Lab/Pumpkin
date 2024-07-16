use std::ops::Not;

use enumset::EnumSet;

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

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct Literal {
    predicate: Predicate,
}

impl Literal {
    pub fn new(predicate: Predicate) -> Literal {
        Literal { predicate }
    }
}

impl Not for Literal {
    type Output = Literal;

    fn not(self) -> Self::Output {
        Literal {
            predicate: !self.predicate,
        }
    }
}

impl From<Literal> for Predicate {
    fn from(value: Literal) -> Self {
        value.predicate
    }
}

impl IntegerVariable for Literal {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    /// Literals that evaluate to true have a lower bound of 1.
    /// Literal that evaluate to false have a lower bound of 0.
    /// Unassigned literals have a lower bound of 0.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        match assignment.evaluate_predicate(self.predicate) {
            Some(truth_value) => truth_value as i32,
            None => 0,
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        match assignment.evaluate_predicate_at_trail_position(self.predicate, trail_position) {
            Some(truth_value) => truth_value as i32,
            None => 0,
        }
    }

    /// Returns the upper bound represented as a 0-1 value.
    /// Literals that evaluate to true have an upper bound of 1.
    /// Literal that evaluate to false have a upper bound of 0.
    /// Unassigned literals have a upper bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        match assignment.evaluate_predicate(self.predicate) {
            Some(truth_value) => truth_value as i32,
            None => 1,
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        match assignment.evaluate_predicate_at_trail_position(self.predicate, trail_position) {
            Some(truth_value) => truth_value as i32,
            None => 1,
        }
    }

    /// Returns whether the input value, when interpreted as a bool,
    /// can be considered for the literal.
    /// Literals that evaluate to true only contain value 1.
    /// Literals that evaluate to false only contain value 0.
    /// Unassigned literals contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        match assignment.evaluate_predicate(self.predicate) {
            Some(truth_value) => truth_value && value == 1 || !truth_value && value == 0,
            None => value == 1 || value == 0,
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        match assignment.evaluate_predicate_at_trail_position(self.predicate, trail_position) {
            Some(truth_value) => truth_value && value == 1 || !truth_value && value == 0,
            None => value == 1 || value == 0,
        }
    }

    fn describe_domain(&self, _assignment: &Assignments) -> Vec<Predicate> {
        unimplemented!();
    }

    fn remove(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        // Note that removing a value that is neither zero nor one does nothing.
        match value {
            0 => assignment.post_predicate(self.predicate, reason),
            1 => assignment.post_predicate(!self.predicate, reason),
            _ => Ok(()),
        }
    }

    fn set_lower_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if value <= 0 {
            // Do nothing, since literals always have lower bound of zero.
            Ok(())
        } else if value == 1 {
            assignment.post_predicate(self.predicate, reason)
        } else {
            // Otherwise the bound surpasses one, so the domain gets empty.
            Err(EmptyDomain)
        }
    }

    fn set_upper_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if value >= 1 {
            // Do nothing, the upper bound is always at mos one.
            Ok(())
        } else if value == 0 {
            assignment.post_predicate(!self.predicate, reason)
        } else {
            // Otherwise the bound goes below zero, so the domain is empty.
            Err(EmptyDomain)
        }
    }

    fn watch_all(&self, _watchers: &mut Watchers<'_>, _events: EnumSet<IntDomainEvent>) {
        unimplemented!()
        // watchers.watch_all(*self, events);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent {
        event.unwrap()
    }
}

impl PredicateConstructor for Literal {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound <= 0 {
            Predicate::trivially_true()
        } else if bound == 1 {
            self.predicate
        } else {
            Predicate::trivially_false()
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound >= 1 {
            Predicate::trivially_true()
        } else if bound == 0 {
            !self.predicate
        } else {
            Predicate::trivially_false()
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            0 => !self.predicate,
            1 => self.predicate,
            _ => Predicate::trivially_false(),
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            0 => self.predicate,
            1 => !self.predicate,
            _ => Predicate::trivially_true(),
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
