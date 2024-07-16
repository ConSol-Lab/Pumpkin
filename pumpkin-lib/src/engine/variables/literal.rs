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
    /// For other values, the function will panic.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        assert!(
            value == 0 || value == 1,
            "Literals can only be asked whether they contain zero or one values."
        );

        match assignment.evaluate_predicate(self.predicate) {
            Some(truth_value) => {
                // We rely on having the input value being restricted to zero or one.
                truth_value && value == 1 || !truth_value && value == 0
            }
            None => {
                // Since zero and one are the only options, then we simply return true since the
                // truth value of the predicate has not yet been determined.
                true
            }
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        assert!(
            value == 0 || value == 1,
            "Literals can only be asked whether they contain zero or one values."
        );

        match assignment.evaluate_predicate_at_trail_position(self.predicate, trail_position) {
            Some(truth_value) => {
                // We rely on having the input value being restricted to zero or one.
                truth_value && value == 1 || !truth_value && value == 0
            }
            None => {
                // Since zero and one are the only options, then we simply return true since the
                // truth value of the predicate has not yet been determined.
                true
            }
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
        match value {
            0 => assignment.post_predicate(self.predicate, reason),
            1 => assignment.post_predicate(!self.predicate, reason),
            _ => panic!("Literals can only be asked to remove zero or one values."),
        }
    }

    fn set_lower_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        match value {
            0 => {
                // do nothing, since literals always have lower bound of zero.
                Ok(())
            }
            1 => assignment.post_predicate(self.predicate, reason),
            _ => panic!("Literals can only be asked to set lower bounds to either zero or one."),
        }
    }

    fn set_upper_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        match value {
            0 => assignment.post_predicate(!self.predicate, reason),
            1 => {
                // do nothing, since literals always have an upper bound of one.
                Ok(())
            }
            _ => panic!("Literals can only be asked to set upper bounds to either zero or one."),
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
        assert!(bound == 0 || bound == 1);
        match bound {
            0 => Predicate::trivially_true(),
            1 => self.predicate,
            _ => {
                panic!(
                    "Lower bound predicate for literal must be restricted to zero or one values."
                )
            }
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        assert!(bound == 0 || bound == 1);
        match bound {
            0 => !self.predicate,
            1 => Predicate::trivially_false(),
            _ => {
                panic!(
                    "Upper bound predicate for literal must be restricted to zero or one values."
                )
            }
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        assert!(bound == 0 || bound == 1);
        match bound {
            0 => !self.predicate,
            1 => self.predicate,
            _ => panic!("Equality predicate for literal must be restricted to zero or one values."),
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        assert!(bound == 0 || bound == 1);
        match bound {
            0 => self.predicate,
            1 => !self.predicate,
            _ => {
                panic!("Not equals predicate for literal must be restricted to zero or one values.")
            }
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
