use std::fmt::Debug;
use std::fmt::Formatter;

use super::propagation::PropagatorId;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::Trail;
#[cfg(doc)]
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::debug_helper::DebugDyn;
use crate::engine::propagation::PropagationContext;
use crate::pumpkin_assert_simple;

/// The reason store holds a reason for each change made by a CP propagator on a trail.
///   This trail makes is easy to garbage collect reasons by simply synchronising whenever
///   the `AssignmentsInteger` and `AssignmentsPropositional` are synchronised.
#[derive(Default, Debug)]
pub struct ReasonStore {
    trail: Trail<(PropagatorId, Reason)>,
}

impl ReasonStore {
    pub fn push(&mut self, propagator: PropagatorId, reason: Reason) -> ReasonRef {
        let index = self.trail.len();
        self.trail.push((propagator, reason));
        pumpkin_assert_simple!(
            index < (1 << 30),
            "ReasonRef in reason store should fit in ContraintReference, \
             which has 30 bits available at most"
        );
        ReasonRef(index as u32)
    }

    pub fn get_or_compute<'this>(
        &'this mut self,
        reference: ReasonRef,
        context: &PropagationContext,
    ) -> Option<&'this PropositionalConjunction> {
        self.trail
            .get_mut(reference.0 as usize)
            .map(|reason| reason.1.compute(context))
    }

    pub fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub fn synchronise(&mut self, level: usize) {
        let _ = self.trail.synchronise(level);
    }

    #[cfg(test)]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.trail.len()
    }

    /// Get the propagator which generated the given reason.
    pub fn get_propagator(&self, reason_ref: ReasonRef) -> PropagatorId {
        self.trail.get(reason_ref.0 as usize).unwrap().0
    }
}

#[derive(Default, Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct ReasonRef(pub(crate) u32);

/// A reason for CP propagator to make a change
pub enum Reason {
    /// An eager reason contains the propositional conjunction with the reason, without the
    ///   propagated literal itself, which is added by the
    /// [`ConflictAnalysisContext`] later on.
    Eager(PropositionalConjunction),
    /// A lazy reason, which contains a closure that computes the reason later. Again, the
    ///   propagated literal is _not_ part of the reason but added by the
    /// [`ConflictAnalysisContext`]. Lazy reasons are typically computed
    /// only once, then replaced by an Eager version with the   result.
    Lazy(Box<dyn LazyReason>),
}

impl Debug for Reason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Reason::Eager(prop_conj) => f.debug_tuple("Eager").field(prop_conj).finish(),
            Reason::Lazy(_) => f
                .debug_tuple("Lazy")
                .field(&DebugDyn::from("Reason"))
                .finish(),
        }
    }
}

/// A lazy reason, which contains a closure that computes the reason later.
pub trait LazyReason {
    /// The computation receives  a read-only context that provides information about the
    ///   assignments at the time of computing the reason, not from the time when the change was
    ///   made. The CP propagator must compute and save any required information in the closure if
    ///   dependent on the state of the assignments at that time.
    fn compute(self: Box<Self>, context: &PropagationContext) -> PropositionalConjunction;
}

impl<F: FnOnce(&PropagationContext) -> PropositionalConjunction> LazyReason for F {
    fn compute(self: Box<Self>, context: &PropagationContext) -> PropositionalConjunction {
        self(context)
    }
}

impl Reason {
    /// Compute the reason for a propagation, replacing the original reason with an `Eager` one if
    ///   it's `Lazy`.
    pub fn compute(&mut self, context: &PropagationContext) -> &PropositionalConjunction {
        // You can't just (1) match on the reason to see if it's Lazy, (2) use it to compute a new
        //   result, and (3) then change the Lazy into an Eager, because you'll still be borrowing
        //   the closure.
        //   Instead, we first unconditionally mem::replace the reason with an eager one, so the
        //   original is moved to a local variable, then match on that original, and put the result
        //   into the eager one.
        let reason = std::mem::replace(self, Reason::Eager(Default::default()));
        // Get a &mut to the field in Eager to put the result there.
        let Reason::Eager(result) = self else {
            // (this branch gets optimised out)
            unreachable!()
        };
        match reason {
            Reason::Eager(prop_conj) => *result = prop_conj,
            Reason::Lazy(f) => *result = f.compute(context),
        }
        result
    }
}

impl From<PropositionalConjunction> for Reason {
    fn from(value: PropositionalConjunction) -> Self {
        Reason::Eager(value)
    }
}

impl<F: FnOnce(&PropagationContext) -> PropositionalConjunction + 'static> From<F> for Reason {
    fn from(value: F) -> Self {
        Reason::Lazy(Box::new(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::variables::DomainId;
    use crate::engine::AssignmentsInteger;
    use crate::engine::AssignmentsPropositional;

    #[test]
    fn computing_an_eager_reason_returns_a_reference_to_the_conjunction() {
        let integers = AssignmentsInteger::default();
        let booleans = AssignmentsPropositional::default();
        let context = PropagationContext::new(&integers, &booleans);

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let mut reason = Reason::Eager(conjunction.clone());

        assert_eq!(&conjunction, reason.compute(&context));
    }

    #[test]
    fn computing_a_lazy_reason_evaluates_the_reason_and_returns_a_reference() {
        let integers = AssignmentsInteger::default();
        let booleans = AssignmentsPropositional::default();
        let context = PropagationContext::new(&integers, &booleans);

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let conjunction_to_return = conjunction.clone();
        let mut reason = Reason::from(|_: &PropagationContext| conjunction_to_return);

        assert_eq!(&conjunction, reason.compute(&context));
    }

    #[test]
    fn pushing_a_reason_gives_a_reason_ref_that_can_be_computed() {
        let mut reason_store = ReasonStore::default();
        let integers = AssignmentsInteger::default();
        let booleans = AssignmentsPropositional::default();
        let context = PropagationContext::new(&integers, &booleans);

        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conjunction = conjunction!([x == 1] & [y == 2]);
        let reason_ref = reason_store.push(PropagatorId(0), Reason::Eager(conjunction.clone()));

        assert_eq!(ReasonRef(0), reason_ref);

        assert_eq!(
            Some(&conjunction),
            reason_store.get_or_compute(reason_ref, &context)
        );
    }
}
