mod hypercube;
mod linear;

use crate::{
    basic_types::PredicateId,
    proof::ConstraintTag,
    propagation::{
        EnqueueDecision, LocalId, NotificationContext, OpaqueDomainEvent, PropagationContext,
        Propagator, PropagatorConstructor, PropagatorConstructorContext,
    },
    results::PropagationStatusCP,
};

pub use hypercube::*;
pub use linear::*;

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub struct HypercubeLinearConstructor {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
    pub constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for HypercubeLinearConstructor {
    type PropagatorImpl = HypercubeLinearPropagator;

    fn create(self, context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        todo!()
    }
}

/// A [`Propagator`] for the hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct HypercubeLinearPropagator {
    hypercube: Hypercube,
    hypercube_predicates: Box<[PredicateId]>,
    linear: LinearInequality,
}

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn notify(
        &mut self,
        _context: NotificationContext,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        todo!()
    }

    fn propagate(&mut self, context: PropagationContext) -> PropagationStatusCP {
        todo!()
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use crate::{predicate, state::State};

    use super::*;

    #[test]
    fn conflict_detected() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into()));
        let z = state.new_interval_variable(2, 5, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");
        // x + y + z <= 5.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            5,
        );

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }
}
