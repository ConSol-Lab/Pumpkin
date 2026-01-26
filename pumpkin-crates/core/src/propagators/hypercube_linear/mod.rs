mod hypercube;
mod linear;

pub use hypercube::*;
pub use linear::*;

use crate::basic_types::PredicateId;
use crate::declare_inference_label;
use crate::predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::results::PropagationStatusCP;
use crate::state::Conflict;
use crate::state::PropagatorConflict;

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub struct HypercubeLinearConstructor {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
    pub constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for HypercubeLinearConstructor {
    type PropagatorImpl = HypercubeLinearPropagator;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        } = self;

        let hypercube_predicates = hypercube
            .iter_predicates()
            .map(|predicate| context.register_predicate(predicate))
            .collect();

        HypercubeLinearPropagator {
            hypercube_predicates,
            linear,
            constraint_tag,
        }
    }
}

/// A [`Propagator`] for the hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct HypercubeLinearPropagator {
    linear: LinearInequality,

    hypercube_predicates: Box<[PredicateId]>,
    constraint_tag: ConstraintTag,
}

declare_inference_label!(HypercubeLinear);

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        let is_hypercube_satisfied = self
            .hypercube_predicates
            .iter()
            .all(|&predicate_id| context.is_predicate_id_satisfied(predicate_id));

        if !is_hypercube_satisfied {
            return Ok(());
        }

        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| i64::from(context.lower_bound(&term)))
            .sum::<i64>();

        if lower_bound_terms > i64::from(self.linear.bound()) {
            let conjunction = self
                .linear
                .terms()
                .map(|term| predicate![term >= context.lower_bound(&term)])
                .collect();

            Err(Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code: InferenceCode::new(self.constraint_tag, HypercubeLinear),
            }))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;
    use crate::predicate;
    use crate::state::State;

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
        )
        .expect("not trivially false");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }
}
