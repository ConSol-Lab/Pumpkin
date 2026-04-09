use pumpkin_core::ConstraintOperationError;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagators::nogoods::NogoodPropagator;
use pumpkin_core::state::PropagatorHandle;
use pumpkin_core::state::State;
use pumpkin_core::variables::Literal;

/// Creates the [`NegatableConstraint`] `\/ predicate`
///
/// Its negation is `/\ !predicate`
pub fn clause(
    literals: impl IntoIterator<Item = Predicate>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
) -> impl NegatableConstraint {
    Clause {
        literals: literals.into_iter().collect(),
        constraint_tag,
        propagator_handle,
    }
}

/// Creates the [`NegatableConstraint`] `/\ predicate`
///
/// Its negation is `\/ !predicate`
pub fn conjunction(
    literals: impl IntoIterator<Item = Predicate>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
) -> impl NegatableConstraint {
    Conjunction {
        literals: literals.into_iter().collect(),
        constraint_tag,
        propagator_handle,
    }
}

struct Clause {
    literals: Vec<Predicate>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
}

impl Constraint for Clause {
    fn post(self, state: &mut State) {
        let Clause {
            literals,
            constraint_tag,
            propagator_handle,
        } = self;

        let propagator = state
            .get_propagator_mut(propagator_handle)
            .expect("handle does not point to existing nogood propagator");

        // Since we are adding this constraint as a nogood, every literal is negated.
        propagator.add_nogood(
            literals.into_iter().map(|predicate| !predicate),
            constraint_tag,
        );
    }

    fn implied_by(self, state: &mut State, reification_literal: Literal) {
        let Clause {
            literals,
            constraint_tag,
            propagator_handle,
        } = self;

        clause(
            literals
                .into_iter()
                .chain(std::iter::once(reification_literal.get_false_predicate())),
            constraint_tag,
            propagator_handle,
        )
        .post(state)
    }
}

impl NegatableConstraint for Clause {
    type NegatedConstraint = Conjunction;

    fn negation(&self) -> Self::NegatedConstraint {
        let Clause {
            literals,
            constraint_tag,
            propagator_handle,
        } = self;

        Conjunction {
            literals: literals.iter().map(|&lit| !lit).collect(),
            constraint_tag: *constraint_tag,
            propagator_handle: *propagator_handle,
        }
    }
}

struct Conjunction {
    literals: Vec<Predicate>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
}

impl Constraint for Conjunction {
    fn post(self, state: &mut State) {
        let Conjunction {
            literals,
            constraint_tag,
            propagator_handle,
        } = self;

        for literal in literals {
            clause([literal], constraint_tag, propagator_handle).post(state);
        }
    }

    fn implied_by(self, state: &mut State, reification_literal: Literal) {
        let Conjunction {
            literals,
            constraint_tag,
            propagator_handle,
        } = self;

        for literal in literals {
            clause(
                [reification_literal.get_false_predicate(), literal],
                constraint_tag,
                propagator_handle,
            )
            .post(state);
        }
    }
}

impl NegatableConstraint for Conjunction {
    type NegatedConstraint = Clause;

    fn negation(&self) -> Self::NegatedConstraint {
        let Conjunction {
            literals,
            constraint_tag,
            propagator_handle,
        } = self;

        Clause {
            literals: literals.iter().map(|&lit| !lit).collect(),
            constraint_tag: *constraint_tag,
            propagator_handle: *propagator_handle,
        }
    }
}
