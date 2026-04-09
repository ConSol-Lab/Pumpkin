use std::num::NonZero;

use crate::ConstraintOperationError;
use crate::Solver;
use crate::constraints::Constraint;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearConstructor;
use crate::hypercube_linear::LinearInequality;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::variables::DomainId;
use crate::variables::Literal;

pub fn hypercube_linear(
    hypercube: impl IntoIterator<Item = Predicate>,
    linear_terms: impl IntoIterator<Item = (NonZero<i32>, DomainId)>,
    linear_rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    HLConstraint {
        hypercube,
        linear_terms,
        linear_rhs,
        constraint_tag,
    }
}

struct HLConstraint<Predicates, LinearTerms> {
    hypercube: Predicates,
    linear_terms: LinearTerms,
    linear_rhs: i32,
    constraint_tag: ConstraintTag,
}

impl<Predicates, LinearTerms> Constraint for HLConstraint<Predicates, LinearTerms>
where
    Predicates: IntoIterator<Item = Predicate>,
    LinearTerms: IntoIterator<Item = (NonZero<i32>, DomainId)>,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let Ok(hypercube) = Hypercube::new(self.hypercube) else {
            // If the hypercube is inconsistent, then the constraint simplifies to
            // `false implies linear`, which is trivially true.
            return Ok(());
        };

        let Some(linear) = LinearInequality::new(self.linear_terms, self.linear_rhs) else {
            // If the linear is trivially satisfied, then there is no point in posting
            // a constraint.
            return Ok(());
        };

        let _ = solver.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag: self.constraint_tag,
        })?;

        Ok(())
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        hypercube_linear(
            self.hypercube
                .into_iter()
                .chain(std::iter::once(reification_literal.get_true_predicate())),
            self.linear_terms,
            self.linear_rhs,
            self.constraint_tag,
        )
        .post(solver)
    }
}
