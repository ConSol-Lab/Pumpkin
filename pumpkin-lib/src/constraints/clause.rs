use std::num::NonZero;

use super::Constraint;
use super::NegatableConstraint;
use crate::predicates::Predicate;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// Creates the [`NegatableConstraint`] `\/ literal`
///
/// Its negation is `/\ !literal`
pub fn clause(literals: impl Into<Vec<Literal>>) -> impl NegatableConstraint {
    Clause(literals.into())
}

/// Creates the [`NegatableConstraint`] `/\ literal`
///
/// Its negation is `\/ !literal`
pub fn conjunction(literals: impl Into<Vec<Literal>>) -> impl NegatableConstraint {
    Conjunction(literals.into())
}

struct Clause(Vec<Literal>);

impl Constraint for Clause {
    fn post(
        self,
        solver: &mut Solver,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        assert!(tag.is_none(), "tagging clauses is not implemented");

        solver.add_clause(self.0.iter().map(|&literal| literal.into()))
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        assert!(tag.is_none(), "tagging clauses is not implemented");

        solver.add_clause(
            self.0
                .into_iter()
                .chain(std::iter::once(!reification_literal))
                .map(Predicate::from),
        )
    }
}

impl NegatableConstraint for Clause {
    type NegatedConstraint = Conjunction;

    fn negation(&self) -> Self::NegatedConstraint {
        Conjunction(self.0.iter().map(|&lit| !lit).collect())
    }
}

struct Conjunction(Vec<Literal>);

impl Constraint for Conjunction {
    fn post(
        self,
        solver: &mut Solver,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        assert!(tag.is_none(), "tagging clauses is not implemented");

        self.0
            .into_iter()
            .try_for_each(|lit| solver.add_clause([Predicate::from(lit)]))
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        assert!(tag.is_none(), "tagging clauses is not implemented");

        self.0
            .into_iter()
            .try_for_each(|lit| solver.add_clause([(!reification_literal).into(), lit.into()]))
    }
}

impl NegatableConstraint for Conjunction {
    type NegatedConstraint = Clause;

    fn negation(&self) -> Self::NegatedConstraint {
        Clause(self.0.iter().map(|&lit| !lit).collect())
    }
}
