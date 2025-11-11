use super::Constraint;
use super::NegatableConstraint;
use crate::ConstraintOperationError;
use crate::Solver;
use crate::proof::ConstraintTag;
use crate::variables::Literal;

/// Creates the [`NegatableConstraint`] `\/ literal`
///
/// Its negation is `/\ !literal`
pub fn clause(
    literals: impl Into<Vec<Literal>>,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    Clause(literals.into(), constraint_tag)
}

/// Creates the [`NegatableConstraint`] `/\ literal`
///
/// Its negation is `\/ !literal`
pub fn conjunction(
    literals: impl Into<Vec<Literal>>,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    Conjunction(literals.into(), constraint_tag)
}

struct Clause(Vec<Literal>, ConstraintTag);

impl Constraint for Clause {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let Clause(clause, constraint_tag) = self;

        solver.add_clause(
            clause.iter().map(|literal| literal.get_true_predicate()),
            constraint_tag,
        )
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let Clause(clause, constraint_tag) = self;

        solver.add_clause(
            clause
                .into_iter()
                .chain(std::iter::once(!reification_literal))
                .map(|literal| literal.get_true_predicate()),
            constraint_tag,
        )
    }
}

impl NegatableConstraint for Clause {
    type NegatedConstraint = Conjunction;

    fn negation(&self) -> Self::NegatedConstraint {
        let Clause(clause, constraint_tag) = self;

        Conjunction(clause.iter().map(|&lit| !lit).collect(), *constraint_tag)
    }
}

struct Conjunction(Vec<Literal>, ConstraintTag);

impl Constraint for Conjunction {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let Conjunction(conjunction, constraint_tag) = self;

        conjunction
            .into_iter()
            .try_for_each(|lit| solver.add_clause([lit.get_true_predicate()], constraint_tag))
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let Conjunction(conjunction, constraint_tag) = self;

        conjunction.into_iter().try_for_each(|lit| {
            solver.add_clause(
                [
                    (!(reification_literal)).get_true_predicate(),
                    lit.get_true_predicate(),
                ],
                constraint_tag,
            )
        })
    }
}

impl NegatableConstraint for Conjunction {
    type NegatedConstraint = Clause;

    fn negation(&self) -> Self::NegatedConstraint {
        let Conjunction(conjunction, constraint_tag) = self;

        Clause(
            conjunction.iter().map(|&lit| !lit).collect(),
            *constraint_tag,
        )
    }
}
