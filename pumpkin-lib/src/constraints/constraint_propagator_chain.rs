use super::Constraint;
use super::NegatableConstraint;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

#[derive(Debug)]
pub struct ConstraintChainLink<Constraint, Next> {
    pub(crate) constraint: Constraint,
    pub(crate) next: Next,
}

impl<C, Next> Constraint for ConstraintChainLink<C, Next>
where
    C: Constraint,
    Next: Constraint,
{
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        self.constraint.post(solver)?;

        self.next.post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        self.constraint.implied_by(solver, reification_literal)?;

        self.next.implied_by(solver, reification_literal)
    }
}

impl<C, Next> NegatableConstraint for ConstraintChainLink<C, Next>
where
    C: NegatableConstraint,
    Next: NegatableConstraint,
{
    type NegatedConstraint = ConstraintChainLink<C::NegatedConstraint, Next::NegatedConstraint>;

    fn negation(&self) -> Self::NegatedConstraint {
        ConstraintChainLink {
            constraint: self.constraint.negation(),
            next: self.next.negation(),
        }
    }
}
