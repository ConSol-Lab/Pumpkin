use std::num::NonZero;

use super::binary_not_equals;
use super::Constraint;
use crate::variables::IntegerVariable;
use crate::variables::Literal;
use crate::ConstraintOperationError;
use crate::Solver;

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct.
pub fn all_different<Var: IntegerVariable + Clone + 'static>(
    variables: impl Into<Box<[Var]>>,
) -> impl Constraint {
    let variables: Box<[Var]> = variables.into();

    AllDifferent { variables }
}

#[derive(Clone, Debug)]
struct AllDifferent<Var> {
    variables: Box<[Var]>,
}

impl<Var> Constraint for AllDifferent<Var>
where
    Var: IntegerVariable + Clone + 'static,
{
    fn post(
        &self,
        solver: &mut Solver,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        for i in 0..self.variables.len() {
            for j in i + 1..self.variables.len() {
                binary_not_equals(self.variables[i].clone(), self.variables[j].clone())
                    .post(solver, tag)?;
            }
        }

        Ok(())
    }

    fn implied_by(
        &self,
        solver: &mut Solver,
        reification_literal: Literal,
        tag: Option<NonZero<u32>>,
    ) -> Result<(), ConstraintOperationError> {
        for i in 0..self.variables.len() {
            for j in i + 1..self.variables.len() {
                binary_not_equals(self.variables[i].clone(), self.variables[j].clone())
                    .implied_by(solver, reification_literal, tag)?;
            }
        }

        Ok(())
    }

    fn boxed_clone(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}
