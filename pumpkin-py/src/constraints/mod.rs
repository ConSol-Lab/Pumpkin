mod globals;

use globals::AllDifferent;
use pyo3::prelude::*;

#[derive(Clone, FromPyObject)]
pub enum Constraint {
    AllDifferent(AllDifferent),
}

impl pumpkin_lib::constraints::Constraint for Constraint {
    fn post(
        self,
        solver: &mut pumpkin_lib::Solver,
        tag: Option<std::num::NonZero<u32>>,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        match self {
            Constraint::AllDifferent(constraint) => constraint.post(solver, tag),
        }
    }

    fn implied_by(
        self,
        solver: &mut pumpkin_lib::Solver,
        reification_literal: pumpkin_lib::variables::Literal,
        tag: Option<std::num::NonZero<u32>>,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        match self {
            Constraint::AllDifferent(constraint) => {
                constraint.implied_by(solver, reification_literal, tag)
            }
        }
    }
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<AllDifferent>()?;
    Ok(())
}
