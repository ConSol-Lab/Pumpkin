mod globals;

use globals::*;
use pyo3::prelude::*;

macro_rules! declare_constraint_enum {
    ($name:ident { $($constraint:ident),+ $(,)? }) => {
        #[derive(Clone, FromPyObject)]
        pub enum $name {
            $($constraint($constraint)),+
        }

        impl pumpkin_lib::constraints::Constraint for Constraint {
            fn post(
                self,
                solver: &mut pumpkin_lib::Solver,
                tag: Option<std::num::NonZero<u32>>,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                match self {
                    $($name::$constraint(cns) => cns.post(solver, tag)),+
                }
            }

            fn implied_by(
                self,
                solver: &mut pumpkin_lib::Solver,
                reification_literal: pumpkin_lib::variables::Literal,
                tag: Option<std::num::NonZero<u32>>,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                match self {
                    $($name::$constraint(cns) => cns.implied_by(solver, reification_literal, tag)),+
                }
            }
        }
    };
}

declare_constraint_enum! {
    Constraint {
        AllDifferent,
        Cumulative,
    }
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<AllDifferent>()?;
    Ok(())
}
