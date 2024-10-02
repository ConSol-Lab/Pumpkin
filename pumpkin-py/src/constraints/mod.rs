pub mod arguments;
mod globals;

use globals::*;
use pyo3::prelude::*;

macro_rules! declare_constraints {
    ($name:ident { $($constraint:ident),+ $(,)? }) => {
        #[derive(Clone, FromPyObject)]
        pub enum $name {
            $($constraint($constraint)),+
        }

        impl Constraint {
            pub fn post(
                self,
                solver: &mut pumpkin_lib::Solver,
                tag: Option<std::num::NonZero<u32>>,
                variable_map: &$crate::variables::VariableMap,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                match self {
                    $($name::$constraint(cns) => cns.post(solver, tag, variable_map)),+
                }
            }

            pub fn implied_by(
                self,
                solver: &mut pumpkin_lib::Solver,
                reification_literal: pumpkin_lib::variables::Literal,
                tag: Option<std::num::NonZero<u32>>,
                variable_map: &$crate::variables::VariableMap,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                match self {
                    $($name::$constraint(cns) => cns.implied_by(solver, reification_literal, tag, variable_map)),+
                }
            }
        }

        pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
            $(m.add_class::<$constraint>()?;)+
            Ok(())
        }
    };
}

declare_constraints! {
    Constraint {
        Absolute,
        AllDifferent,
        BinaryEquals,
        BinaryLessThanEqual,
        BinaryLessThan,
        BinaryNotEquals,
        Cumulative,
        Division,
        Element,
        Equals,
        LessThanOrEquals,
        Maximum,
        Minimum,
        NotEquals,
        Plus,
        Times,
        Clause,
        Conjunction,
    }
}
