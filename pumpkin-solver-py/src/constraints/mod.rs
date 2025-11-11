pub mod arguments;
pub mod globals;

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
                solver: &mut pumpkin_solver::Solver,
                variable_map: &$crate::variables::VariableMap,
            ) -> Result<(), pumpkin_solver::ConstraintOperationError> {
                match self {
                    $($name::$constraint(cns) => cns.post(solver, variable_map)),+
                }
            }

            pub fn implied_by(
                self,
                solver: &mut pumpkin_solver::Solver,
                reification_literal: pumpkin_solver::variables::Literal,
                variable_map: &$crate::variables::VariableMap,
            ) -> Result<(), pumpkin_solver::ConstraintOperationError> {
                match self {
                    $($name::$constraint(cns) => cns.implied_by(solver, reification_literal, variable_map)),+
                }
            }
        }

        pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
            $(m.add_class::<$constraint>()?;)+
            m.add_class::<Task>()?;
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
        Table,
        NegativeTable,
    }
}
