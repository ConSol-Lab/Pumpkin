pub(crate) mod arguments;
pub(crate) mod globals;

use globals::*;
use pyo3::prelude::*;

macro_rules! declare_constraints {
    ($name:ident { $($constraint:ident),+ $(,)? }) => {
        #[derive(Clone, FromPyObject)]
        pub(crate) enum $name {
            $($constraint($constraint)),+
        }

        impl Constraint {
            pub(crate) fn post(
                self,
                solver: &mut pumpkin_solver::Solver,
            ) {
                match self {
                    $($name::$constraint(cns) => cns.post(solver)),+
                }
            }

            pub(crate) fn implied_by(
                self,
                solver: &mut pumpkin_solver::Solver,
                reification_literal: pumpkin_solver::core::variables::Literal,
            ) {
                match self {
                    $($name::$constraint(cns) => cns.implied_by(solver, reification_literal)),+
                }
            }
        }

        pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
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
        Table,
        NegativeTable,
    }
}
