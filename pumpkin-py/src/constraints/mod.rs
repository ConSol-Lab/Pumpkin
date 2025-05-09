pub mod arguments;
pub mod globals;

use arguments::PythonConstraintArg;
use globals::*;
use pumpkin_solver::proof::ConstraintTag;
use pyo3::prelude::*;

use crate::variables::VariableMap;

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

#[derive(Clone, Copy)]
#[pyclass]
pub struct Tag(ConstraintTag);

impl From<ConstraintTag> for Tag {
    fn from(value: ConstraintTag) -> Self {
        Tag(value)
    }
}

impl From<Tag> for ConstraintTag {
    fn from(value: Tag) -> Self {
        value.0
    }
}

impl PythonConstraintArg for Tag {
    type Output = ConstraintTag;

    fn to_solver_constraint_argument(self, _: &VariableMap) -> Self::Output {
        self.0
    }
}
