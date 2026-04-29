use pumpkin_solver::core::predicates::Predicate;
use pumpkin_solver::core::variables::AnyInteger;

use crate::variables::BoolExpression;
use crate::variables::IntExpression;

/// Trait which helps to convert Python API types to the solver types when creating constraints.
pub trait PythonConstraintArg {
    type Output;

    fn to_solver_constraint_argument(self) -> Self::Output;
}

impl PythonConstraintArg for IntExpression {
    type Output = AnyInteger;

    fn to_solver_constraint_argument(self) -> Self::Output {
        self.0
    }
}

impl PythonConstraintArg for BoolExpression {
    type Output = Predicate;

    fn to_solver_constraint_argument(self) -> Self::Output {
        self.0
    }
}

impl PythonConstraintArg for i32 {
    type Output = i32;

    fn to_solver_constraint_argument(self) -> Self::Output {
        self
    }
}

impl<Arg: PythonConstraintArg> PythonConstraintArg for Vec<Arg> {
    type Output = Vec<Arg::Output>;

    fn to_solver_constraint_argument(self) -> Self::Output {
        self.into_iter()
            .map(|arg| arg.to_solver_constraint_argument())
            .collect()
    }
}
