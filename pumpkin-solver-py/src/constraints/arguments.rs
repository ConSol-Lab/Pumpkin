use pumpkin_core::variables::AffineView;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::Literal;

use crate::variables::BoolExpression;
use crate::variables::IntExpression;
use crate::variables::VariableMap;

/// Trait which helps to convert Python API types to the solver types when creating constraints.
pub trait PythonConstraintArg {
    type Output;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output;
}

impl PythonConstraintArg for IntExpression {
    type Output = AffineView<DomainId>;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output {
        self.to_affine_view(variable_map)
    }
}

impl PythonConstraintArg for BoolExpression {
    type Output = Literal;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output {
        self.to_literal(variable_map)
    }
}

impl PythonConstraintArg for i32 {
    type Output = i32;

    fn to_solver_constraint_argument(self, _: &VariableMap) -> Self::Output {
        self
    }
}

impl<Arg: PythonConstraintArg> PythonConstraintArg for Vec<Arg> {
    type Output = Vec<Arg::Output>;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output {
        self.into_iter()
            .map(|arg| arg.to_solver_constraint_argument(variable_map))
            .collect()
    }
}
