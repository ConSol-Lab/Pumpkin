use pumpkin_lib::variables::AffineView;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::Literal;

use crate::variables::BoolVariable;
use crate::variables::IntVariable;
use crate::variables::VariableMap;

pub trait PythonConstraintArg {
    type Output;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output;
}

impl PythonConstraintArg for IntVariable {
    type Output = AffineView<DomainId>;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output {
        variable_map.get_integer(self)
    }
}

impl PythonConstraintArg for BoolVariable {
    type Output = Literal;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output {
        variable_map.get_boolean(self)
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
