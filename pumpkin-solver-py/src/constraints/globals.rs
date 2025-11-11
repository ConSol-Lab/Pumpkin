use pumpkin_solver::constraint_arguments::ArgTask;
use pumpkin_solver::constraints::Constraint;
use pumpkin_solver::constraints::{self};
use pumpkin_solver::variables::AffineView;
use pumpkin_solver::variables::DomainId;
use pyo3::pyclass;
use pyo3::pymethods;

use crate::constraints::arguments::PythonConstraintArg;
use crate::model::Tag;
use crate::variables::*;

macro_rules! python_constraint {
    ($name:ident : $constraint_func:ident { $($field:ident : $type:ty),+ $(,)? }) => {
        #[pyclass]
        #[derive(Clone)]
        pub(crate) struct $name {
            constraint_tag: Tag,
            $($field: $type),+
        }

        #[pymethods]
        impl $name {
            #[new]
            fn new($($field: $type),+ , constraint_tag: Tag) -> Self {
                $name {
                    constraint_tag,
                    $($field),+
                }
            }
        }

        impl $name {
            pub fn post(
                self,
                solver: &mut pumpkin_solver::Solver,
                variable_map: &VariableMap,
            ) -> Result<(), pumpkin_solver::ConstraintOperationError> {
                constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field, variable_map)),+ ,
                    self.constraint_tag.0,
                ).post(solver)
            }

            pub fn implied_by(
                self,
                solver: &mut pumpkin_solver::Solver,
                reification_literal: pumpkin_solver::variables::Literal,
                variable_map: &VariableMap,
            ) -> Result<(), pumpkin_solver::ConstraintOperationError> {
                constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field, variable_map)),+ ,
                    self.constraint_tag.0,
                ).implied_by(solver, reification_literal)
            }
        }
    };
}

python_constraint! {
    Absolute: absolute {
        signed: IntExpression,
        absolute: IntExpression,
    }
}

python_constraint! {
    AllDifferent: all_different {
        variables: Vec<IntExpression>,
    }
}

python_constraint! {
    BinaryEquals: binary_equals {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    BinaryLessThan: binary_less_than {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    BinaryLessThanEqual: binary_less_than_or_equals {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    BinaryNotEquals: binary_not_equals {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    Division: division {
        numerator: IntExpression,
        denominator: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    Element: element {
        index: IntExpression,
        array: Vec<IntExpression>,
        rhs: IntExpression,
    }
}

python_constraint! {
    Equals: equals {
        terms: Vec<IntExpression>,
        rhs: i32,
    }
}

python_constraint! {
    LessThanOrEquals: less_than_or_equals {
        terms: Vec<IntExpression>,
        rhs: i32,
    }
}

python_constraint! {
    Maximum: maximum {
        choices: Vec<IntExpression>,
        rhs: IntExpression,
    }
}

python_constraint! {
    Minimum: minimum {
        choices: Vec<IntExpression>,
        rhs: IntExpression,
    }
}

python_constraint! {
    NotEquals: not_equals {
        terms: Vec<IntExpression>,
        rhs: i32,
    }
}

python_constraint! {
    Plus: plus {
        a: IntExpression,
        b: IntExpression,
        c: IntExpression,
    }
}

python_constraint! {
    Times: times {
        a: IntExpression,
        b: IntExpression,
        c: IntExpression,
    }
}

python_constraint! {
    Conjunction: conjunction {
        literals: Vec<BoolExpression>,
    }
}

python_constraint! {
    Clause: clause {
        literals: Vec<BoolExpression>,
    }
}

python_constraint! {
    Table: table {
        variables: Vec<IntExpression>,
        table: Vec<Vec<i32>>,
    }
}

python_constraint! {
    NegativeTable: negative_table {
        variables: Vec<IntExpression>,
        table: Vec<Vec<i32>>,
    }
}

python_constraint! {
    Cumulative: cumulative {
        tasks: Vec<Task>,
        capacity: i32
    }
}

#[pyclass(eq, hash, frozen)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Task {
    start_time: IntExpression,
    processing_time: i32,
    resource_usage: i32,
}

#[pymethods]
impl Task {
    #[new]
    fn new(start_time: IntExpression, processing_time: i32, resource_usage: i32) -> Self {
        Self {
            start_time,
            processing_time,
            resource_usage,
        }
    }
}

impl PythonConstraintArg for Task {
    type Output = ArgTask<AffineView<DomainId>, i32, i32>;

    fn to_solver_constraint_argument(self, variable_map: &VariableMap) -> Self::Output {
        ArgTask {
            start_time: self.start_time.to_solver_constraint_argument(variable_map),
            processing_time: self.processing_time,
            resource_usage: self.resource_usage,
        }
    }
}
