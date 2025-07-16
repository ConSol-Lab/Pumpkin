use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::{self};
use pyo3::pyclass;
use pyo3::pymethods;

use crate::variables::*;

macro_rules! python_constraint {
    ($name:ident : $constraint_func:ident { $($field:ident : $type:ty),+ $(,)? }) => {
        #[pyclass]
        #[derive(Clone)]
        pub(crate) struct $name {
            $($field: $type),+
        }

        #[pymethods]
        impl $name {
            #[new]
            fn new($($field: $type),+) -> Self {
                $name {
                    $($field),+
                }
            }
        }

        impl $name {
            pub fn post(
                self,
                solver: &mut pumpkin_core::Solver,
                variable_map: &VariableMap,
            ) -> Result<(), pumpkin_core::ConstraintOperationError> {
                let cs = solver.new_constraint_tag();

                constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field, variable_map)),+
                    , cs,
                ).post(solver)
            }

            pub fn implied_by(
                self,
                solver: &mut pumpkin_core::Solver,
                reification_literal: pumpkin_core::variables::Literal,
                variable_map: &VariableMap,
            ) -> Result<(), pumpkin_core::ConstraintOperationError> {
                let cs = solver.new_constraint_tag();

                constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field, variable_map)),+
                    , cs,
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
    Cumulative: cumulative {
        start_times: Vec<IntExpression>,
        durations: Vec<i32>,
        resource_requirements: Vec<i32>,
        resource_capacity: i32,
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
