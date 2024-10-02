use pumpkin_lib::constraints::Constraint;
use pumpkin_lib::constraints::{self};
use pyo3::pyclass;
use pyo3::pymethods;

use super::arguments::PythonConstraintArg;
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
                solver: &mut pumpkin_lib::Solver,
                tag: Option<std::num::NonZero<u32>>,
                variable_map: &VariableMap,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field, variable_map)),+
                ).post(solver, tag)
            }

            pub fn implied_by(
                self,
                solver: &mut pumpkin_lib::Solver,
                reification_literal: pumpkin_lib::variables::Literal,
                tag: Option<std::num::NonZero<u32>>,
                variable_map: &VariableMap,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field, variable_map)),+
                ).implied_by(solver, reification_literal, tag)
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

#[pyclass]
#[derive(Clone)]
pub(crate) struct Clause {
    literals: Vec<BoolVariable>,
}

#[pymethods]
impl Clause {
    #[new]
    fn new(literals: Vec<BoolVariable>) -> Self {
        Clause { literals }
    }
}

impl Clause {
    pub fn post(
        self,
        solver: &mut pumpkin_lib::Solver,
        tag: Option<std::num::NonZero<u32>>,
        variable_map: &VariableMap,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        let clause = self.literals.to_solver_constraint_argument(variable_map);
        constraints::clause(clause).post(solver, tag)
    }

    pub fn implied_by(
        self,
        solver: &mut pumpkin_lib::Solver,
        reification_literal: pumpkin_lib::variables::Literal,
        tag: Option<std::num::NonZero<u32>>,
        variable_map: &VariableMap,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        let clause = self.literals.to_solver_constraint_argument(variable_map);
        constraints::clause(clause).implied_by(solver, reification_literal, tag)
    }
}

#[pyclass]
#[derive(Clone)]
pub(crate) struct Conjunction {
    literals: Vec<BoolVariable>,
}

#[pymethods]
impl Conjunction {
    #[new]
    fn new(literals: Vec<BoolVariable>) -> Self {
        Conjunction { literals }
    }
}

impl Conjunction {
    pub fn post(
        self,
        solver: &mut pumpkin_lib::Solver,
        tag: Option<std::num::NonZero<u32>>,
        variable_map: &VariableMap,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        let conjunction = self.literals.to_solver_constraint_argument(variable_map);
        constraints::conjunction(conjunction).post(solver, tag)
    }

    pub fn implied_by(
        self,
        solver: &mut pumpkin_lib::Solver,
        reification_literal: pumpkin_lib::variables::Literal,
        tag: Option<std::num::NonZero<u32>>,
        variable_map: &VariableMap,
    ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
        let conjunction = self.literals.to_solver_constraint_argument(variable_map);
        constraints::conjunction(conjunction).implied_by(solver, reification_literal, tag)
    }
}
