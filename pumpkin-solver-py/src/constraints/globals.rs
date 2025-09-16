use pumpkin_solver::constraint_arguments::ArgTask;
use pumpkin_solver::constraints::Constraint;
use pumpkin_solver::constraints::{self};
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

#[pyclass]
#[derive(Clone)]
pub struct Cumulative {
    start_times: Vec<IntExpression>,
    durations: Vec<IntExpression>,
    resource_usages: Vec<IntExpression>,
    resource_capacity: IntExpression,
    constraint_tag: Tag,
}

#[pymethods]
impl Cumulative {
    #[new]
    pub fn new(
        start_times: Vec<IntExpression>,
        durations: Vec<IntExpression>,
        resource_usages: Vec<IntExpression>,
        resource_capacity: IntExpression,
        constraint_tag: Tag,
    ) -> Self {
        Self {
            constraint_tag,
            start_times,
            durations,
            resource_usages,
            resource_capacity,
        }
    }
}

impl Cumulative {
    pub fn post(
        self,
        solver: &mut pumpkin_solver::Solver,
        variable_map: &VariableMap,
    ) -> Result<(), pumpkin_solver::ConstraintOperationError> {
        let tasks = self
            .start_times
            .into_iter()
            .zip(self.durations.iter())
            .zip(self.resource_usages.iter())
            .map(|((start_time, processing_time), resource_usage)| {
                let start_time = start_time.to_solver_constraint_argument(variable_map);
                let processing_time = processing_time.to_solver_constraint_argument(variable_map);
                let resource_usage = resource_usage.to_solver_constraint_argument(variable_map);
                ArgTask {
                    start_time,
                    processing_time,
                    resource_usage,
                }
            })
            .collect::<Vec<_>>();
        constraints::cumulative(
            tasks,
            self.resource_capacity
                .to_solver_constraint_argument(variable_map),
            self.constraint_tag.0,
        )
        .post(solver)
    }

    pub fn implied_by(
        self,
        solver: &mut pumpkin_solver::Solver,
        reification_literal: pumpkin_solver::variables::Literal,
        variable_map: &VariableMap,
    ) -> Result<(), pumpkin_solver::ConstraintOperationError> {
        let tasks = self
            .start_times
            .into_iter()
            .zip(self.durations.iter())
            .zip(self.resource_usages.iter())
            .map(|((start_time, processing_time), resource_usage)| {
                let start_time = start_time.to_solver_constraint_argument(variable_map);
                let processing_time = processing_time.to_solver_constraint_argument(variable_map);
                let resource_usage = resource_usage.to_solver_constraint_argument(variable_map);
                ArgTask {
                    start_time,
                    processing_time,
                    resource_usage,
                }
            })
            .collect::<Vec<_>>();
        constraints::cumulative(
            tasks,
            self.resource_capacity
                .to_solver_constraint_argument(variable_map),
            self.constraint_tag.0,
        )
        .implied_by(solver, reification_literal)
    }
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
