use pumpkin_solver::core::constraints::Constraint;
use pyo3::pyclass;
use pyo3::pymethods;

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
            ) -> Result<(), pumpkin_solver::core::ConstraintOperationError> {
                pumpkin_constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field)),+ ,
                    self.constraint_tag.0,
                ).post(solver)
            }

            pub fn implied_by(
                self,
                solver: &mut pumpkin_solver::Solver,
                reification_literal: pumpkin_solver::core::variables::Literal,
            ) -> Result<(), pumpkin_solver::core::ConstraintOperationError> {
                pumpkin_constraints::$constraint_func(
                    $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field)),+ ,
                    self.constraint_tag.0,
                ).implied_by(solver, reification_literal)
            }
        }
    };
}

python_constraint! {
    Absolute: absolute {
        signed: IntegerVariableWrapper,
        absolute: IntegerVariableWrapper,
    }
}

python_constraint! {
    AllDifferent: all_different {
        variables: Vec<IntegerVariableWrapper>,
    }
}

python_constraint! {
    BinaryEquals: binary_equals {
        lhs: IntegerVariableWrapper,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    BinaryLessThan: binary_less_than {
        lhs: IntegerVariableWrapper,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    BinaryLessThanEqual: binary_less_than_or_equals {
        lhs: IntegerVariableWrapper,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    BinaryNotEquals: binary_not_equals {
        lhs: IntegerVariableWrapper,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    Cumulative: cumulative {
        start_times: Vec<IntegerVariableWrapper>,
        durations: Vec<i32>,
        resource_requirements: Vec<i32>,
        resource_capacity: i32,
    }
}

python_constraint! {
    Division: division {
        numerator: IntegerVariableWrapper,
        denominator: IntegerVariableWrapper,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    Element: element {
        index: IntegerVariableWrapper,
        array: Vec<IntegerVariableWrapper>,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    Equals: equals {
        terms: Vec<IntegerVariableWrapper>,
        rhs: i32,
    }
}

python_constraint! {
    LessThanOrEquals: less_than_or_equals {
        terms: Vec<IntegerVariableWrapper>,
        rhs: i32,
    }
}

python_constraint! {
    Maximum: maximum {
        choices: Vec<IntegerVariableWrapper>,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    Minimum: minimum {
        choices: Vec<IntegerVariableWrapper>,
        rhs: IntegerVariableWrapper,
    }
}

python_constraint! {
    NotEquals: not_equals {
        terms: Vec<IntegerVariableWrapper>,
        rhs: i32,
    }
}

python_constraint! {
    Plus: plus {
        a: IntegerVariableWrapper,
        b: IntegerVariableWrapper,
        c: IntegerVariableWrapper,
    }
}

python_constraint! {
    Times: times {
        a: IntegerVariableWrapper,
        b: IntegerVariableWrapper,
        c: IntegerVariableWrapper,
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
        variables: Vec<IntegerVariableWrapper>,
        table: Vec<Vec<i32>>,
    }
}

python_constraint! {
    NegativeTable: negative_table {
        variables: Vec<IntegerVariableWrapper>,
        table: Vec<Vec<i32>>,
    }
}
