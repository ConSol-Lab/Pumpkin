use pumpkin_lib::constraints;
use pyo3::pyclass;
use pyo3::pymethods;

use crate::core::Variable;

macro_rules! python_constraint {
    ($name:ident : $constraint_func:ident { $($field:ident : $type:ty),+ $(,)? }) => {
        #[pyclass]
        #[derive(Clone)]
        pub struct $name {
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

        impl pumpkin_lib::constraints::Constraint for $name {
            fn post(
                self,
                solver: &mut pumpkin_lib::Solver,
                tag: Option<std::num::NonZero<u32>>,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                constraints::$constraint_func($(self.$field),+).post(solver, tag)
            }

            fn implied_by(
                self,
                solver: &mut pumpkin_lib::Solver,
                reification_literal: pumpkin_lib::variables::Literal,
                tag: Option<std::num::NonZero<u32>>,
            ) -> Result<(), pumpkin_lib::ConstraintOperationError> {
                constraints::$constraint_func($(self.$field),+).implied_by(solver, reification_literal, tag)
            }
        }
    };
}

python_constraint! {
    Absolute: absolute {
        signed: Variable,
        absolute: Variable,
    }
}

python_constraint! {
    AllDifferent: all_different {
        variables: Vec<Variable>,
    }
}

python_constraint! {
    BinaryEquals: binary_equals {
        lhs: Variable,
        rhs: Variable,
    }
}

python_constraint! {
    BinaryLessThan: binary_less_than {
        lhs: Variable,
        rhs: Variable,
    }
}

python_constraint! {
    BinaryLessThanEqual: binary_less_than_or_equals {
        lhs: Variable,
        rhs: Variable,
    }
}

python_constraint! {
    BinaryNotEquals: binary_not_equals {
        lhs: Variable,
        rhs: Variable,
    }
}

python_constraint! {
    Cumulative: cumulative {
        start_times: Vec<Variable>,
        durations: Vec<i32>,
        resource_requirements: Vec<i32>,
        resource_capacity: i32,
    }
}

python_constraint! {
    Division: division {
        numerator: Variable,
        denominator: Variable,
        rhs: Variable,
    }
}

python_constraint! {
    Element: element {
        index: Variable,
        array: Vec<Variable>,
        rhs: Variable,
    }
}

python_constraint! {
    Equals: equals {
        terms: Vec<Variable>,
        rhs: i32,
    }
}

python_constraint! {
    LessThanOrEquals: less_than_or_equals {
        terms: Vec<Variable>,
        rhs: i32,
    }
}

python_constraint! {
    Maximum: maximum {
        choices: Vec<Variable>,
        rhs: Variable,
    }
}

python_constraint! {
    Minimum: minimum {
        choices: Vec<Variable>,
        rhs: Variable,
    }
}

python_constraint! {
    NotEquals: not_equals {
        terms: Vec<Variable>,
        rhs: i32,
    }
}

python_constraint! {
    Plus: plus {
        a: Variable,
        b: Variable,
        c: Variable,
    }
}

python_constraint! {
    Times: times {
        a: Variable,
        b: Variable,
        c: Variable,
    }
}
