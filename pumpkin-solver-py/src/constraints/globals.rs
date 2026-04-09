use pumpkin_solver::core::constraints::NegatableConstraint;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::propagators::nogoods::NogoodPropagator;
use pumpkin_solver::core::state::PropagatorHandle;
use pumpkin_solver::core::variables::Literal;
use pyo3::pyclass;
use pyo3::pymethods;

use crate::model::Tag;
use crate::variables::*;

macro_rules! python_constraint {
    ($name:ident : $constraint_func:path [nogood] { $($field:ident : $type:ty),+ $(,)? }) => {
        python_constraint!(@impl solver, $name, $constraint_func, (solver.nogood_propagator_handle()), $($field : $type),+);
    };

    ($name:ident : $constraint_func:path [equality] { $($field:ident : $type:ty),+ $(,)? }) => {
        python_constraint!(@impl solver, $name, $constraint_func, (pumpkin_constraints::EqualityConsistency::Bound), $($field : $type),+);
    };

    ($name:ident : $constraint_func:path { $($field:ident : $type:ty),+ $(,)? }) => {
        python_constraint!(@impl solver, $name, $constraint_func, (), $($field : $type),+);
    };

    (@impl $solver_name:ident, $name:ident, $constraint_func:path, ($($extra:expr),*), $($field:ident : $type:ty),+) => {
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
                $solver_name: &mut pumpkin_solver::Solver,
            ) -> Result<(), pumpkin_solver::core::ConstraintOperationError> {
                $solver_name
                    .add_constraint($constraint_func(
                        $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field)),+ ,
                        self.constraint_tag.0,
                        $($extra,)*
                    ))
                    .post()
            }

            pub fn implied_by(
                self,
                $solver_name: &mut pumpkin_solver::Solver,
                reification_literal: pumpkin_solver::core::variables::Literal,
            ) -> Result<(), pumpkin_solver::core::ConstraintOperationError> {
                $solver_name
                    .add_constraint($constraint_func(
                        $(<$type as super::arguments::PythonConstraintArg>::to_solver_constraint_argument(self.$field)),+ ,
                        self.constraint_tag.0,
                        $($extra,)*
                    ))
                    .implied_by(reification_literal)
            }
        }
    };
}

python_constraint! {
    Absolute: pumpkin_constraints::absolute {
        signed: IntExpression,
        absolute: IntExpression,
    }
}

python_constraint! {
    AllDifferent: pumpkin_constraints::all_different {
        variables: Vec<IntExpression>,
    }
}

python_constraint! {
    BinaryEquals: pumpkin_constraints::binary_equals [equality] {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    BinaryLessThan: pumpkin_constraints::binary_less_than {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    BinaryLessThanEqual: pumpkin_constraints::binary_less_than_or_equals {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    BinaryNotEquals: pumpkin_constraints::binary_not_equals [equality] {
        lhs: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    Cumulative: pumpkin_constraints::cumulative {
        start_times: Vec<IntExpression>,
        durations: Vec<i32>,
        resource_requirements: Vec<i32>,
        resource_capacity: i32,
    }
}

python_constraint! {
    Division: pumpkin_constraints::division {
        numerator: IntExpression,
        denominator: IntExpression,
        rhs: IntExpression,
    }
}

python_constraint! {
    Element: pumpkin_constraints::element {
        index: IntExpression,
        array: Vec<IntExpression>,
        rhs: IntExpression,
    }
}

python_constraint! {
    Equals: pumpkin_constraints::equals [equality] {
        terms: Vec<IntExpression>,
        rhs: i32,
    }
}

python_constraint! {
    LessThanOrEquals: pumpkin_constraints::less_than_or_equals {
        terms: Vec<IntExpression>,
        rhs: i32,
    }
}

python_constraint! {
    Maximum: pumpkin_constraints::maximum {
        choices: Vec<IntExpression>,
        rhs: IntExpression,
    }
}

python_constraint! {
    Minimum: pumpkin_constraints::minimum {
        choices: Vec<IntExpression>,
        rhs: IntExpression,
    }
}

python_constraint! {
    NotEquals: pumpkin_constraints::not_equals [equality] {
        terms: Vec<IntExpression>,
        rhs: i32,
    }
}

python_constraint! {
    Plus: pumpkin_constraints::plus {
        a: IntExpression,
        b: IntExpression,
        c: IntExpression,
    }
}

python_constraint! {
    Times: pumpkin_constraints::times {
        a: IntExpression,
        b: IntExpression,
        c: IntExpression,
    }
}

fn conjunction(
    literals: impl IntoIterator<Item = Literal>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
) -> impl NegatableConstraint {
    pumpkin_constraints::conjunction(
        literals.into_iter().map(|lit| lit.get_true_predicate()),
        constraint_tag,
        propagator_handle,
    )
}

fn clause(
    literals: impl IntoIterator<Item = Literal>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
) -> impl NegatableConstraint {
    pumpkin_constraints::clause(
        literals.into_iter().map(|lit| lit.get_true_predicate()),
        constraint_tag,
        propagator_handle,
    )
}

python_constraint! {
    Conjunction: conjunction [nogood] {
        literals: Vec<BoolExpression>,
    }
}

python_constraint! {
    Clause: clause [nogood] {
        literals: Vec<BoolExpression>,
    }
}

python_constraint! {
    Table: pumpkin_constraints::table [nogood] {
        variables: Vec<IntExpression>,
        table: Vec<Vec<i32>>,
    }
}

python_constraint! {
    NegativeTable: pumpkin_constraints::negative_table [nogood] {
        variables: Vec<IntExpression>,
        table: Vec<Vec<i32>>,
    }
}
