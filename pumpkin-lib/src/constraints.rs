//! Exposes a convenience API for adding constraints to the satisfaction solver. Since a constraint
//! may be decomposed into other constraints, and expressed using zero or multiple propagators,
//! the functions are more expressive when building a model using the programmatic API.
//!
//! The naming of the constraints follows the MiniZinc standard library where possible.

use crate::engine::propagation::PropagatorConstructor;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::propagators::arithmetic::absolute_value::AbsoluteValueConstructor;
use crate::propagators::arithmetic::integer_multiplication::IntegerMultiplicationConstructor;
use crate::propagators::arithmetic::linear_less_or_equal::LinearLessOrEqualConstructor;
use crate::propagators::arithmetic::linear_not_equal::LinearNotEqualConstructor;
use crate::propagators::arithmetic::maximum::MaximumConstructor;
use crate::propagators::division::DivisionConstructor;
use crate::propagators::element::ElementConstructor;
use crate::propagators::ArgTask;
use crate::propagators::TimeTableOverIntervalIncremental;
use crate::pumpkin_assert_simple;

/// Provides common constraint implementations. Methods return false if the problem becomes
/// trivially unsatisfiable after adding the constraint, or when the solver was already in an
/// infeasible state.
pub trait ConstraintsExt {
    fn post<Constructor>(&mut self, constructor: Constructor) -> bool
    where
        Constructor: PropagatorConstructor,
        Constructor::Propagator: 'static;

    fn lower_bound(&self, var: &impl IntegerVariable) -> i32;
    fn upper_bound(&self, var: &impl IntegerVariable) -> i32;
    fn contains(&self, var: &impl IntegerVariable, value: i32) -> bool;

    /// Adds the constraint `array[index] = rhs`.
    fn array_var_int_element<ElementVar: IntegerVariable + 'static>(
        &mut self,
        index: impl IntegerVariable + 'static,
        array: impl Into<Box<[ElementVar]>>,
        rhs: impl IntegerVariable + 'static,
    ) -> bool {
        self.post(ElementConstructor {
            index,
            array: array.into(),
            rhs,
        })
    }

    /// Adds the constraint `\sum terms_i != rhs`.
    fn int_lin_ne<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        self.post(LinearNotEqualConstructor::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif -> \sum terms_i != rhs`.
    fn int_lin_ne_reif<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        self.post(LinearNotEqualConstructor::reified(terms.into(), rhs, reif))
    }

    /// Adds the constraint `\sum terms_i <= rhs`.
    fn int_lin_le<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        self.post(LinearLessOrEqualConstructor::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif -> (\sum terms_i <= rhs)`.
    fn int_lin_le_reif<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        self.post(LinearLessOrEqualConstructor::reified(
            terms.into(),
            rhs,
            reif,
        ))
    }

    /// Adds the constraint `\sum terms_i = rhs`.
    fn int_lin_eq<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        let terms = terms.into();

        if !self.int_lin_le(terms.clone(), rhs) {
            return false;
        }

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.int_lin_le(negated, -rhs)
    }

    /// Adds the constraint `reif -> (\sum terms_i = rhs)`.
    fn int_lin_eq_reif<Var: IntegerVariable + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        let terms = terms.into();

        if !self.int_lin_le_reif(terms.clone(), rhs, reif) {
            return false;
        }

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.int_lin_le_reif(negated, -rhs, reif)
    }

    /// Adds the constraint `lhs != rhs`.
    fn int_ne<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_lin_ne([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> lhs != rhs`.
    fn int_ne_reif<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.int_lin_ne_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs <= rhs`.
    fn int_le<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_lin_le([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> (lhs <= rhs)`.
    fn int_le_reif<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.int_lin_le_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs < rhs`.
    fn int_lt<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_le(lhs.scaled(1), rhs.offset(-1))
    }

    /// Adds the constraint `reif -> (lhs < rhs)`.
    fn int_lt_reif<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.int_le_reif(lhs.scaled(1), rhs.offset(-1), reif)
    }

    /// Adds the constraint `lhs = rhs`.
    fn int_eq<Var: IntegerVariable + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_lin_eq([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> (lhs = rhs)`.
    fn int_eq_reif<Var: IntegerVariable + 'static>(
        &mut self,
        lhs: Var,
        rhs: Var,
        reif: Literal,
    ) -> bool {
        self.int_lin_eq_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `a + b = c`.
    fn int_plus<Var: IntegerVariable + 'static>(&mut self, a: Var, b: Var, c: Var) -> bool {
        self.int_lin_eq([a.scaled(1), b.scaled(1), c.scaled(-1)], 0)
    }

    /// Adds the constraint `a * b = c`.
    fn int_times(
        &mut self,
        a: impl IntegerVariable + 'static,
        b: impl IntegerVariable + 'static,
        c: impl IntegerVariable + 'static,
    ) -> bool {
        self.post(IntegerMultiplicationConstructor { a, b, c })
    }

    /// A propagator for maintaining the constraint `numerator / denominator = rhs`; note that this
    /// propagator performs truncating division (i.e. rounding towards 0).
    ///
    /// The propagator assumes that the `denominator` is a non-zero integer.
    ///
    /// The implementation is ported from [OR-tools](https://github.com/google/or-tools/blob/870edf6f7bff6b8ff0d267d936be7e331c5b8c2d/ortools/sat/integer_expr.cc#L1209C1-L1209C19).
    fn int_div(
        &mut self,
        numerator: impl IntegerVariable + 'static,
        denominator: impl IntegerVariable + 'static,
        rhs: impl IntegerVariable + 'static,
    ) -> bool
    where
        Self: Sized,
    {
        pumpkin_assert_simple!(
            !self.contains(&denominator, 0),
            "We do not support a value of 0 in the domain of the denominator of `int_div`"
        );
        self.post(DivisionConstructor {
            numerator,
            denominator,
            rhs,
        })
    }

    /// Adds the constraint `|signed| = absolute`.
    fn int_abs(
        &mut self,
        signed: impl IntegerVariable + 'static,
        absolute: impl IntegerVariable + 'static,
    ) -> bool {
        self.post(AbsoluteValueConstructor { signed, absolute })
    }

    /// Adds the constraint that all variables must be distinct.
    fn all_different<Var: IntegerVariable + 'static>(
        &mut self,
        variables: impl Into<Box<[Var]>>,
    ) -> bool {
        let variables = variables.into();

        for i in 0..variables.len() {
            for j in i + 1..variables.len() {
                if !self.int_ne(variables[i].clone(), variables[j].clone()) {
                    return false;
                }
            }
        }

        true
    }

    /// Posts the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint.
    /// This constraint ensures that at no point in time, the cumulative resource usage of the tasks
    /// exceeds `bound`. See [`crate::propagators`] for more information.
    ///
    /// The length of `start_times`, `durations` and `resource_requirements` should be the same; if
    /// this is not the case then this method will panic.
    ///
    /// For now we assume that the durations, resource requirements and bound are constant.
    fn cumulative<Var: IntegerVariable + 'static + std::fmt::Debug + Copy>(
        &mut self,
        start_times: &[Var],
        durations: &[i32],
        resource_requirements: &[i32],
        resource_capacity: i32,
        allow_holes_in_domain: bool,
    ) -> bool {
        pumpkin_assert_simple!(
            start_times.len() == durations.len() && durations.len() == resource_requirements.len(),
            "The number of start variables, durations and resource requirements should be the same!car"
        );
        self.post(TimeTableOverIntervalIncremental::new(
            start_times
                .iter()
                .zip(durations)
                .zip(resource_requirements)
                .map(|((start_time, duration), resource_requirement)| ArgTask {
                    start_time: *start_time,
                    processing_time: *duration,
                    resource_usage: *resource_requirement,
                })
                .collect(),
            resource_capacity,
            allow_holes_in_domain,
        ))
    }

    /// Posts the constraint `max(array) = m`.
    fn maximum<Var: IntegerVariable + 'static>(
        &mut self,
        array: impl Into<Box<[Var]>>,
        rhs: impl IntegerVariable + 'static,
    ) -> bool {
        self.post(MaximumConstructor {
            array: array.into(),
            rhs,
        })
    }

    /// Posts the constraint `min(array) = m`.
    fn minimum<Var: IntegerVariable + 'static>(
        &mut self,
        array: impl IntoIterator<Item = Var>,
        rhs: impl IntegerVariable + 'static,
    ) -> bool {
        let array = array
            .into_iter()
            .map(|var| var.scaled(-1))
            .collect::<Box<_>>();
        self.maximum(array, rhs.scaled(-1))
    }
}

impl ConstraintsExt for ConstraintSatisfactionSolver {
    fn post<Constructor>(&mut self, constructor: Constructor) -> bool
    where
        Constructor: PropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        self.add_propagator(constructor)
    }

    fn lower_bound(&self, var: &impl IntegerVariable) -> i32 {
        self.get_lower_bound(var)
    }

    fn upper_bound(&self, var: &impl IntegerVariable) -> i32 {
        self.get_upper_bound(var)
    }

    fn contains(&self, var: &impl IntegerVariable, value: i32) -> bool {
        self.integer_variable_contains(var, value)
    }
}
