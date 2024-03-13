//! Exposes a convenience API for adding constraints to the satisfaction solver. Since a constraint
//! may be decomposed into other constraints, and expressed using zero or multiple propagators,
//! the functions are more expressive when building a model using the programmatic API.
//!
//! The naming of the constraints follows the MiniZinc standard library where possible.

use crate::basic_types::variables::IntVar;
use crate::basic_types::Literal;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintSatisfactionSolver;
use crate::propagators::arithmetic::absolute_value::AbsoluteValueArgs;
use crate::propagators::arithmetic::integer_multiplication::IntegerMultiplicationArgs;
use crate::propagators::arithmetic::linear_less_or_equal::LinearLessOrEqualArgs;
use crate::propagators::arithmetic::linear_not_equal::LinearNotEqualArgs;
use crate::propagators::arithmetic::maximum::MaximumArgs;
use crate::propagators::element::ElementArgs;
use crate::propagators::ArgTask;
use crate::propagators::TimeTablePerPoint;

/// Provides common constraint implementations. Methods return false if the problem becomes
/// trivially unsatisfiable after adding the constraint, or when the solver was already in an
/// infeasible state.
pub trait ConstraintsExt {
    fn post<Constructor>(&mut self, constructor: Constructor) -> bool
    where
        Constructor: CPPropagatorConstructor,
        Constructor::Propagator: 'static;

    /// Adds the constraint `array[index] = rhs`.
    fn array_var_int_element<ElementVar: IntVar + 'static>(
        &mut self,
        index: impl IntVar + 'static,
        array: impl Into<Box<[ElementVar]>>,
        rhs: impl IntVar + 'static,
    ) -> bool {
        self.post(ElementArgs {
            index,
            array: array.into(),
            rhs,
        })
    }

    /// Adds the constraint `\sum terms_i != rhs`.
    fn int_lin_ne<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        self.post(LinearNotEqualArgs::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif -> \sum terms_i != rhs`.
    fn int_lin_ne_reif<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        self.post(LinearNotEqualArgs::reified(terms.into(), rhs, reif))
    }

    /// Adds the constraint `\sum terms_i <= rhs`.
    fn int_lin_le<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
    ) -> bool {
        self.post(LinearLessOrEqualArgs::new(terms.into(), rhs))
    }

    /// Adds the constraint `reif -> (\sum terms_i <= rhs)`.
    fn int_lin_le_reif<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) -> bool {
        self.post(LinearLessOrEqualArgs::reified(terms.into(), rhs, reif))
    }

    /// Adds the constraint `\sum terms_i = rhs`.
    fn int_lin_eq<Var: IntVar + 'static>(
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
    fn int_lin_eq_reif<Var: IntVar + 'static>(
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
    fn int_ne<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_lin_ne([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> lhs != rhs`.
    fn int_ne_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) -> bool {
        self.int_lin_ne_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs <= rhs`.
    fn int_le<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_lin_le([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> (lhs <= rhs)`.
    fn int_le_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) -> bool {
        self.int_lin_le_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `lhs < rhs`.
    fn int_lt<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_le(lhs.scaled(1), rhs.offset(-1))
    }

    /// Adds the constraint `reif -> (lhs < rhs)`.
    fn int_lt_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) -> bool {
        self.int_le_reif(lhs.scaled(1), rhs.offset(-1), reif)
    }

    /// Adds the constraint `lhs = rhs`.
    fn int_eq<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) -> bool {
        self.int_lin_eq([lhs.scaled(1), rhs.scaled(-1)], 0)
    }

    /// Adds the constraint `reif -> (lhs = rhs)`.
    fn int_eq_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) -> bool {
        self.int_lin_eq_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif)
    }

    /// Adds the constraint `a + b = c`.
    fn int_plus<Var: IntVar + 'static>(&mut self, a: Var, b: Var, c: Var) -> bool {
        self.int_lin_eq([a.scaled(1), b.scaled(1), c.scaled(-1)], 0)
    }

    /// Adds the constraint `a * b = c`.
    fn int_times(
        &mut self,
        a: impl IntVar + 'static,
        b: impl IntVar + 'static,
        c: impl IntVar + 'static,
    ) -> bool {
        self.post(IntegerMultiplicationArgs { a, b, c })
    }

    /// Adds the constraint `|signed| = absolute`.
    fn int_abs(&mut self, signed: impl IntVar + 'static, absolute: impl IntVar + 'static) -> bool {
        self.post(AbsoluteValueArgs { signed, absolute })
    }

    /// Adds the constraint that all variables must be distinct.
    fn all_different<Var: IntVar + 'static>(&mut self, variables: impl Into<Box<[Var]>>) -> bool {
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

    fn cumulative<Var: IntVar + 'static + std::fmt::Debug>(
        &mut self,
        tasks: &[ArgTask<Var>],
        capacity: i32,
    ) {
        // TODO: how to differentiate between different strategies
        let _ = self.post(TimeTablePerPoint::new(
            tasks.iter().cloned().collect(),
            capacity,
        ));
    }

    /// Posts the constraint `max(array) = m`.
    fn maximum<Var: IntVar + 'static>(
        &mut self,
        array: impl Into<Box<[Var]>>,
        rhs: impl IntVar + 'static,
    ) -> bool {
        self.post(MaximumArgs {
            array: array.into(),
            rhs,
        })
    }

    /// Posts the constraint `min(array) = m`.
    fn minimum<Var: IntVar + 'static>(
        &mut self,
        array: impl IntoIterator<Item = Var>,
        rhs: impl IntVar + 'static,
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
        Constructor: CPPropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        self.add_propagator(constructor)
    }
}
