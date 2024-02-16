//! Exposes a convenience API for adding constraints to the satisfaction solver. Since a constraint
//! may be decomposed into other constraints, and expressed using zero or multiple propagators,
//! the functions are more expressive when building a model using the programmatic API.
//!
//! The naming of the constraints follows the MiniZinc standard library where possible.

use crate::basic_types::variables::IntVar;
use crate::basic_types::Literal;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintSatisfactionSolver;
use crate::propagators::element::Element;
use crate::propagators::IntTimes;
use crate::propagators::LinearLeq;
use crate::propagators::LinearNe;

/// Provides common constraint implementations.
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
    ) {
        let _ = self.post(Element {
            index,
            array: array.into(),
            rhs,
        });
    }

    /// Adds the constraint `\sum terms_i != rhs`.
    fn int_lin_ne<Var: IntVar + 'static>(&mut self, terms: impl Into<Box<[Var]>>, rhs: i32) {
        let _ = self.post(LinearNe::new(terms.into(), rhs));
    }

    /// Adds the constraint `\sum terms_i <= rhs`.
    fn int_lin_le<Var: IntVar + 'static>(&mut self, terms: impl Into<Box<[Var]>>, rhs: i32) {
        let _ = self.post(LinearLeq::new(terms.into(), rhs));
    }

    /// Adds the constraint `reif -> (\sum terms_i <= rhs)`.
    fn int_lin_le_reif<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) {
        let _ = self.post(LinearLeq::reified(terms.into(), rhs, reif));
    }

    /// Adds the constraint `\sum terms_i = rhs`.
    fn int_lin_eq<Var: IntVar + 'static>(&mut self, terms: impl Into<Box<[Var]>>, rhs: i32) {
        let terms = terms.into();

        self.int_lin_le(terms.clone(), rhs);

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.int_lin_le(negated, -rhs);
    }

    /// Adds the constraint `reif -> (\sum terms_i = rhs)`.
    fn int_lin_eq_reif<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) {
        let terms = terms.into();

        self.int_lin_le_reif(terms.clone(), rhs, reif);

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.int_lin_le_reif(negated, -rhs, reif);
    }

    /// Adds the constraint `lhs != rhs`.
    fn int_ne<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) {
        self.int_lin_ne([lhs.scaled(1), rhs.scaled(-1)], 0);
    }

    /// Adds the constraint `lhs <= rhs`.
    fn int_le<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) {
        self.int_lin_le([lhs.scaled(1), rhs.scaled(-1)], 0);
    }

    /// Adds the constraint `reif -> (lhs <= rhs)`.
    fn int_le_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) {
        self.int_lin_le_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif);
    }

    /// Adds the constraint `lhs < rhs`.
    fn int_lt<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) {
        self.int_le(lhs.scaled(1), rhs.offset(-1));
    }

    /// Adds the constraint `reif -> (lhs < rhs)`.
    fn int_lt_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) {
        self.int_le_reif(lhs.scaled(1), rhs.offset(-1), reif);
    }

    /// Adds the constraint `lhs = rhs`.
    fn int_eq<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) {
        self.int_lin_eq([lhs.scaled(1), rhs.scaled(-1)], 0);
    }

    /// Adds the constraint `reif -> (lhs = rhs)`.
    fn int_eq_reif<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var, reif: Literal) {
        self.int_lin_eq_reif([lhs.scaled(1), rhs.scaled(-1)], 0, reif);
    }

    /// Adds the constraint `a + b = c`.
    fn int_plus<Var: IntVar + 'static>(&mut self, a: Var, b: Var, c: Var) {
        self.int_lin_eq([a.scaled(1), b.scaled(1), c.scaled(-1)], 0);
    }

    /// Adds the constraint `a * b = c`.
    fn int_times(
        &mut self,
        a: impl IntVar + 'static,
        b: impl IntVar + 'static,
        c: impl IntVar + 'static,
    ) {
        let _ = self.post(IntTimes { a, b, c });
    }

    /// Adds the constraint that all variables must be distinct.
    fn all_different<Var: IntVar + 'static>(&mut self, variables: impl Into<Box<[Var]>>) {
        let variables = variables.into();

        for i in 0..variables.len() {
            for j in i + 1..variables.len() {
                self.int_ne(variables[i].clone(), variables[j].clone());
            }
        }
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
