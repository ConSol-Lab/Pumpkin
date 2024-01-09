//! Exposes a convenience API for adding constraints to the satisfaction solver. Since a constraint
//! may be decomposed into other constraints, and expressed using zero or multiple propagators,
//! the functions are more expressive when building a model using the programmatic API.
//!
//! The naming of the constraints follows the MiniZinc standard library where possible.

use crate::{
    basic_types::{variables::IntVar, Literal},
    engine::{CPPropagatorConstructor, ConstraintSatisfactionSolver},
    propagators::{IntTimes, LinearLeq, LinearNe},
};

/// Provides common constraint implementations.
pub trait ConstraintsExt {
    fn post<Constructor>(&mut self, constructor: Constructor) -> bool
    where
        Constructor: CPPropagatorConstructor,
        Constructor::Propagator: 'static;

    /// Adds the constraint `\sum terms_i != rhs`.
    fn int_lin_ne<Var: IntVar + 'static>(&mut self, terms: impl Into<Box<[Var]>>, rhs: i32) {
        self.post(LinearNe {
            terms: terms.into(),
            rhs,
        });
    }

    /// Adds the constraint `\sum terms_i <= rhs`.
    fn int_lin_le<Var: IntVar + 'static>(&mut self, terms: impl Into<Box<[Var]>>, rhs: i32) {
        self.post(LinearLeq::new(terms.into(), rhs));
    }

    /// Adds the constraint `reif -> \sum terms_i <= rhs`.
    fn int_lin_le_reif<Var: IntVar + 'static>(
        &mut self,
        terms: impl Into<Box<[Var]>>,
        rhs: i32,
        reif: Literal,
    ) {
        self.post(LinearLeq::reified(terms.into(), rhs, reif));
    }

    /// Adds the constraint `\sum terms_i = rhs`.
    fn int_lin_eq<Var: IntVar + 'static>(&mut self, terms: impl Into<Box<[Var]>>, rhs: i32) {
        let terms = terms.into();

        self.int_lin_le(terms.clone(), rhs);

        let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
        self.int_lin_le(negated, -rhs);
    }

    /// Adds the constraint `lhs != rhs`.
    fn int_ne<Var: IntVar + 'static>(&mut self, lhs: Var, rhs: Var) {
        self.int_lin_ne([lhs.scaled(1), rhs.scaled(-1)], 0);
    }

    /// Adds the constraint `lhs
    fn int_times(
        &mut self,
        a: impl IntVar + 'static,
        b: impl IntVar + 'static,
        c: impl IntVar + 'static,
    ) {
        self.post(IntTimes { a, b, c });
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
