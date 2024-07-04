#[cfg(doc)]
use super::LocalId;
#[cfg(doc)]
use super::PropagatorConstructorContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::variables::TransformableVariable;

/// A propagator variable is a handle to a variable for a propagator.
/// For all practical purposes, this is the variable that propagators operate on.
/// Propagator variables are obtained through [`PropagatorConstructorContext::register()`].

/// In the backend, [`PropagatorVariable`] is a wrapper around the internal representation
/// of the variable in the solver, i.e., it keeps track of the [`LocalId`] when modifying the
/// domain. This is done to seamlessly incorporate views \[1\] in the solver.
///
/// # Bibliography
/// \[1\] C. Schulte and G. Tack, ‘Views and iterators for generic constraint implementations’, in
/// International Workshop on Constraint Solving and Constraint Logic Programming, 2005, pp.
/// 118–132.
#[derive(Hash, Eq, PartialEq, Clone)]
pub struct PropagatorVariable<Var> {
    pub(super) inner: Var,
}

impl<Var: IntegerVariable> TransformableVariable<PropagatorVariable<Var::AffineView>>
    for PropagatorVariable<Var>
{
    fn scaled(&self, scale: i32) -> PropagatorVariable<Var::AffineView> {
        PropagatorVariable {
            inner: self.inner.scaled(scale),
        }
    }

    fn offset(&self, offset: i32) -> PropagatorVariable<Var::AffineView> {
        PropagatorVariable {
            inner: self.inner.offset(offset),
        }
    }
}

impl<Var> PropagatorVariable<Var> {
    #[cfg(test)]
    pub fn new(variable: Var) -> Self {
        Self { inner: variable }
    }
}

impl PropagatorVariable<Literal> {
    pub fn get_literal(&self) -> Literal {
        self.inner
    }
}

impl<Var: std::fmt::Debug> std::fmt::Debug for PropagatorVariable<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var({:?})", self.inner)
    }
}

impl<Var: PredicateConstructor> PredicateConstructor for PropagatorVariable<Var> {
    type Value = Var::Value;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.lower_bound_predicate(bound)
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.upper_bound_predicate(bound)
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.equality_predicate(bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        self.inner.disequality_predicate(bound)
    }
}
