use crate::basic_types::Literal;
use crate::basic_types::Predicate;
use crate::basic_types::PredicateConstructor;

/// A propagator variable is a handle to a variable for a propagator.
/// For all practical purposes, this is the variable that propagators operate on.
/// Propagator variables are obtained through [`PropagatorConstructorContext::register()`].

/// In the backend, ['PropagatorVariable'] is a wrapper around the internal representation
/// of the variable in the solver, i.e., it keeps track of the [`LocalId`] when modifying the
/// domain. This is done to seamlessly incorporate 'views' in the solver.
#[derive(Hash, Eq, PartialEq, Clone)]
pub struct PropagatorVariable<Var> {
    pub(super) inner: Var,
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
