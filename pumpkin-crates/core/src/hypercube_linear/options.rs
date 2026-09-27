/// Determines how the hypercube linear propagator propagates the hypercube of its constraint.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum HypercubeLinearPropagation {
    /// Propagates when at most one predicate of the hypercube is not true.
    #[default]
    Standard,
    /// Also propagates when the predicates of the hypercube that are not true all concern one
    /// variable, by removing the values of that variable for which the constraint is violated.
    Extended,
}
