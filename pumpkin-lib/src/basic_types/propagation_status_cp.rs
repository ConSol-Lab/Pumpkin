use super::PropositionalConjunction;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub type PropagationStatusCP = Result<(), PropositionalConjunction>;
