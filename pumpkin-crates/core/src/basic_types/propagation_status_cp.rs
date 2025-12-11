use super::PropositionalConjunction;
use crate::proof::InferenceCode;
use crate::state::Conflict;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub(crate) type PropagationStatusCP = Result<(), Conflict>;

/// A conflict stated by a propagator. A propagator that identifies a conflict that is _not_ an
/// empty domain, describes that conflict with this type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PropagatorConflict {
    /// The conjunction that describes the infeasible partial assignment.
    pub conjunction: PropositionalConjunction,
    /// The inference code that identified the conflict.
    pub inference_code: InferenceCode,
}
