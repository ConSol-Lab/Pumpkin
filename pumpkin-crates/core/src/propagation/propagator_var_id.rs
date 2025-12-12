use crate::propagation::LocalId;
use crate::propagation::PropagatorId;

/// A handle to a variable registered to a propagator.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct PropagatorVarId {
    pub(crate) propagator: PropagatorId,
    pub(crate) variable: LocalId,
}
