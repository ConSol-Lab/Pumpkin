use std::fmt::Debug;

#[cfg(doc)]
use crate::propagation::ConstraintDescription;

/// The settings of a propagator: choices that change how it propagates or explains, but not the
/// constraint it enforces, which its [`ConstraintDescription`] defines. An example is the
/// explanation type of the cumulative.
///
/// Every legal setting has to be sound for the rule of the propagator, so testing tools such as the
/// fuzzer run the propagator under each of them, and identify a setting by its [`Debug`] text.
pub trait PropagatorParameters: Clone + Debug + Sized {
    /// Every setting, including combinations that [`PropagatorParameters::is_legal`] rejects.
    fn all() -> Vec<Self>;

    /// Whether the propagator supports this setting. Implement it only when
    /// [`PropagatorParameters::all`] lists combinations that the propagator does not support.
    fn is_legal(&self) -> bool {
        true
    }

    /// The settings that [`PropagatorParameters::is_legal`] accepts.
    fn all_legal() -> Vec<Self> {
        Self::all()
            .into_iter()
            .filter(|parameters| parameters.is_legal())
            .collect()
    }
}
