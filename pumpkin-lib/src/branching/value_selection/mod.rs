//! Provides the [`ValueSelector`] trait which is required
//! for value selectors to implement; the main method in this trait relies on
//! [`ValueSelector::select_value`].
//!
//! Furthermore, it defines several implementations of the [`ValueSelector`] trait such as
//! [`InDomainMin`], [`PhaseSaving`] and [`SolutionGuidedValueSelector`]. Any [`ValueSelector`]
//! should only select values which are in the domain of the provided variable.

mod in_domain_min;
mod phase_saving;
mod solution_guided_value_selector;
mod value_selector;

pub use in_domain_min::InDomainMin;
pub use phase_saving::PhaseSaving;
pub use solution_guided_value_selector::SolutionGuidedValueSelector;
pub use value_selector::ValueSelector;
