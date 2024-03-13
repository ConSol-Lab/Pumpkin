//! Provides the [`ValueSelector`] trait which is required
//! for value selectors to implement; the main method in this trait relies on
//! [`ValueSelector::select_value`].
//!
//! Furthermore, it defines several implementations of the [`ValueSelector`] trait such as
//! [`InDomainMin`], [`PhaseSaving`] and [`SolutionGuidedValueSelector`]. Any [`ValueSelector`]
//! should only select values which are in the domain of the provided variable.

mod in_domain_interval;
mod in_domain_max;
mod in_domain_median;
mod in_domain_middle;
mod in_domain_min;
mod in_domain_random;
mod in_domain_split;
mod in_domain_split_random;
mod out_domain_max;
mod out_domain_median;
mod out_domain_min;
mod out_domain_random;
mod phase_saving;
mod reverse_in_domain_split;
mod solution_guided_value_selector;
mod value_selector;

pub use in_domain_interval::*;
pub use in_domain_max::*;
pub use in_domain_median::*;
pub use in_domain_middle::*;
pub use in_domain_min::*;
pub use in_domain_random::*;
pub use in_domain_split::*;
pub use in_domain_split_random::*;
pub use out_domain_max::*;
pub use out_domain_median::*;
pub use out_domain_min::*;
pub use out_domain_random::*;
pub use phase_saving::*;
pub use reverse_in_domain_split::*;
pub use solution_guided_value_selector::*;
pub use value_selector::ValueSelector;
