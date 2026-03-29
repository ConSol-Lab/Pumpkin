//! Contains the propagators used by Pumpkin.
//!
//! If you want to implement your own propagator then we recommend following the guide in
//! [`pumpkin_core::propagation`].
mod propagators;
pub use propagators::*;
#[cfg(test)]
use pumpkin_core::{state::State, variables::DomainId};

#[cfg(test)]
pub(crate) trait StateExt {
    fn assert_bounds(&self, domain_id: DomainId, lower_bound: i32, upper_bound: i32);
}

#[cfg(test)]
impl StateExt for State {
    fn assert_bounds(&self, domain_id: DomainId, lower_bound: i32, upper_bound: i32) {
        let actual_lb = self.lower_bound(domain_id);
        let actual_ub = self.upper_bound(domain_id);

        assert_eq!(
            (lower_bound, upper_bound),
            (actual_lb, actual_ub),
            "The expected bounds [{lower_bound}..{upper_bound}] did not match the actual bounds [{actual_lb}..{actual_ub}]"
        );
    }
}
