//! Contains the propagators used by Pumpkin.
//!
//! If you want to implement your own propagator then we recommend following the guide in
//! [`pumpkin_core::propagation`].
mod propagators;
pub use propagators::*;
#[cfg(test)]
use pumpkin_core::state::State;
#[cfg(test)]
use pumpkin_core::variables::DomainId;

/// Utilities that simplify test code using the [`State`].
#[cfg(test)]
pub(crate) trait StateExt {
    /// Assert that the bounds of the given `domain_id` match the provided `lower_bound` and
    /// `upper_bound`.
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

/// Creates a variable for each value, and the domains in which each variable is fixed to its value,
/// for tests of
/// [`ConstraintDescription::check_solution`](pumpkin_core::propagation::ConstraintDescription::check_solution).
#[cfg(test)]
pub(crate) fn fixed_domains(
    values: &[i32],
) -> (
    Vec<DomainId>,
    pumpkin_checking::VariableState<pumpkin_core::predicates::Predicate>,
) {
    let mut state = State::default();
    let variables = values
        .iter()
        .map(|_| state.new_interval_variable(-100, 100, None))
        .collect::<Vec<_>>();
    let domains = pumpkin_checking::VariableState::prepare_for_conflict_check(
        variables
            .iter()
            .zip(values)
            .map(|(&variable, &value)| pumpkin_core::predicate![variable == value]),
        None,
    )
    .expect("the values are consistent");

    (variables, domains)
}
