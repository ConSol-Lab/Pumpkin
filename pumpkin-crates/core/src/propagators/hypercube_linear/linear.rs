use std::num::NonZero;

use crate::containers::HashMap;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::TransformableVariable;

/// The linear inequality part of a hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct LinearInequality {
    terms: Box<[AffineView<DomainId>]>,
    bound: i32,
}

impl LinearInequality {
    /// Create a linear inequality that is trivially false.
    pub fn trivially_false() -> LinearInequality {
        LinearInequality {
            terms: [].into(),
            bound: -1,
        }
    }

    /// Construct a new linear inequality.
    ///
    /// If the terms simplify to 0 and the `bound` is at least 0, then `None` is returned.
    pub fn new(
        terms: impl IntoIterator<Item = (NonZero<i32>, DomainId)>,
        bound: i32,
    ) -> Option<Self> {
        // To merge terms with the same domain, we go through a HashMap mapping the weight to the
        // domain.
        let mut domain_to_weight = HashMap::new();

        for (weight, domain_id) in terms {
            let existing_weight = domain_to_weight.entry(domain_id).or_insert(0);
            *existing_weight += weight.get();
        }

        let terms = domain_to_weight
            .into_iter()
            .filter(|&(_, weight)| weight != 0)
            .map(|(domain, weight)| domain.scaled(weight))
            .collect::<Box<[_]>>();

        if terms.is_empty() && bound >= 0 {
            return None;
        }

        Some(LinearInequality { terms, bound })
    }

    /// Iterate over the terms in the linear inequality.
    pub fn terms(&self) -> impl Iterator<Item = AffineView<DomainId>> + '_ {
        self.terms.iter().copied()
    }

    /// The bound of the linear inequality.
    pub fn bound(&self) -> i32 {
        self.bound
    }

    /// Get the term for the given domain.
    pub fn term_for_domain(&self, domain: DomainId) -> Option<AffineView<DomainId>> {
        self.terms().find(|view| view.inner == domain)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::HashSet;
    use crate::state::State;

    #[test]
    fn terms_are_iterable() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let linear = LinearInequality::new(
            [(NonZero::new(2).unwrap(), x), (NonZero::new(3).unwrap(), y)],
            8,
        )
        .expect("not trivially true");

        let iterated_terms = linear.terms().collect::<HashSet<_>>();

        assert_eq!(
            [x.scaled(2), y.scaled(3)]
                .into_iter()
                .collect::<HashSet<_>>(),
            iterated_terms
        );
    }

    #[test]
    fn terms_for_same_variable_are_merged() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let linear = LinearInequality::new(
            [(NonZero::new(2).unwrap(), x), (NonZero::new(3).unwrap(), x)],
            8,
        )
        .expect("not trivially true");

        let iterated_terms = linear.terms().collect::<HashSet<_>>();

        assert_eq!(
            [x.scaled(5)].into_iter().collect::<HashSet<_>>(),
            iterated_terms
        );
    }

    #[test]
    fn trivially_satisfied_linear_inequalities_are_not_created() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let linear = LinearInequality::new(
            [
                (NonZero::new(2).unwrap(), x),
                (NonZero::new(-2).unwrap(), x),
            ],
            8,
        );

        assert!(linear.is_none());
    }

    #[test]
    fn trivially_unsatisfied_linear_are_okay() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let linear = LinearInequality::new(
            [
                (NonZero::new(2).unwrap(), x),
                (NonZero::new(-2).unwrap(), x),
            ],
            -1,
        )
        .expect("not trivially satisfiable");

        assert_eq!(
            Vec::<AffineView<DomainId>>::new(),
            linear.terms().collect::<Vec<_>>()
        );
    }
}
