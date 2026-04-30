use std::fmt::Display;
use std::num::NonZero;

use itertools::Itertools;

use crate::containers::HashMap;
use crate::hypercube_linear::BoundComparator;
use crate::hypercube_linear::BoundPredicate;
use crate::math::num_ext::NumExt;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::TransformableVariable;

/// The linear inequality part of a hypercube linear constraint.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LinearInequality {
    terms: Vec<AffineView<DomainId>>,
    bound: i32,
}

impl Default for LinearInequality {
    fn default() -> Self {
        LinearInequality::trivially_false()
    }
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

        let mut terms = domain_to_weight
            .into_iter()
            .filter(|&(_, weight)| weight != 0)
            .map(|(domain, weight)| domain.scaled(weight))
            .collect::<Vec<_>>();

        if terms.is_empty() && bound >= 0 {
            return None;
        }

        terms.sort_by_key(|t| t.inner);

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

    /// Tests whether the left-hand side simplifies to 0 and the right-hand side is less than 0.
    pub fn is_trivially_false(&self) -> bool {
        self.terms.is_empty() && self.bound < 0
    }

    /// Get the term for the given domain.
    pub fn term_for_domain(&self, domain: DomainId) -> Option<AffineView<DomainId>> {
        self.terms().find(|view| view.inner == domain)
    }

    /// Divide the linear inequality and round the bound down.
    ///
    /// The divisor _must_ divide all term weights, otherwise this function panics.
    pub fn divide(&mut self, divisor: i32) {
        for term in self.terms.iter_mut() {
            assert_eq!(term.scale % divisor, 0);
            term.scale /= divisor;
        }

        self.bound = <i32 as NumExt>::div_floor(self.bound, divisor);
    }

    /// Weakens the linear inequality on the given bound.
    ///
    /// Does nothing if the bound does not contribute to the slack of the linear.
    pub fn weaken(mut self, bound: BoundPredicate, count: i32) -> Option<Self> {
        let Some(term_idx) = self
            .terms
            .iter()
            .position(|term| term.inner == bound.domain)
        else {
            return Some(self);
        };

        let term = &mut self.terms[term_idx];
        let contributes_to_slack = (term.scale.is_positive()
            && bound.comparator == BoundComparator::LowerBound)
            || (term.scale.is_negative() && bound.comparator == BoundComparator::UpperBound);

        if !contributes_to_slack {
            return Some(self);
        }

        let signed_diff = match bound.comparator {
            BoundComparator::LowerBound => -count,
            BoundComparator::UpperBound => count,
        };

        term.scale += signed_diff;
        self.bound += signed_diff * bound.value;

        if term.scale == 0 {
            let _ = self.terms.remove(term_idx);
        }

        if self.terms.is_empty() && self.bound >= 0 {
            None
        } else {
            Some(self)
        }
    }

    /// Weakens the linear inequality on the given bound and ensures the weight of the domain
    /// of the bound is 0.
    ///
    /// Does nothing if the bound does not contribute to the slack of the linear.
    pub fn weaken_to_zero(self, bound: BoundPredicate) -> Option<Self> {
        let Some(term) = self.term_for_domain(bound.domain) else {
            return Some(self);
        };

        self.weaken(bound, term.scale.abs())
    }
}

impl Display for LinearInequality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} <= {}",
            self.terms().format_with(" ", |elt, f| f(&format_args!(
                "{} {}",
                elt.scale, elt.inner
            ))),
            self.bound(),
        )
    }
}

/// A convenient helper to construct linear inequalities.
///
/// ## Examples
/// In the following, variable bindings are [`DomainId`]s.
/// ```ignore
/// linear_inequality(2 x + 3 y <= 4);
/// linear_inequality(-1 x + 5 y <= -3);
/// ```
#[macro_export]
macro_rules! linear_inequality {
    ($($weight:literal $var:ident $(+)?)* <= $bound:expr) => {{
        let terms = [$((
            std::num::NonZero::new($weight).unwrap(),
            $var,
        ),)*];
        LinearInequality::new(terms, $bound).unwrap()
    }};
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

    #[test]
    fn macro_handles_linears() {
        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let actual = linear_inequality!(2 x + -3 y <= 5);
        let expected = LinearInequality::new(
            [
                (NonZero::new(2).unwrap(), x),
                (NonZero::new(-3).unwrap(), y),
            ],
            5,
        )
        .expect("not trivially satisfiable");

        assert_eq!(actual, expected);
    }
}
