use std::fmt::Display;
use std::num::NonZero;

use enumset::EnumSet;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;

use crate::containers::HashMap;
use crate::engine::Assignments;
use crate::engine::VariableNames;
use crate::engine::notifications::Watchers;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateConstructor;
use crate::propagation::DomainEvent;
use crate::propagation::OpaqueDomainEvent;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

/// The linear inequality part of a hypercube linear constraint.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LinearInequality {
    terms: Box<[Term]>,
    pub(crate) bound: i32,
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
            .filter_map(|(domain_id, weight)| NonZero::new(weight).map(|w| (domain_id, w)))
            .map(|(domain, weight)| Term { domain, weight })
            .collect::<Box<[_]>>();

        if terms.is_empty() && bound >= 0 {
            return None;
        }

        // We will always sort the terms by the domain ID to keep a consistent ordering.
        // This is useful for debugging, and makes the equality and hash derives more
        // robust.
        terms.sort_by_key(|view| view.domain);

        Some(LinearInequality { terms, bound })
    }

    /// Iterate over the terms in the linear inequality.
    pub fn terms(&self) -> impl ExactSizeIterator<Item = Term> + '_ {
        self.terms.iter().copied()
    }

    /// Iterate over the terms in the linear inequality to mutate them.
    pub fn terms_mut(&mut self) -> impl Iterator<Item = &mut Term> + '_ {
        self.terms.iter_mut()
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
    pub fn term_for_domain(&self, domain: DomainId) -> Option<Term> {
        self.terms
            .binary_search_by(|view| view.domain.cmp(&domain))
            .ok()
            .map(|index| self.terms[index])
    }

    /// Print the linear inequality.
    pub(crate) fn display(&self, names: &VariableNames) -> impl Display {
        LinearDisplay {
            linear: self,
            names,
        }
    }
}

struct LinearDisplay<'l, 'names> {
    linear: &'l LinearInequality,
    names: &'names VariableNames,
}

impl Display for LinearDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let num_terms = self.linear.terms.len();

        for (idx, term) in self.linear.terms().enumerate() {
            write!(f, "{}*{}", term.weight, term.domain.display(self.names))?;

            if idx < num_terms - 1 {
                write!(f, " + ")?;
            }
        }

        if self.linear.terms.is_empty() {
            write!(f, "0")?;
        }

        write!(f, " <= {}", self.linear.bound)?;

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Term {
    pub weight: NonZero<i32>,
    pub domain: DomainId,
}

impl Term {
    fn map(&self, value: i32) -> i32 {
        self.weight.get() * value
    }
}

impl CheckerVariable<Predicate> for Term {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        self.domain.does_atomic_constrain_self(atomic)
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        predicate![self <= value]
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        predicate![self >= value]
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        predicate![self == value]
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        predicate![self != value]
    }

    fn induced_lower_bound(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> IntExt {
        if self.weight.is_positive() {
            match self.domain.induced_lower_bound(variable_state) {
                IntExt::Int(value) => IntExt::Int(self.map(value)),
                bound => bound,
            }
        } else {
            match self.domain.induced_upper_bound(variable_state) {
                IntExt::Int(value) => IntExt::Int(self.map(value)),
                IntExt::NegativeInf => IntExt::PositiveInf,
                IntExt::PositiveInf => IntExt::NegativeInf,
            }
        }
    }

    fn induced_upper_bound(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> IntExt {
        if self.weight.is_positive() {
            match self.domain.induced_upper_bound(variable_state) {
                IntExt::Int(value) => IntExt::Int(self.map(value)),
                bound => bound,
            }
        } else {
            match self.domain.induced_lower_bound(variable_state) {
                IntExt::Int(value) => IntExt::Int(self.map(value)),
                IntExt::NegativeInf => IntExt::PositiveInf,
                IntExt::PositiveInf => IntExt::NegativeInf,
            }
        }
    }

    fn induced_fixed_value(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> Option<i32> {
        self.domain
            .induced_fixed_value(variable_state)
            .map(|value| self.map(value))
    }

    fn induced_domain_contains(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
        value: i32,
    ) -> bool {
        // If the translated value does not divide by weight, then the original value is not in the
        // domain of this affine view.
        if value % self.weight.get() != 0 {
            return false;
        }

        let unweightd_value = value / self.weight.get();

        self.domain
            .induced_domain_contains(variable_state, unweightd_value)
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        variable_state: &'state pumpkin_checking::VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        if self.weight.get() == 1 || self.weight.get() == -1 {
            return self
                .domain
                .induced_holes(variable_state)
                .map(|value| self.map(value));
        }

        todo!("how to iterate holes of a scaled domain");
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state pumpkin_checking::VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        self.domain
            .iter_induced_domain(variable_state)
            .map(|iter| iter.map(|value| self.map(value)))
    }
}

impl PredicateConstructor for Term {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if self.weight.is_negative() {
            let inverted_bound = <i32 as NumExt>::div_floor(bound, self.weight.get());
            predicate![self.domain <= inverted_bound]
        } else {
            let inverted_bound = <i32 as NumExt>::div_ceil(bound, self.weight.get());
            predicate![self.domain >= inverted_bound]
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if self.weight.is_negative() {
            let inverted_bound = <i32 as NumExt>::div_ceil(bound, self.weight.get());
            predicate![self.domain >= inverted_bound]
        } else {
            let inverted_bound = <i32 as NumExt>::div_floor(bound, self.weight.get());
            predicate![self.domain <= inverted_bound]
        }
    }

    fn equality_predicate(&self, _: Self::Value) -> Predicate {
        todo!("cannot create equality predicates for terms")
    }

    fn disequality_predicate(&self, _: Self::Value) -> Predicate {
        todo!("cannot create disequality predicates for terms")
    }
}

impl IntegerVariable for Term {
    type AffineView = AffineView<DomainId>;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        if self.weight.is_negative() {
            self.map(self.domain.upper_bound(assignment))
        } else {
            self.map(self.domain.lower_bound(assignment))
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if self.weight.is_negative() {
            self.map(
                self.domain
                    .upper_bound_at_trail_position(assignment, trail_position),
            )
        } else {
            self.map(
                self.domain
                    .lower_bound_at_trail_position(assignment, trail_position),
            )
        }
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        if self.weight.is_negative() {
            self.map(self.domain.lower_bound(assignment))
        } else {
            self.map(self.domain.upper_bound(assignment))
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if self.weight.is_negative() {
            self.map(
                self.domain
                    .lower_bound_at_trail_position(assignment, trail_position),
            )
        } else {
            self.map(
                self.domain
                    .upper_bound_at_trail_position(assignment, trail_position),
            )
        }
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        if value % self.weight.get() == 0 {
            let inverted = value / self.weight.get();
            self.domain.contains(assignment, inverted)
        } else {
            false
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        if value % self.weight.get() == 0 {
            let inverted = value / self.weight.get();
            self.domain
                .contains_at_trail_position(assignment, inverted, trail_position)
        } else {
            false
        }
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        self.domain
            .iterate_domain(assignment)
            .map(|value| self.map(value))
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, mut events: EnumSet<DomainEvent>) {
        let bound = DomainEvent::LowerBound | DomainEvent::UpperBound;
        let intersection = events.intersection(bound);
        if intersection.len() == 1 && self.weight.is_negative() {
            events = events.symmetrical_difference(bound);
        }
        self.domain.watch_all(watchers, events);
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        self.domain.unwatch_all(watchers);
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, mut events: EnumSet<DomainEvent>) {
        let bound = DomainEvent::LowerBound | DomainEvent::UpperBound;
        let intersection = events.intersection(bound);
        if intersection.len() == 1 && self.weight.is_negative() {
            events = events.symmetrical_difference(bound);
        }
        self.domain.watch_all_backtrack(watchers, events);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        if self.weight.is_negative() {
            match self.domain.unpack_event(event) {
                DomainEvent::LowerBound => DomainEvent::UpperBound,
                DomainEvent::UpperBound => DomainEvent::LowerBound,
                event => event,
            }
        } else {
            self.domain.unpack_event(event)
        }
    }

    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        self.domain
            .get_holes_at_current_checkpoint(assignments)
            .map(|value| self.map(value))
    }

    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32> {
        self.domain
            .get_holes(assignments)
            .map(|value| self.map(value))
    }
}

impl TransformableVariable<AffineView<DomainId>> for Term {
    fn scaled(&self, scale: i32) -> AffineView<DomainId> {
        let new_weight = self.weight.get().checked_add(scale).expect("overflow");
        self.domain.scaled(new_weight)
    }

    fn offset(&self, offset: i32) -> AffineView<DomainId> {
        self.domain.scaled(self.weight.get()).offset(offset)
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
            [
                Term {
                    domain: x,
                    weight: NonZero::new(2).unwrap()
                },
                Term {
                    domain: y,
                    weight: NonZero::new(3).unwrap()
                }
            ]
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
            [Term {
                domain: x,
                weight: NonZero::new(5).unwrap()
            }]
            .into_iter()
            .collect::<HashSet<_>>(),
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

        assert_eq!(Vec::<Term>::new(), linear.terms().collect::<Vec<_>>());
    }
}
