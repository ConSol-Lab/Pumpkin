use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::num::NonZero;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::containers::HashMap;
#[allow(unused, reason = "used when debugging")]
use crate::containers::StorageKey;
use crate::declare_inference_label;
use crate::engine::Assignments;
use crate::engine::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
#[allow(unused, reason = "used when debugging")]
use crate::engine::propagation::PropagatorId;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateConstructor;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::variables::DomainId;

declare_inference_label!(HypercubeLinearInference);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Hypercube {
    domains: HashMap<DomainId, (i32, i32)>,
}

impl Hash for Hypercube {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for predicate in self.iter() {
            predicate.hash(state);
        }
    }
}

impl Hypercube {
    /// Panics if the assignment becomes inconsistent.
    fn apply(&mut self, predicate: Predicate) {
        let (lower_bound, upper_bound) = self
            .domains
            .entry(predicate.get_domain())
            .or_insert((i32::MIN, i32::MAX));

        match predicate.get_predicate_type() {
            crate::predicates::PredicateType::LowerBound => {
                if *lower_bound < predicate.get_right_hand_side() {
                    assert_ne!(
                        predicate.get_right_hand_side(),
                        i32::MIN,
                        "i32::MIN is used to model -infinity"
                    );

                    *lower_bound = predicate.get_right_hand_side();
                }
            }
            crate::predicates::PredicateType::UpperBound => {
                if *upper_bound > predicate.get_right_hand_side() {
                    assert_ne!(
                        predicate.get_right_hand_side(),
                        i32::MAX,
                        "i32::MAX is used to model infinity"
                    );

                    *upper_bound = predicate.get_right_hand_side();
                }
            }
            crate::predicates::PredicateType::NotEqual
            | crate::predicates::PredicateType::Equal => {
                panic!("hypercubes cannot contain != or == predicates")
            }
        }

        assert!(*lower_bound <= *upper_bound);
    }

    fn lower_bound(&self, term: Term) -> Option<i64> {
        let (lower_bound, upper_bound) = self.domains.get(&term.domain)?;

        if term.weight.is_positive() {
            if *lower_bound == i32::MIN {
                None
            } else {
                Some(*lower_bound as i64 * term.weight.get() as i64)
            }
        } else if term.weight.is_negative() {
            if *upper_bound == i32::MAX {
                None
            } else {
                Some(*upper_bound as i64 * term.weight.get() as i64)
            }
        } else {
            unreachable!("term weight is never zero")
        }
    }

    fn iter(&self) -> impl Iterator<Item = Predicate> + '_ {
        self.domains
            .iter()
            .flat_map(|(&domain, &(lower_bound, upper_bound))| {
                let lower_bound_predicate = if lower_bound == i32::MIN {
                    None
                } else {
                    Some(predicate![domain >= lower_bound])
                };
                let upper_bound_predicate = if upper_bound == i32::MAX {
                    None
                } else {
                    Some(predicate![domain <= upper_bound])
                };

                [lower_bound_predicate, upper_bound_predicate]
                    .into_iter()
                    .flatten()
            })
    }

    fn is_empty(&self) -> bool {
        self.iter().next().is_none()
    }

    fn len(&self) -> usize {
        self.iter().count()
    }
}

impl Display for Hypercube {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bounds = self.iter().collect::<Vec<_>>();
        bounds.sort();

        write!(f, "{bounds:?}")
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Term {
    weight: NonZero<i32>,
    domain: DomainId,
}

impl Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Term")
            .field(&self.weight)
            .field(&self.domain)
            .finish()
    }
}

impl Term {
    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i64 {
        let unscaled_bound = if self.weight.is_positive() {
            assignment.get_lower_bound_at_trail_position(self.domain, trail_position)
        } else {
            assignment.get_upper_bound_at_trail_position(self.domain, trail_position)
        };

        i64::from(self.weight.get())
            .checked_mul(i64::from(unscaled_bound))
            .expect("integer overflow")
    }
}

impl PredicateConstructor for Term {
    type Value = i64;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if self.weight.is_negative() {
            let inverted_bound = <i64 as NumExt>::div_floor(bound, self.weight.get() as i64);
            let inverted_bound_i32 = i32::try_from(inverted_bound).expect("integer_overflow");
            predicate![self.domain <= inverted_bound_i32]
        } else {
            let inverted_bound = <i64 as NumExt>::div_ceil(bound, self.weight.get() as i64);
            let inverted_bound_i32 = i32::try_from(inverted_bound).expect("integer_overflow");
            predicate![self.domain >= inverted_bound_i32]
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if self.weight.is_negative() {
            let inverted_bound = <i64 as NumExt>::div_ceil(bound, self.weight.get() as i64);
            let inverted_bound_i32 = i32::try_from(inverted_bound).expect("integer_overflow");
            predicate![self.domain >= inverted_bound_i32]
        } else {
            let inverted_bound = <i64 as NumExt>::div_floor(bound, self.weight.get() as i64);
            let inverted_bound_i32 = i32::try_from(inverted_bound).expect("integer_overflow");
            predicate![self.domain <= inverted_bound_i32]
        }
    }

    fn equality_predicate(&self, _: Self::Value) -> Predicate {
        todo!("cannot create equality predicates for terms")
    }

    fn disequality_predicate(&self, _: Self::Value) -> Predicate {
        todo!("cannot create disequality predicates for terms")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HypercubeLinear {
    hypercube: Hypercube,
    linear_terms: Vec<Term>,
    linear_rhs: i32,
}

impl Display for HypercubeLinear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let HypercubeLinear {
            hypercube,
            linear_terms,
            linear_rhs,
        } = self;

        write!(f, "{hypercube} -> {linear_terms:?} <= {linear_rhs}")
    }
}

impl HypercubeLinear {
    pub fn clause(hypercube: Vec<Predicate>) -> Self {
        HypercubeLinear::new(hypercube, vec![], -1).expect("clause is never trivially satisfiable")
    }

    pub fn new(
        hypercube_predicates: Vec<Predicate>,
        mut linear_terms: Vec<(NonZero<i32>, DomainId)>,
        mut linear_rhs: i32,
    ) -> Option<Self> {
        let mut hypercube = Hypercube::default();
        for predicate in hypercube_predicates.iter().copied() {
            hypercube.apply(predicate);
        }

        let fixed_domain_value_pairs =
            hypercube
                .domains
                .iter()
                .filter_map(|(domain, (lower_bound, upper_bound))| {
                    if lower_bound == upper_bound {
                        Some((*domain, lower_bound))
                    } else {
                        None
                    }
                });

        // Merge domain IDs with multiple weights.
        linear_terms.sort_by_key(|(_, domain)| *domain);
        let mut linear_terms =
            linear_terms
                .into_iter()
                .fold(vec![], |mut acc, (weight, domain)| {
                    // Get the last term in the linear component.
                    let Some(last_term) = acc.last_mut() else {
                        acc.push(Term { domain, weight });
                        return acc;
                    };

                    // If it is the same domain as the current domain, then add the weights
                    // together.
                    if domain == last_term.domain {
                        let new_weight = last_term
                            .weight
                            .get()
                            .checked_add(weight.get())
                            .expect("integer overflow");

                        if let Some(new_weight) = NonZero::new(new_weight) {
                            last_term.weight = new_weight;
                            return acc;
                        } else {
                            let _ = acc.pop();
                            return acc;
                        }
                    }

                    // Otherwise, add the current term to the accumulator.
                    acc.push(Term { domain, weight });
                    acc
                });

        // If the domain is fixed, then modify the right-hand side and exclude the term from the
        // linear component.
        for (domain, value) in fixed_domain_value_pairs {
            if let Some(index) = linear_terms.iter().position(|view| view.domain == domain) {
                let view = linear_terms.remove(index);
                linear_rhs = linear_rhs
                    .checked_sub(
                        value
                            .checked_mul(view.weight.get())
                            .expect("integer overflow"),
                    )
                    .expect("integer overflow");
            }
        }

        if linear_terms.is_empty() && linear_rhs >= 0 {
            None
        } else {
            Some(Self {
                hypercube,
                linear_terms,
                linear_rhs,
            })
        }
    }

    pub(crate) fn compute_slack_at_trail_position(
        &self,
        assignments: &Assignments,
        trail_position: usize,
    ) -> i64 {
        let lhs = self.evaluate_linear_lower_bound(assignments, trail_position);
        self.linear_rhs as i64 - lhs
    }

    fn evaluate_linear_lower_bound(&self, assignments: &Assignments, trail_position: usize) -> i64 {
        self.linear_terms
            .iter()
            .map(|term| {
                let domain_lower_bound =
                    term.lower_bound_at_trail_position(assignments, trail_position);

                if let Some(hypercube_bound) = self.hypercube.lower_bound(*term) {
                    i64::max(hypercube_bound, domain_lower_bound)
                } else {
                    domain_lower_bound
                }
            })
            .reduce(|lb1, lb2| lb1.checked_add(lb2).expect("integer overflow"))
            .unwrap_or(0)
    }

    /// Get an iterator over the hypercube of this constraint.
    pub fn iter_hypercube(&self) -> impl Iterator<Item = Predicate> + '_ {
        self.hypercube.iter()
    }

    /// Get an iterator over the terms in the linear component of the constraint. Each [`DomainId`]
    /// is guaranteed to be present only once.
    pub fn iter_linear_terms(
        &self,
    ) -> impl ExactSizeIterator<Item = (NonZero<i32>, DomainId)> + '_ {
        self.linear_terms.iter().copied().map(term_to_tuple)
    }

    pub fn linear_rhs(&self) -> i32 {
        self.linear_rhs
    }

    /// Get the weight for the given domain in the linear component of the hypercube linear.
    ///
    /// If the domain is not in the linear component, this returns `None`.
    pub(crate) fn variable_weight(&self, domain: DomainId) -> Option<NonZero<i32>> {
        self.linear_terms.iter().find_map(|term| {
            if term.domain == domain {
                Some(term.weight)
            } else {
                None
            }
        })
    }

    pub(crate) fn weaken(&self, predicate: Predicate) -> Option<HypercubeLinear> {
        let domain = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        assert!(self.linear_terms.is_sorted_by_key(|term| term.domain));

        let term_index = self
            .linear_terms
            .binary_search_by(|term| term.domain.cmp(&domain))
            .ok()?;

        let term_to_weaken = self.linear_terms[term_index];

        let rhs_delta = if predicate.is_lower_bound_predicate()
            && term_to_weaken.weight.is_positive()
            || predicate.is_upper_bound_predicate() && term_to_weaken.weight.is_negative()
        {
            term_to_weaken.weight.get().checked_mul(value).unwrap()
        } else {
            return None;
        };

        let mut hypercube = self.hypercube.clone();
        hypercube.apply(predicate);

        // Filtering keeps the sorted order of the linear terms.
        let linear_terms = self
            .linear_terms
            .iter()
            .copied()
            .filter(|&term| term != term_to_weaken)
            .collect();

        Some(HypercubeLinear {
            hypercube,
            linear_terms,
            linear_rhs: self.linear_rhs - rhs_delta,
        })
    }

    pub(crate) fn propagate_at(
        &self,
        assignments: &Assignments,
        trail_position: usize,
        #[allow(unused, reason = "used for debugging")] propagator_id: Option<PropagatorId>,
    ) -> Vec<(Predicate, PropositionalConjunction)> {
        let mut propagations = vec![];

        let num_satisfied_bounds = self
            .hypercube
            .iter()
            .filter(|&predicate| {
                assignments.evaluate_predicate_at_trail_position(predicate, trail_position)
                    == Some(true)
            })
            .count();

        let slack = self.compute_slack_at_trail_position(assignments, trail_position);

        // if let Some(pid) = propagator_id
        //     && pid == PropagatorId::create_from_index(864)
        // {
        //     println!("{}", self);
        //     dbg!(assignments.get_decision_level());
        //     dbg!(
        //         self.hypercube
        //             .iter()
        //             .filter(|&predicate| assignments
        //                 .evaluate_predicate_at_trail_position(predicate, trail_position)
        //                 != Some(true))
        //             .collect::<Vec<_>>()
        //     );
        //     dbg!(num_satisfied_bounds);
        //     dbg!(self.hypercube.len());
        //     dbg!(slack);
        //     for p in self.iter_hypercube() {
        //         let domain = p.get_domain();
        //         println!(
        //             "{} in [{}, {}]",
        //             domain,
        //             assignments.get_lower_bound_at_trail_position(domain, trail_position),
        //             assignments.get_upper_bound_at_trail_position(domain, trail_position)
        //         );
        //     }
        //     for term in self.linear_terms.iter() {
        //         println!(
        //             "{:?} >= {} ({} in [{}, {}]",
        //             term,
        //             term.lower_bound_at_trail_position(assignments, trail_position),
        //             term.domain,
        //             assignments.get_lower_bound_at_trail_position(term.domain, trail_position),
        //             assignments.get_upper_bound_at_trail_position(term.domain, trail_position)
        //         );
        //     }
        // }

        if !self.hypercube.is_empty() && num_satisfied_bounds == self.hypercube.len() - 1 {
            let unassigned_predicate = self
                .hypercube
                .iter()
                .find(|&predicate| {
                    assignments.evaluate_predicate_at_trail_position(predicate, trail_position)
                        != Some(true)
                })
                .expect("at least one bound in the hypercube is not satisfied");

            let neg_unassigned_predicate = !unassigned_predicate;
            // dbg!(neg_unassigned_predicate);

            if slack < 0 {
                let reason: PropositionalConjunction = self
                    .iter_hypercube()
                    .chain(self.linear_terms.iter().map(|term| {
                        predicate![
                            term >= term.lower_bound_at_trail_position(assignments, trail_position)
                        ]
                    }))
                    .collect();

                propagations.push((neg_unassigned_predicate, reason));
            } else if let Some(weight) = self.variable_weight(unassigned_predicate.get_domain()) {
                let positive_weight_and_lower_bound =
                    weight.is_positive() && unassigned_predicate.is_lower_bound_predicate();
                let negative_weight_and_upper_bound =
                    weight.is_negative() && unassigned_predicate.is_upper_bound_predicate();

                if positive_weight_and_lower_bound || negative_weight_and_upper_bound {
                    let term = Term {
                        domain: unassigned_predicate.get_domain(),
                        weight,
                    };

                    // The slack is computed wrt unassigned_predicate being true. It is guaranteed
                    // to be stronger than the current bound for `term`, so we have to update the
                    // slack with the bound for `term` in the domain instead of in the hypercube.

                    let slack_without_term = slack
                        + self
                            .hypercube
                            .lower_bound(term)
                            .expect("unassigned predicate contributes to slack");
                    let to_propagate = predicate![term <= slack_without_term];

                    assert!(
                        (neg_unassigned_predicate).implies(to_propagate),
                        "{neg_unassigned_predicate} should imply {to_propagate}"
                    );

                    let reason: PropositionalConjunction = self
                        .iter_hypercube()
                        .filter(|&p| p != unassigned_predicate)
                        .chain(self.linear_terms.iter().filter_map(|&other_term| {
                            if other_term != term {
                                Some(predicate![
                                    other_term
                                        >= other_term.lower_bound_at_trail_position(
                                            assignments,
                                            trail_position
                                        )
                                ])
                            } else {
                                None
                            }
                        }))
                        .collect();

                    propagations.push((to_propagate, reason));
                }
            }
        } else if num_satisfied_bounds == self.hypercube.len() {
            let lower_bound_left_hand_side = self
                .linear_terms
                .iter()
                .map(|term| term.lower_bound_at_trail_position(assignments, trail_position))
                .sum::<i64>();

            for (i, term) in self.linear_terms.iter().enumerate() {
                let bound = self.linear_rhs as i64
                    - (lower_bound_left_hand_side
                        - term.lower_bound_at_trail_position(assignments, trail_position));

                let reason: PropositionalConjunction = self
                    .hypercube
                    .iter()
                    .chain(self.linear_terms.iter().enumerate().filter_map(|(j, x_j)| {
                        if j != i {
                            Some(predicate![
                                x_j >= x_j
                                    .lower_bound_at_trail_position(assignments, trail_position)
                            ])
                        } else {
                            None
                        }
                    }))
                    .collect();

                propagations.push((predicate![term <= bound], reason));
            }
        }

        propagations
    }
}

fn term_to_tuple(term: Term) -> (NonZero<i32>, DomainId) {
    (term.weight, term.domain)
}

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinearPropagatorArgs {
    pub(crate) hypercube_linear: HypercubeLinear,
    pub(crate) constraint_tag: ConstraintTag,
    pub(crate) is_learned: bool,
}

impl PropagatorConstructor for HypercubeLinearPropagatorArgs {
    type PropagatorImpl = HypercubeLinearPropagator;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let HypercubeLinearPropagatorArgs {
            hypercube_linear,
            constraint_tag,
            is_learned,
        } = self;

        for predicate in hypercube_linear.hypercube.iter() {
            context.register_predicate(predicate);
        }

        for (i, x_i) in hypercube_linear.linear_terms.iter().enumerate() {
            if x_i.weight.is_positive() {
                context.register(
                    x_i.domain,
                    DomainEvents::LOWER_BOUND,
                    LocalId::from(i as u32),
                );
            } else {
                context.register(
                    x_i.domain,
                    DomainEvents::UPPER_BOUND,
                    LocalId::from(i as u32),
                );
            }
        }

        HypercubeLinearPropagator {
            hypercube_linear,
            constraint_tag,
            inference_code: context.create_inference_code(constraint_tag, HypercubeLinearInference),
            is_learned,
        }
    }
}

/// Propagator for the constraint `/\ hypercube -> \sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct HypercubeLinearPropagator {
    pub(crate) hypercube_linear: HypercubeLinear,
    pub(crate) constraint_tag: ConstraintTag,
    pub(crate) inference_code: InferenceCode,
    pub(crate) is_learned: bool,
}

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let propagations = self.hypercube_linear.propagate_at(
            context.assignments,
            context.assignments.num_trail_entries(),
            Some(context.propagator_id),
        );

        for (predicate, reason) in propagations {
            // if context.propagator_id == PropagatorId::create_from_index(864) {
            //     println!("will propagate {predicate}");
            // }

            context.post(predicate, reason, self.inference_code)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::test_solver::TestSolver;

    const ONE: NonZero<i32> = NonZero::new(1).unwrap();

    #[test]
    fn duplicate_bounds_in_hypercube_are_collapsed() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);

        let constraint =
            HypercubeLinear::new(vec![predicate![x <= 4], predicate![x <= 4]], vec![], -1)
                .expect("not trivially satisfiable");

        let hypercube = constraint.iter_hypercube().collect::<Vec<_>>();
        assert_eq!(hypercube, vec![predicate![x <= 4]]);
        assert_eq!(constraint.hypercube.len(), 1);
    }

    #[test]
    fn implied_predicates_are_removed_from_hypercube() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);

        let constraint =
            HypercubeLinear::new(vec![predicate![x <= 4], predicate![x <= 2]], vec![], -1)
                .expect("not trivially satisfiable");

        let hypercube = constraint.iter_hypercube().collect::<Vec<_>>();
        assert_eq!(hypercube, vec![predicate![x <= 2]]);
        assert_eq!(constraint.hypercube.len(), 1);
    }

    #[test]
    fn weakening_of_positive_weight_term() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);
        let y = solver.new_variable(0, 10);

        let constraint = HypercubeLinear::new(vec![], [(ONE, x), (-ONE, y)].into(), 1)
            .expect("not trivially satisfiable");

        let weakened_constraint =
            HypercubeLinear::new(vec![predicate![x >= 9]], [(-ONE, y)].into(), -8)
                .expect("not trivially satisfiable");

        assert_eq!(
            constraint.weaken(predicate![x >= 9]),
            Some(weakened_constraint)
        );
    }

    #[test]
    fn weakening_of_positive_weight_term_not_one() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);
        let y = solver.new_variable(0, 10);

        let constraint =
            HypercubeLinear::new(vec![], [(NonZero::new(2).unwrap(), x), (-ONE, y)].into(), 1)
                .expect("not trivially satisfiable");

        let weakened_constraint =
            HypercubeLinear::new(vec![predicate![x >= -1]], [(-ONE, y)].into(), 3)
                .expect("not trivially satisfiable");

        assert_eq!(
            constraint.weaken(predicate![x >= -1]),
            Some(weakened_constraint)
        );
    }

    #[test]
    fn weakening_of_negative_weight_term() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);
        let y = solver.new_variable(0, 10);

        let constraint = HypercubeLinear::new(vec![], [(ONE, x), (-ONE, y)].into(), 1)
            .expect("not trivially satisfiable");

        let weakened_constraint =
            HypercubeLinear::new(vec![predicate![y <= 9]], [(ONE, x)].into(), 10)
                .expect("not trivially satisfiable");

        assert_eq!(
            constraint.weaken(predicate![y <= 9]),
            Some(weakened_constraint)
        );
    }

    #[test]
    fn weakening_of_negative_weight_term_not_one() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);
        let y = solver.new_variable(0, 10);

        let constraint =
            HypercubeLinear::new(vec![], [(ONE, x), (NonZero::new(-3).unwrap(), y)].into(), 1)
                .expect("not trivially satisfiable");

        let weakened_constraint =
            HypercubeLinear::new(vec![predicate![y <= 2]], [(ONE, x)].into(), 7)
                .expect("not trivially satisfiable");

        assert_eq!(
            constraint.weaken(predicate![y <= 2]),
            Some(weakened_constraint)
        );
    }

    #[test]
    fn terms_fixed_in_premise_are_removed_from_linear() {
        let x = DomainId::new(0);
        let y = DomainId::new(1);
        let constraint = HypercubeLinear::new(
            vec![predicate![x >= 2], predicate![x <= 2]],
            vec![(NonZero::new(2).unwrap(), x), (ONE, y)],
            4,
        )
        .expect("not trivially satisfiable");

        let linear_terms = constraint.iter_linear_terms().collect::<Vec<_>>();
        assert_eq!(linear_terms, vec![(ONE, y)]);
        assert_eq!(constraint.linear_rhs, 0);
    }

    #[test]
    fn terms_fixed_positive_in_premise_are_removed_from_linear() {
        let x = DomainId::new(0);
        let y = DomainId::new(1);
        let constraint = HypercubeLinear::new(
            vec![predicate![x >= -2], predicate![x <= -2]],
            vec![(NonZero::new(2).unwrap(), x), (ONE, y)],
            4,
        )
        .expect("not trivially satisfiable");

        let linear_terms = constraint.iter_linear_terms().collect::<Vec<_>>();
        assert_eq!(linear_terms, vec![(ONE, y)]);
        assert_eq!(constraint.linear_rhs, 8);
    }

    #[test]
    fn test_bounds_are_not_propagated_if_hypercube_not_satisfied() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x >= 2]],
                    [(ONE, x), (ONE, y)].into(),
                    7,
                )
                .unwrap(),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 10);
    }

    #[test]
    fn test_bounds_are_propagated_if_hypercube_satisfied() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x >= 2]],
                    [(ONE, x), (ONE, y)].into(),
                    7,
                )
                .unwrap(),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 2, 5);
        solver.assert_bounds(y, 0, 5);
    }

    #[test]
    fn test_bounds_are_propagated_if_hypercube_satisfied_non_unit_weight() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x >= 2]],
                    [(ONE, x), (NonZero::new(2).unwrap(), y)].into(),
                    7,
                )
                .unwrap(),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 2, 5);
        solver.assert_bounds(y, 0, 2);
    }

    #[test]
    fn test_hypercube_is_propagated_when_slack_negative() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 5);
        let y = solver.new_variable(2, 10);
        let z = solver.new_variable(5, 10);

        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x >= 2]],
                    [(ONE, y), (ONE, z)].into(),
                    5,
                )
                .unwrap(),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 0, 1);
        solver.assert_bounds(y, 2, 10);
        solver.assert_bounds(z, 5, 10);
    }

    #[test]
    fn test_hypercube_is_propagated_when_slack_including_hypercube_is_negative() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 5);
        let y = solver.new_variable(2, 10);
        let z = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x >= 2], predicate![z >= 4]],
                    [(ONE, y), (ONE, z)].into(),
                    5,
                )
                .unwrap(),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 2, 5);
        solver.assert_bounds(y, 2, 10);
        solver.assert_bounds(z, 0, 3);
    }

    #[test]
    fn linear_propagates_when_hypercube_is_true_except_shared_variable_with_linear() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);
        let y = solver.new_variable(1, 5);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x >= 4]],
                    vec![(ONE, x), (ONE, y)],
                    6,
                )
                .expect("not trivially satisfied"),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 0, 5);
        solver.assert_bounds(y, 1, 5);
    }

    #[test]
    fn linear_propagates_upper_bound_when_hypercube_is_true_except_shared_variable_with_linear_negative_weight()
     {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 10);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(HypercubeLinearPropagatorArgs {
                hypercube_linear: HypercubeLinear::new(
                    vec![predicate![x <= 4]],
                    vec![(-ONE, x)],
                    -1,
                )
                .expect("not trivially satisfied"),
                constraint_tag,
                is_learned: false,
            })
            .expect("no empty domains");

        solver.assert_bounds(x, 1, 10);
    }
}
