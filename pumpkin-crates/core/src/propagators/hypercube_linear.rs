use std::fmt::Display;
use std::num::NonZero;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::containers::HashMap;
use crate::declare_inference_label;
use crate::engine::Assignments;
use crate::engine::DomainEvents;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

declare_inference_label!(HypercubeLinearInference);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Hypercube {
    domains: HashMap<DomainId, (i32, i32)>,
    num_predicates: usize,
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
                    self.num_predicates += 1;
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
                    self.num_predicates += 1;
                }
            }
            crate::predicates::PredicateType::NotEqual
            | crate::predicates::PredicateType::Equal => {
                panic!("hypercubes cannot contain != or == predicates")
            }
        }

        assert!(*lower_bound <= *upper_bound);
    }

    fn lower_bound(&self, view: AffineView<DomainId>) -> i32 {
        let Some((lower_bound, upper_bound)) = self.domains.get(&view.inner) else {
            return i32::MIN;
        };

        if view.scale > 0 {
            lower_bound * view.scale
        } else if view.scale < 0 {
            upper_bound * view.scale
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
        self.num_predicates == 0
    }

    fn len(&self) -> usize {
        self.num_predicates
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HypercubeLinear {
    hypercube: Hypercube,
    linear_terms: Vec<AffineView<DomainId>>,
    linear_rhs: i32,
}

impl Display for HypercubeLinear {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let HypercubeLinear {
            hypercube,
            linear_terms,
            linear_rhs,
        } = self;

        write!(f, "{hypercube:?} -> {linear_terms:?} <= {linear_rhs}")
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
                        acc.push(domain.scaled(weight.get()));
                        return acc;
                    };

                    // If it is the same domain as the current domain, then add the weights
                    // together.
                    if domain == last_term.inner {
                        last_term.scale += weight.get();
                        return acc;
                    }

                    // Otherwise, add the current term to the accumulator.
                    acc.push(domain.scaled(weight.get()));
                    acc
                });

        linear_terms.retain(|view| view.scale != 0);

        // If the domain is fixed, then modify the right-hand side and exclude the term from the
        // linear component.
        for (domain, value) in fixed_domain_value_pairs {
            if let Some(index) = linear_terms.iter().position(|view| view.inner == domain) {
                let view = linear_terms.remove(index);
                linear_rhs -= value * view.scale;
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

    pub(crate) fn compute_slack(&self, assignments: &Assignments) -> i32 {
        self.compute_slack_at_trail_position(assignments, assignments.num_trail_entries() - 1)
    }

    pub(crate) fn compute_slack_at_trail_position(
        &self,
        assignments: &Assignments,
        trail_position: usize,
    ) -> i32 {
        let lhs = self.evaluate_linear_lower_bound(assignments, trail_position);
        self.linear_rhs - lhs
    }

    fn evaluate_linear_lower_bound(&self, assignments: &Assignments, trail_position: usize) -> i32 {
        self.linear_terms
            .iter()
            .map(|view| {
                i32::max(
                    view.lower_bound_at_trail_position(assignments, trail_position),
                    self.hypercube.lower_bound(*view),
                )
            })
            .sum::<i32>()
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
        self.linear_terms.iter().copied().map(view_to_tuple)
    }

    pub fn linear_rhs(&self) -> i32 {
        self.linear_rhs
    }

    /// Get the weight for the given domain in the linear component of the hypercube linear.
    ///
    /// If the domain is not in the linear component, this returns `None`.
    pub(crate) fn variable_weight(&self, domain: DomainId) -> Option<NonZero<i32>> {
        self.linear_terms.iter().find_map(|view| {
            if view.inner == domain {
                Some(view.scale.try_into().expect("scales should be non-zero"))
            } else {
                None
            }
        })
    }

    pub(crate) fn weaken(&self, predicate: Predicate) -> Option<HypercubeLinear> {
        let domain = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        assert!(self.linear_terms.is_sorted_by_key(|term| term.inner));

        let term_index = self
            .linear_terms
            .binary_search_by(|term| term.inner.cmp(&domain))
            .ok()?;

        let term_to_weaken = self.linear_terms[term_index];

        let rhs_delta = if predicate.is_lower_bound_predicate()
            && term_to_weaken.scale.is_positive()
            || predicate.is_upper_bound_predicate() && term_to_weaken.scale.is_negative()
        {
            term_to_weaken.scale.checked_mul(value).unwrap()
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
}

fn view_to_tuple(view: AffineView<DomainId>) -> (NonZero<i32>, DomainId) {
    (NonZero::new(view.scale).unwrap(), view.inner)
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
            context.register(*x_i, DomainEvents::LOWER_BOUND, LocalId::from(i as u32));
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
        let num_satisfied_bounds = self
            .hypercube_linear
            .hypercube
            .iter()
            .filter(|&predicate| context.evaluate_predicate(predicate) == Some(true))
            .count();

        // println!("{}", self.hypercube_linear);
        // dbg!(num_satisfied_bounds);
        // for p in self.hypercube_linear.iter_hypercube() {
        //     let domain = p.get_domain();
        //     println!(
        //         "{} in [{}, {}]",
        //         domain,
        //         context.assignments.get_lower_bound(domain),
        //         context.assignments.get_upper_bound(domain)
        //     );
        // }
        // for domain in self.hypercube_linear.linear_terms.iter() {
        //     use crate::engine::variables::IntegerVariable;

        //     println!(
        //         "{:?} in [{}, {}]",
        //         domain,
        //         domain.lower_bound(context.assignments),
        //         domain.upper_bound(context.assignments)
        //     );
        // }

        if !self.hypercube_linear.hypercube.is_empty()
            && num_satisfied_bounds == self.hypercube_linear.hypercube.len() - 1
        {
            let slack = self.hypercube_linear.compute_slack(context.assignments);
            // dbg!(slack);

            if slack < 0 {
                let unassigned_predicate = self
                    .hypercube_linear
                    .hypercube
                    .iter()
                    .find(|&predicate| context.evaluate_predicate(predicate) != Some(true))
                    .expect("at least one predicate does not evaluate to true");

                context.post(!unassigned_predicate, 0_u64, self.inference_code)?;
            }
        } else if num_satisfied_bounds == self.hypercube_linear.hypercube.len() {
            let lower_bound_left_hand_side = self
                .hypercube_linear
                .linear_terms
                .iter()
                .map(|term| context.lower_bound(term))
                .sum::<i32>();

            for (i, term) in self.hypercube_linear.linear_terms.iter().enumerate() {
                let bound = self.hypercube_linear.linear_rhs
                    - (lower_bound_left_hand_side - context.lower_bound(term));

                let reason: PropositionalConjunction = self
                    .hypercube_linear
                    .hypercube
                    .iter()
                    .chain(
                        self.hypercube_linear
                            .linear_terms
                            .iter()
                            .enumerate()
                            .filter_map(|(j, x_j)| {
                                if j != i {
                                    Some(predicate![x_j >= context.lower_bound(x_j)])
                                } else {
                                    None
                                }
                            }),
                    )
                    .collect();

                context.post(predicate![term <= bound], reason, self.inference_code)?;
            }
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

        let propagator = solver
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

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 10);
    }

    #[test]
    fn test_bounds_are_propagated_if_hypercube_satisfied() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(2, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
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

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 2, 5);
        solver.assert_bounds(y, 0, 5);
    }

    #[test]
    fn test_hypercube_is_propagated_when_slack_negative() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(0, 5);
        let y = solver.new_variable(2, 10);
        let z = solver.new_variable(5, 10);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
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

        solver.propagate(propagator).expect("non-empty domain");

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

        let propagator = solver
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

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 2, 5);
        solver.assert_bounds(y, 2, 10);
        solver.assert_bounds(z, 0, 3);
    }
}
