//! Contains the propagator for the [Element](https://sofdem.github.io/gccat/gccat/Celement.html)
//! constraint.
#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]

use std::cell::RefCell;

use bitfield_struct::bitfield;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::Domain;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::Union;
use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Reason;

#[derive(Clone, Debug)]
pub struct ElementArgs<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
    pub constraint_tag: ConstraintTag,
}

declare_inference_label!(Element);

impl<VX, VI, VE> PropagatorConstructor for ElementArgs<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    type PropagatorImpl = ElementPropagator<VX, VI, VE>;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, Element),
            Box::new(ElementChecker::new(
                self.array.clone(),
                self.index.clone(),
                self.rhs.clone(),
            )),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let ElementArgs {
            array,
            index,
            rhs,
            constraint_tag,
        } = self;

        for (i, x_i) in array.iter().enumerate() {
            context.register(
                x_i.clone(),
                DomainEvents::ANY_INT,
                LocalId::from(i as u32 + ID_X_OFFSET),
            );
        }

        context.register(index.clone(), DomainEvents::ANY_INT, ID_INDEX);
        context.register(rhs.clone(), DomainEvents::ANY_INT, ID_RHS);

        let inference_code = InferenceCode::new(constraint_tag, Element);

        ElementPropagator {
            array,
            index,
            rhs,
            inference_code,
            rhs_reason_buffer: vec![],
        }
    }
}

const ID_INDEX: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);

// local ids of array vars are shifted by ID_X_OFFSET
const ID_X_OFFSET: u32 = 2;

/// Arc-consistent propagator for constraint `element([x_1, \ldots, x_n], i, e)`, where `x_j` are
///  variables, `i` is an integer variable, and `e` is a variable, which holds iff `x_i = e`
///
/// Note that this propagator is 0-indexed
#[derive(Clone, Debug)]
pub struct ElementPropagator<VX, VI, VE> {
    array: Box<[VX]>,
    index: VI,
    rhs: VE,
    inference_code: InferenceCode,

    rhs_reason_buffer: Vec<Predicate>,
}

impl<VX, VI, VE> Propagator for ElementPropagator<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    fn priority(&self) -> Priority {
        Priority::Low
    }

    fn name(&self) -> &str {
        "Element"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        self.propagate_index_bounds_within_array(&mut context)?;

        self.propagate_rhs_bounds_based_on_array(&mut context)?;

        self.propagate_index_based_on_domain_intersection_with_rhs(&mut context)?;

        if context.is_fixed(&self.index) {
            let idx = context.lower_bound(&self.index);
            self.propagate_equality(&mut context, idx)?;
        }

        Ok(())
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> &[Predicate] {
        let payload = RightHandSideReason::from_bits(code);

        self.rhs_reason_buffer.clear();
        self.rhs_reason_buffer
            .extend(self.array.iter().enumerate().map(|(idx, variable)| {
                if context.contains_at_trail_position(
                    &self.index,
                    idx as i32,
                    context.get_trail_position(),
                ) {
                    match payload.bound() {
                        Bound::Lower => predicate![variable >= payload.value()],
                        Bound::Upper => predicate![variable <= payload.value()],
                    }
                } else {
                    predicate![self.index != idx as i32]
                }
            }));

        &self.rhs_reason_buffer
    }
}

impl<VX, VI, VE> ElementPropagator<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    /// Propagate the bounds of `self.index` to be in the range `[0, self.array.len())`.
    fn propagate_index_bounds_within_array(
        &self,
        context: &mut PropagationContext<'_>,
    ) -> PropagationStatusCP {
        context.post(
            predicate![self.index >= 0],
            conjunction!(),
            &self.inference_code,
        )?;
        context.post(
            predicate![self.index <= self.array.len() as i32 - 1],
            conjunction!(),
            &self.inference_code,
        )?;
        Ok(())
    }

    /// The lower bound (resp. upper bound) of the right-hand side can be the minimum lower
    /// bound (res. maximum upper bound) of the elements.
    fn propagate_rhs_bounds_based_on_array(
        &self,
        context: &mut PropagationContext<'_>,
    ) -> PropagationStatusCP {
        let (rhs_lb, rhs_ub) = self
            .array
            .iter()
            .enumerate()
            .filter(|(idx, _)| context.contains(&self.index, *idx as i32))
            .fold((i32::MAX, i32::MIN), |(rhs_lb, rhs_ub), (_, element)| {
                (
                    i32::min(rhs_lb, context.lower_bound(element)),
                    i32::max(rhs_ub, context.upper_bound(element)),
                )
            });

        context.post(
            predicate![self.rhs >= rhs_lb],
            Reason::DynamicLazy(
                RightHandSideReason::new()
                    .with_bound(Bound::Lower)
                    .with_value(rhs_lb)
                    .into_bits(),
            ),
            &self.inference_code,
        )?;
        context.post(
            predicate![self.rhs <= rhs_ub],
            Reason::DynamicLazy(
                RightHandSideReason::new()
                    .with_bound(Bound::Upper)
                    .with_value(rhs_ub)
                    .into_bits(),
            ),
            &self.inference_code,
        )?;

        Ok(())
    }

    /// Go through the array. For every element for which the domain does not intersect with the
    /// right-hand side, remove it from index.
    fn propagate_index_based_on_domain_intersection_with_rhs(
        &self,
        context: &mut PropagationContext<'_>,
    ) -> PropagationStatusCP {
        let rhs_lb = context.lower_bound(&self.rhs);
        let rhs_ub = context.upper_bound(&self.rhs);
        let mut to_remove = vec![];
        for idx in context.iterate_domain(&self.index) {
            let element = &self.array[idx as usize];

            let element_ub = context.upper_bound(element);
            let element_lb = context.lower_bound(element);

            let reason = if rhs_lb > element_ub {
                conjunction!([element <= rhs_lb - 1] & [self.rhs >= rhs_lb])
            } else if rhs_ub < element_lb {
                conjunction!([element >= rhs_ub + 1] & [self.rhs <= rhs_ub])
            } else {
                continue;
            };

            to_remove.push((idx, reason));
        }

        for (idx, reason) in to_remove.drain(..) {
            context.post(predicate![self.index != idx], reason, &self.inference_code)?;
        }

        Ok(())
    }

    /// Propagate equality between lhs and rhs. This assumes the bounds of rhs have already been
    /// tightened to the bounds of lhs, through a previous propagation rule.
    fn propagate_equality(
        &self,
        context: &mut PropagationContext<'_>,
        index: i32,
    ) -> PropagationStatusCP {
        let rhs_lb = context.lower_bound(&self.rhs);
        let rhs_ub = context.upper_bound(&self.rhs);
        let lhs = &self.array[index as usize];

        context.post(
            predicate![lhs >= rhs_lb],
            conjunction!([self.rhs >= rhs_lb] & [self.index == index]),
            &self.inference_code,
        )?;
        context.post(
            predicate![lhs <= rhs_ub],
            conjunction!([self.rhs <= rhs_ub] & [self.index == index]),
            &self.inference_code,
        )?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
enum Bound {
    Lower = 0,
    Upper = 1,
}

impl Bound {
    const fn into_bits(self) -> u8 {
        self as _
    }

    const fn from_bits(value: u8) -> Self {
        match value {
            0 => Bound::Lower,
            _ => Bound::Upper,
        }
    }
}

#[bitfield(u64)]
struct RightHandSideReason {
    #[bits(32, from = Bound::from_bits)]
    bound: Bound,
    value: i32,
}

#[derive(Clone, Debug)]
pub struct ElementChecker<VX, VI, VE> {
    array: Box<[VX]>,
    index: VI,
    rhs: VE,

    union: RefCell<Union>,
}

impl<VX, VI, VE> ElementChecker<VX, VI, VE> {
    /// Create a new [`ElementChecker`].
    pub fn new(array: Box<[VX]>, index: VI, rhs: VE) -> Self {
        ElementChecker {
            array,
            index,
            rhs,
            union: RefCell::new(Union::empty()),
        }
    }
}

impl<VX, VI, VE, Atomic> InferenceChecker<Atomic> for ElementChecker<VX, VI, VE>
where
    Atomic: AtomicConstraint,
    VX: CheckerVariable<Atomic>,
    VI: CheckerVariable<Atomic>,
    VE: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        self.union.borrow_mut().reset();

        // A domain consistent checker for element does the following:
        // 1. Determine the elements in the array whose index is in the domain of the index
        //    variable.
        // 2. Take the union of the domains of those elements.
        // 3. Intersect that union with the domain on the right-hand side.
        //
        // The intersection should be empty for a conflict to exist.
        let supported_elements: Vec<_> = self
            .array
            .iter()
            .enumerate()
            .filter(|(idx, _)| self.index.induced_domain_contains(&state, *idx as i32))
            .map(|(_, element)| element)
            .collect();

        for element in supported_elements {
            self.union.borrow_mut().add(&state, element);
        }

        assert!(
            self.union.borrow().is_consistent(),
            "at least one element has a non-empty domain or else variable state would be inconsistent"
        );

        // Compute `|union cap rhs| == 0`.
        let intersection_lower_bound = self
            .union
            .borrow()
            .lower_bound()
            .max(self.rhs.induced_lower_bound(&state));
        let intersection_upper_bound = self
            .union
            .borrow()
            .upper_bound()
            .min(self.rhs.induced_upper_bound(&state));
        let holes = self
            .union
            .borrow()
            .holes()
            .chain(self.rhs.induced_holes(&state))
            .collect();

        let intersected_domain =
            Domain::new(intersection_lower_bound, intersection_upper_bound, holes);

        !intersected_domain.is_consistent()
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_checking::TestAtomic;
    use pumpkin_checking::VariableState;
    use pumpkin_core::TestSolver;

    use super::*;

    #[test]
    fn elements_from_array_with_disjoint_domains_to_rhs_are_filtered_from_index() {
        let mut solver = TestSolver::default();

        let x_0 = solver.new_variable(4, 6);
        let x_1 = solver.new_variable(2, 3);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(14, 15);

        let index = solver.new_variable(0, 3);
        let rhs = solver.new_variable(6, 9);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(ElementArgs {
                array: vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(index, 0, 2);

        assert_eq!(
            solver.get_reason_int(predicate![index != 3]),
            conjunction!([x_3 >= 10] & [rhs <= 9])
        );

        assert_eq!(
            solver.get_reason_int(predicate![index != 1]),
            conjunction!([x_1 <= 5] & [rhs >= 6])
        );
    }

    #[test]
    fn bounds_of_rhs_are_min_and_max_of_lower_and_upper_in_array() {
        let mut solver = TestSolver::default();

        let x_0 = solver.new_variable(3, 10);
        let x_1 = solver.new_variable(2, 3);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(14, 15);

        let index = solver.new_variable(0, 3);
        let rhs = solver.new_variable(0, 20);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(ElementArgs {
                array: vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(rhs, 2, 15);

        assert_eq!(
            solver.get_reason_int(predicate![rhs >= 2]),
            conjunction!([x_0 >= 2] & [x_1 >= 2] & [x_2 >= 2] & [x_3 >= 2])
        );

        assert_eq!(
            solver.get_reason_int(predicate![rhs <= 15]),
            conjunction!([x_0 <= 15] & [x_1 <= 15] & [x_2 <= 15] & [x_3 <= 15])
        );
    }

    #[test]
    fn fixed_index_propagates_bounds_on_element() {
        let mut solver = TestSolver::default();

        let x_0 = solver.new_variable(3, 10);
        let x_1 = solver.new_variable(0, 15);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(14, 15);
        let constraint_tag = solver.new_constraint_tag();

        let index = solver.new_variable(1, 1);
        let rhs = solver.new_variable(6, 9);

        let _ = solver
            .new_propagator(ElementArgs {
                array: vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(x_1, 6, 9);

        assert_eq!(
            solver.get_reason_int(predicate![x_1 >= 6]),
            conjunction!([index == 1] & [rhs >= 6])
        );

        assert_eq!(
            solver.get_reason_int(predicate![x_1 <= 9]),
            conjunction!([index == 1] & [rhs <= 9])
        );
    }

    #[test]
    fn index_hole_propagates_bounds_on_rhs() {
        let mut solver = TestSolver::default();

        let x_0 = solver.new_variable(3, 10);
        let x_1 = solver.new_variable(0, 15);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(14, 15);
        let constraint_tag = solver.new_constraint_tag();

        let index = solver.new_variable(0, 3);
        solver.remove(index, 1).expect("Value can be removed");

        let rhs = solver.new_variable(-10, 30);

        let _ = solver
            .new_propagator(ElementArgs {
                array: vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(rhs, 3, 15);

        assert_eq!(
            solver.get_reason_int(predicate![rhs >= 3]),
            conjunction!([x_0 >= 3] & [x_2 >= 3] & [x_3 >= 3] & [index != 1])
        );

        assert_eq!(
            solver.get_reason_int(predicate![rhs <= 15]),
            conjunction!([x_0 <= 15] & [x_2 <= 15] & [x_3 <= 15] & [index != 1])
        );
    }

    #[test]
    fn holes_outside_union_bounds_are_ignored() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 4,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::NotEqual,
                value: 2,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x4",
            comparison: pumpkin_checking::Comparison::NotEqual,
            value: 2,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = ElementChecker::new(vec!["x1", "x2"].into(), "x3", "x4");

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }
}
