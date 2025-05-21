use bitfield_struct::bitfield;

use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::declare_inference_label;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::ReadDomains;
use crate::engine::reason::Reason;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;

#[derive(Clone, Debug)]
pub(crate) struct ElementArgs<VX, VI, VE> {
    pub(crate) array: Box<[VX]>,
    pub(crate) index: VI,
    pub(crate) rhs: VE,
    pub(crate) constraint_tag: ConstraintTag,
}

declare_inference_label!(Element);

impl<VX, VI, VE> PropagatorConstructor for ElementArgs<VX, VI, VE>
where
    VX: IntegerVariable + 'static,
    VI: IntegerVariable + 'static,
    VE: IntegerVariable + 'static,
{
    type PropagatorImpl = ElementPropagator<VX, VI, VE>;

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

        let inference_code = context.create_inference_code(constraint_tag, Element);

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
pub(crate) struct ElementPropagator<VX, VI, VE> {
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
    fn priority(&self) -> u32 {
        2
    }

    fn name(&self) -> &str {
        "Element"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
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
        context: &mut PropagationContextMut<'_>,
    ) -> PropagationStatusCP {
        context.post(
            predicate![self.index >= 0],
            conjunction!(),
            self.inference_code,
        )?;
        context.post(
            predicate![self.index <= self.array.len() as i32 - 1],
            conjunction!(),
            self.inference_code,
        )?;
        Ok(())
    }

    /// The lower bound (resp. upper bound) of the right-hand side can be the minimum lower
    /// bound (res. maximum upper bound) of the elements.
    fn propagate_rhs_bounds_based_on_array(
        &self,
        context: &mut PropagationContextMut<'_>,
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
            self.inference_code,
        )?;
        context.post(
            predicate![self.rhs <= rhs_ub],
            Reason::DynamicLazy(
                RightHandSideReason::new()
                    .with_bound(Bound::Upper)
                    .with_value(rhs_ub)
                    .into_bits(),
            ),
            self.inference_code,
        )?;

        Ok(())
    }

    /// Go through the array. For every element for which the domain does not intersect with the
    /// right-hand side, remove it from index.
    fn propagate_index_based_on_domain_intersection_with_rhs(
        &self,
        context: &mut PropagationContextMut<'_>,
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
            context.post(predicate![self.index != idx], reason, self.inference_code)?;
        }

        Ok(())
    }

    /// Propagate equality between lhs and rhs. This assumes the bounds of rhs have already been
    /// tightened to the bounds of lhs, through a previous propagation rule.
    fn propagate_equality(
        &self,
        context: &mut PropagationContextMut<'_>,
        index: i32,
    ) -> PropagationStatusCP {
        let rhs_lb = context.lower_bound(&self.rhs);
        let rhs_ub = context.upper_bound(&self.rhs);
        let lhs = &self.array[index as usize];

        context.post(
            predicate![lhs >= rhs_lb],
            conjunction!([self.rhs >= rhs_lb] & [self.index == index]),
            self.inference_code,
        )?;
        context.post(
            predicate![lhs <= rhs_ub],
            conjunction!([self.rhs <= rhs_ub] & [self.index == index]),
            self.inference_code,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;

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
}
