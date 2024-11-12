use bitfield_struct::bitfield;

use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::propagation::ReadDomains;
use crate::engine::reason::Reason;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;

/// Arc-consistent propagator for constraint `element([x_1, \ldots, x_n], i, e)`, where `x_j` are
///  variables, `i` is an integer variable, and `e` is a variable, which holds iff `x_i = e`
///
/// Note that this propagator is 0-indexed
#[derive(Clone, Debug)]
pub(crate) struct ElementPropagator<VX, VI, VE> {
    array: Box<[VX]>,
    index: VI,
    rhs: VE,

    rhs_reason_buffer: Vec<Predicate>,
}

impl<VX, VI, VE> ElementPropagator<VX, VI, VE> {
    pub(crate) fn new(array: Box<[VX]>, index: VI, rhs: VE) -> Self {
        Self {
            array,
            index,
            rhs,
            rhs_reason_buffer: vec![],
        }
    }
}

const ID_INDEX: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);

// local ids of array vars are shifted by ID_X_OFFSET
const ID_X_OFFSET: u32 = 2;

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

    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        self.array.iter().enumerate().for_each(|(i, x_i)| {
            let _ = context.register(
                x_i.clone(),
                DomainEvents::ANY_INT,
                LocalId::from(i as u32 + ID_X_OFFSET),
            );
        });
        let _ = context.register(self.index.clone(), DomainEvents::ANY_INT, ID_INDEX);
        let _ = context.register(self.rhs.clone(), DomainEvents::ANY_INT, ID_RHS);
        Ok(())
    }

    fn lazy_explanation(&mut self, code: u64, _: &Assignments) -> &[Predicate] {
        let payload = RightHandSideReason::from_bits(code);

        self.rhs_reason_buffer.clear();
        self.rhs_reason_buffer
            .extend(self.array.iter().map(|variable| match payload.bound() {
                Bound::Lower => predicate![variable >= payload.value()],
                Bound::Upper => predicate![variable <= payload.value()],
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
        context.set_lower_bound(&self.index, 0, conjunction!())?;
        context.set_upper_bound(&self.index, self.array.len() as i32 - 1, conjunction!())?;
        Ok(())
    }

    /// The lower bound (resp. upper bound) of the right-hand side can be the minimum lower
    /// bound (res. maximum upper bound) of the elements.
    fn propagate_rhs_bounds_based_on_array(
        &self,
        context: &mut PropagationContextMut<'_>,
    ) -> PropagationStatusCP {
        let (rhs_lb, rhs_ub) =
            self.array
                .iter()
                .fold((i32::MAX, i32::MIN), |(rhs_lb, rhs_ub), element| {
                    (
                        i32::min(rhs_lb, context.lower_bound(element)),
                        i32::max(rhs_ub, context.upper_bound(element)),
                    )
                });

        context.set_lower_bound(
            &self.rhs,
            rhs_lb,
            Reason::DynamicLazy(
                RightHandSideReason::new()
                    .with_bound(Bound::Lower)
                    .with_value(rhs_ub)
                    .into_bits(),
            ),
        )?;
        context.set_upper_bound(
            &self.rhs,
            rhs_ub,
            Reason::DynamicLazy(
                RightHandSideReason::new()
                    .with_bound(Bound::Upper)
                    .with_value(rhs_ub)
                    .into_bits(),
            ),
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
            context.remove(&self.index, idx, reason)?;
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

        context.set_lower_bound(
            lhs,
            rhs_lb,
            conjunction!([self.rhs >= rhs_lb] & [self.index == index]),
        )?;
        context.set_upper_bound(
            lhs,
            rhs_ub,
            conjunction!([self.rhs <= rhs_ub] & [self.index == index]),
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

        let _ = solver
            .new_propagator(ElementPropagator::new(
                vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
            ))
            .expect("no empty domains");

        solver.assert_bounds(index, 0, 2);

        assert_eq!(
            solver.get_reason_int(predicate![index != 3]),
            &conjunction!([x_3 >= 10] & [rhs <= 9])
        );

        assert_eq!(
            solver.get_reason_int(predicate![index != 1]),
            &conjunction!([x_1 <= 5] & [rhs >= 6])
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

        let _ = solver
            .new_propagator(ElementPropagator::new(
                vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
            ))
            .expect("no empty domains");

        solver.assert_bounds(rhs, 2, 15);

        assert_eq!(
            solver.get_reason_int(predicate![rhs >= 2]),
            &conjunction!([x_0 >= 2] & [x_1 >= 2] & [x_2 >= 2] & [x_3 >= 2])
        );

        assert_eq!(
            solver.get_reason_int(predicate![rhs <= 15]),
            &conjunction!([x_0 <= 15] & [x_1 <= 15] & [x_2 <= 15] & [x_3 <= 15])
        );
    }

    #[test]
    fn fixed_index_propagates_bounds_on_element() {
        let mut solver = TestSolver::default();

        let x_0 = solver.new_variable(3, 10);
        let x_1 = solver.new_variable(0, 15);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(14, 15);

        let index = solver.new_variable(1, 1);
        let rhs = solver.new_variable(6, 9);

        let _ = solver
            .new_propagator(ElementPropagator::new(
                vec![x_0, x_1, x_2, x_3].into(),
                index,
                rhs,
            ))
            .expect("no empty domains");

        solver.assert_bounds(x_1, 6, 9);

        assert_eq!(
            solver.get_reason_int(predicate![x_1 >= 6]),
            &conjunction!([index == 1] & [rhs >= 6])
        );

        assert_eq!(
            solver.get_reason_int(predicate![x_1 <= 9]),
            &conjunction!([index == 1] & [rhs <= 9])
        );
    }
}
