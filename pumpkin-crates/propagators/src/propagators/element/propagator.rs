//! Contains the propagator for the [Element](https://sofdem.github.io/gccat/gccat/Celement.html)
//! constraint.
#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]

use bitfield_struct::bitfield;
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Reason;

/// Arc-consistent propagator for constraint `element([x_1, \ldots, x_n], i, e)`, where `x_j` are
///  variables, `i` is an integer variable, and `e` is a variable, which holds iff `x_i = e`
///
/// Note that this propagator is 0-indexed
#[derive(Clone, Debug)]
pub struct ElementPropagator<VX, VI, VE> {
    pub(super) array: Box<[VX]>,
    pub(super) index: VI,
    pub(super) rhs: VE,
    pub(super) inference_code: InferenceCode,

    pub(super) rhs_reason_buffer: Vec<Predicate>,
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

        if let Some(idx) = context.fixed_value(&self.index) {
            self.propagate_equality(&mut context, idx)?;
        }

        Ok(())
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> LazyExplanation<'_> {
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

        LazyExplanation {
            predicates: self.rhs_reason_buffer.as_slice(),
            inference_code: self.inference_code.clone(),
        }
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
            (conjunction!(), &self.inference_code),
        )?;
        context.post(
            predicate![self.index <= self.array.len() as i32 - 1],
            (conjunction!(), &self.inference_code),
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
        )?;
        context.post(
            predicate![self.rhs <= rhs_ub],
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
            context.post(
                predicate![self.index != idx],
                (reason, &self.inference_code),
            )?;
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
            (
                conjunction!([self.rhs >= rhs_lb] & [self.index == index]),
                &self.inference_code,
            ),
        )?;
        context.post(
            predicate![lhs <= rhs_ub],
            (
                conjunction!([self.rhs <= rhs_ub] & [self.index == index]),
                &self.inference_code,
            ),
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
