use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

#[cfg(doc)]
use super::MaximumArgs;

/// Bounds-consistent propagator which enforces `max(array) = rhs`. Can be constructed through
/// [`MaximumArgs`].
#[derive(Clone, Debug)]
pub struct MaximumPropagator<ElementVar, Rhs> {
    pub(crate) array: Box<[ElementVar]>,
    pub(crate) rhs: Rhs,
    pub(crate) inference_code: InferenceCode,
}

impl<ElementVar: IntegerVariable + 'static, Rhs: IntegerVariable + 'static> Propagator
    for MaximumPropagator<ElementVar, Rhs>
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "Maximum"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        // This is the constraint that is being propagated:
        // max(a_0, a_1, ..., a_{n-1}) = rhs

        let rhs_ub = context.upper_bound(&self.rhs);
        let mut max_ub = context.upper_bound(&self.array[0]);
        let mut max_lb = context.lower_bound(&self.array[0]);
        let mut lb_reason = predicate![self.array[0] >= max_lb];
        for var in self.array.iter() {
            // Rule 1.
            // UB(a_i) <= UB(rhs, constraint_tag }
            context.post(
                predicate![var <= rhs_ub],
                (conjunction!([self.rhs <= rhs_ub]), &self.inference_code),
            )?;

            let var_lb = context.lower_bound(var);
            let var_ub = context.upper_bound(var);

            if var_lb > max_lb {
                max_lb = var_lb;
                lb_reason = predicate![var >= var_lb];
            }

            if var_ub > max_ub {
                max_ub = var_ub;
            }
        }
        // Rule 2.
        // LB(rhs, constraint_tag } >= max{LB(a_i)}.
        context.post(
            predicate![self.rhs >= max_lb],
            (
                PropositionalConjunction::from(lb_reason),
                &self.inference_code,
            ),
        )?;

        // Rule 3.
        // UB(rhs, constraint_tag } <= max{UB(a_i)}.
        // Note that this implicitly also covers the rule:
        // 'if LB(rhs, constraint_tag } > UB(a_i) for all i, then conflict'.
        if rhs_ub > max_ub {
            let ub_reason: PropositionalConjunction = self
                .array
                .iter()
                .map(|var| predicate![var <= max_ub])
                .collect();
            context.post(
                predicate![self.rhs <= max_ub],
                (ub_reason, &self.inference_code),
            )?;
        }

        // Rule 4.
        // If there is only one variable with UB(a_i) >= LB(rhs, constraint_tag },
        // then the bounds for rhs and that variable should be intersected.
        let rhs_lb = context.lower_bound(&self.rhs);
        let mut propagating_variable: Option<&ElementVar> = None;
        let mut propagation_reason = PropositionalConjunction::default();
        for var in self.array.iter() {
            if context.upper_bound(var) >= rhs_lb {
                if propagating_variable.is_none() {
                    propagating_variable = Some(var);
                } else {
                    propagating_variable = None;
                    break;
                }
            } else {
                propagation_reason.push(predicate![var <= rhs_lb - 1]);
            }
        }
        // If there is exactly one variable UB(a_i) >= LB(rhs, constraint_tag }, then the
        // propagating variable is Some. In that case, intersect the bounds of that variable
        // and the rhs. Given previous rules, only the lower bound of the propagated
        // variable needs to be propagated.
        if let Some(propagating_variable) = propagating_variable {
            let var_lb = context.lower_bound(propagating_variable);
            if var_lb < rhs_lb {
                propagation_reason.push(predicate![self.rhs >= rhs_lb]);
                context.post(
                    predicate![propagating_variable >= rhs_lb],
                    (propagation_reason, &self.inference_code),
                )?;
            }
        }

        Ok(())
    }
}
