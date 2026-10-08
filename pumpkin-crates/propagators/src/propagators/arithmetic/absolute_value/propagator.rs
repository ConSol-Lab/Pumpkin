use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

/// Propagator for `absolute = |signed|`, where `absolute` and `signed` are integer variables.
///
/// The propagator is bounds consistent wrt signed. That means that if `signed \in {-2, -1, 1, 2}`,
/// the propagator will not propagate `[absolute >= 1]`.
#[derive(Clone, Debug)]
pub struct AbsoluteValuePropagator<VA, VB> {
    pub(super) signed: VA,
    pub(super) absolute: VB,
    pub(super) inference_code: InferenceCode,
}

impl<VA, VB> Propagator for AbsoluteValuePropagator<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "IntAbs"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        // The bound of absolute may be tightened further during propagation, but it is at least
        // zero at the root.
        context.post(
            predicate![self.absolute >= 0],
            (conjunction!(), &self.inference_code),
        )?;

        // Propagating absolute value can be broken into a few cases:
        // - `signed` is sign-fixed (i.e. `upper_bound <= 0` or `lower_bound >= 0`), in which case
        //   the bounds of `signed` can be propagated to `absolute` (taking care of swapping bounds
        //   when the `signed` is negative).
        // - `signed` is not sign-fixed (i.e. `lower_bound <= 0` and `upper_bound >= 0`), in which
        //   case the lower bound of `absolute` cannot be tightened without looking into specific
        //   domain values for `signed`, which we don't do.
        let signed_lb = context.lower_bound(&self.signed);
        let signed_ub = context.upper_bound(&self.signed);

        let signed_absolute_ub = i32::max(signed_lb.abs(), signed_ub.abs());

        context.post(
            predicate![self.absolute <= signed_absolute_ub],
            (
                conjunction!([self.signed >= signed_lb] & [self.signed <= signed_ub]),
                &self.inference_code,
            ),
        )?;

        if signed_lb > 0 {
            context.post(
                predicate![self.absolute >= signed_lb],
                (
                    conjunction!([self.signed >= signed_lb]),
                    &self.inference_code,
                ),
            )?;
        } else if signed_ub < 0 {
            context.post(
                predicate![self.absolute >= signed_ub.abs()],
                (
                    conjunction!([self.signed <= signed_ub]),
                    &self.inference_code,
                ),
            )?;
        }

        let absolute_ub = context.upper_bound(&self.absolute);
        let absolute_lb = context.lower_bound(&self.absolute);
        context.post(
            predicate![self.signed >= -absolute_ub],
            (
                conjunction!([self.absolute <= absolute_ub]),
                &self.inference_code,
            ),
        )?;
        context.post(
            predicate![self.signed <= absolute_ub],
            (
                conjunction!([self.absolute <= absolute_ub]),
                &self.inference_code,
            ),
        )?;

        if signed_ub <= 0 {
            context.post(
                predicate![self.signed <= -absolute_lb],
                (
                    conjunction!([self.signed <= 0] & [self.absolute >= absolute_lb]),
                    &self.inference_code,
                ),
            )?;
        } else if signed_lb >= 0 {
            context.post(
                predicate![self.signed >= absolute_lb],
                (
                    conjunction!([self.signed >= 0] & [self.absolute >= absolute_lb]),
                    &self.inference_code,
                ),
            )?;
        }

        Ok(())
    }
}
