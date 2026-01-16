use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

declare_inference_label!(AbsoluteValue);

#[derive(Clone, Debug)]
pub struct AbsoluteValueArgs<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB> PropagatorConstructor for AbsoluteValueArgs<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    type PropagatorImpl = AbsoluteValuePropagator<VA, VB>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        } = self;

        context.register(signed.clone(), DomainEvents::BOUNDS, LocalId::from(0));
        context.register(absolute.clone(), DomainEvents::BOUNDS, LocalId::from(1));

        let inference_code = InferenceCode::new(constraint_tag, AbsoluteValue);

        AbsoluteValuePropagator {
            signed,
            absolute,
            inference_code,
        }
    }
}

/// Propagator for `absolute = |signed|`, where `absolute` and `signed` are integer variables.
///
/// The propagator is bounds consistent wrt signed. That means that if `signed \in {-2, -1, 1, 2}`,
/// the propagator will not propagate `[absolute >= 1]`.
#[derive(Clone, Debug)]
pub struct AbsoluteValuePropagator<VA, VB> {
    signed: VA,
    absolute: VB,
    inference_code: InferenceCode,
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
            conjunction!(),
            &self.inference_code,
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
            conjunction!([self.signed >= signed_lb] & [self.signed <= signed_ub]),
            &self.inference_code,
        )?;

        if signed_lb > 0 {
            context.post(
                predicate![self.absolute >= signed_lb],
                conjunction!([self.signed >= signed_lb]),
                &self.inference_code,
            )?;
        } else if signed_ub < 0 {
            context.post(
                predicate![self.absolute >= signed_ub.abs()],
                conjunction!([self.signed <= signed_ub]),
                &self.inference_code,
            )?;
        }

        let absolute_ub = context.upper_bound(&self.absolute);
        let absolute_lb = context.lower_bound(&self.absolute);
        context.post(
            predicate![self.signed >= -absolute_ub],
            conjunction!([self.absolute <= absolute_ub]),
            &self.inference_code,
        )?;
        context.post(
            predicate![self.signed <= absolute_ub],
            conjunction!([self.absolute <= absolute_ub]),
            &self.inference_code,
        )?;

        if signed_ub <= 0 {
            context.post(
                predicate![self.signed <= -absolute_lb],
                conjunction!([self.signed <= 0] & [self.absolute >= absolute_lb]),
                &self.inference_code,
            )?;
        } else if signed_lb >= 0 {
            context.post(
                predicate![self.signed >= absolute_lb],
                conjunction!([self.signed >= 0] & [self.absolute >= absolute_lb]),
                &self.inference_code,
            )?;
        }

        Ok(())
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_core::TestSolver;

    use super::*;

    #[test]
    fn absolute_bounds_are_propagated_at_initialise() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-3, 4);
        let absolute = solver.new_variable(-2, 10);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(AbsoluteValueArgs {
                signed,
                absolute,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(absolute, 0, 4);
    }

    #[test]
    fn signed_bounds_are_propagated_at_initialise() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-5, 5);
        let absolute = solver.new_variable(0, 3);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(AbsoluteValueArgs {
                signed,
                absolute,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(signed, -3, 3);
    }

    #[test]
    fn absolute_lower_bound_can_be_strictly_positive() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(3, 6);
        let absolute = solver.new_variable(0, 10);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(AbsoluteValueArgs {
                signed,
                absolute,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(absolute, 3, 6);
    }

    #[test]
    fn strictly_negative_signed_value_can_propagate_lower_bound_on_absolute() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-5, -3);
        let absolute = solver.new_variable(1, 5);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(AbsoluteValueArgs {
                signed,
                absolute,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(absolute, 3, 5);
    }

    #[test]
    fn lower_bound_on_absolute_can_propagate_negative_upper_bound_on_signed() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-5, 0);
        let absolute = solver.new_variable(1, 5);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(AbsoluteValueArgs {
                signed,
                absolute,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(signed, -5, -1);
    }

    #[test]
    fn lower_bound_on_absolute_can_propagate_positive_lower_bound_on_signed() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(1, 5);
        let absolute = solver.new_variable(3, 5);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(AbsoluteValueArgs {
                signed,
                absolute,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.assert_bounds(signed, 3, 5);
    }
}
