use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::checkers::ConsistencyChecker;
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

    fn add_inference_checkers(
        &self,
        mut checkers: InferenceCheckers<'_>,
    ) -> impl ConsistencyChecker + 'static {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, AbsoluteValue),
            Box::new(AbsoluteValueChecker {
                signed: self.signed.clone(),
                absolute: self.absolute.clone(),
            }),
        );

        #[allow(deprecated, reason = "TODO to implement for int abs")]
        pumpkin_core::propagation::checkers::DefaultChecker
    }

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

#[derive(Clone, Debug)]
pub struct AbsoluteValueChecker<VA, VB> {
    signed: VA,
    absolute: VB,
}

impl<VA, VB, Atomic> InferenceChecker<Atomic> for AbsoluteValueChecker<VA, VB>
where
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        let signed_lower = self.signed.induced_lower_bound(&state);
        let signed_upper = self.signed.induced_upper_bound(&state);
        let absolute_lower = self.absolute.induced_lower_bound(&state);
        let absolute_upper = self.absolute.induced_upper_bound(&state);

        if absolute_lower < 0 {
            // The absolute value cannot have negative values.
            return true;
        }

        // Now we compute the interval for |signed| based on the domain of signed.
        let (computed_signed_lower, computed_signed_upper) = if signed_lower >= 0 {
            (signed_lower, signed_upper)
        } else if signed_upper <= 0 {
            (-signed_upper, -signed_lower)
        } else if signed_lower < 0 && 0_i32 < signed_upper {
            (IntExt::Int(0), std::cmp::max(-signed_lower, signed_upper))
        } else {
            unreachable!()
        };

        // The intervals should not match, otherwise there is no conflict.
        computed_signed_lower != absolute_lower || computed_signed_upper != absolute_upper
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
