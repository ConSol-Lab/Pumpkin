use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

use super::checker::AbsoluteValueChecker;

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

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        } = self;

        let registration = EventsToRegister::builder()
            .add(&signed, DomainEvents::BOUNDS, LocalId::from(0))
            .add(&absolute, DomainEvents::BOUNDS, LocalId::from(1))
            .build();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            constraint_tag,
            AbsoluteValue,
            AbsoluteValueChecker {
                signed: signed.clone(),
                absolute: absolute.clone(),
            },
        );

        let propagator = AbsoluteValuePropagator {
            signed,
            absolute,
            inference_code,
        };

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator,
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
        // The bound of absolute may be tightened further during propagation,
        // but it is at least zero at the root.
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

        // The only absolute value which does not fit in an `i32` is `|i32::MIN|`,
        // in which case the corresponding propagation is skipped.
        let signed_absolute_ub = u32::max(signed_lb.unsigned_abs(), signed_ub.unsigned_abs());

        // We do lifting on the reason: The reason only states `signed` to be within the symmetric
        // interval `[-signed_absolute_ub, signed_absolute_ub]`,
        // which is weaker than its actual bounds.
        if let Ok(signed_absolute_ub) = i32::try_from(signed_absolute_ub) {
            context.post(
                predicate![self.absolute <= signed_absolute_ub],
                (
                    conjunction!(
                        [self.signed >= -signed_absolute_ub] & [self.signed <= signed_absolute_ub]
                    ),
                    &self.inference_code,
                ),
            )?;
        }

        if signed_lb > 0 {
            context.post(
                predicate![self.absolute >= signed_lb],
                (
                    conjunction!([self.signed >= signed_lb]),
                    &self.inference_code,
                ),
            )?;
        } else if signed_ub < 0
            && let Ok(signed_ub_abs) = i32::try_from(signed_ub.unsigned_abs())
        {
            context.post(
                predicate![self.absolute >= signed_ub_abs],
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

        // The bounds of `signed` are re-read since they may have been tightened above.
        let signed_lb = context.lower_bound(&self.signed);
        let signed_ub = context.upper_bound(&self.signed);

        // Let al = lower_bound(absolute).
        // We have that |signed| >= al, meaning that values in the interval (-al, al) are infeasible
        // for `signed`. This can potentially punch holes in the domain of `signed`,
        // but since we chose not to punch holes in this version of the propagator,
        // we only perform bound reasoning:
        // 1. If `signed_ub < al` then all non-negative values of `signed` are infeasible
        // and thus `signed <= -al`.
        // 2. Symmetrically, if `signed_lb > -al` then `signed >= al`.
        // 3. If both hold, the first point will propagate an empty domain.
        // Note that the reasons use the weakest bound on `signed` which implies the propagation,
        // e.g., `[signed <= al - 1]` rather than `[signed <= signed_ub]`.
        if absolute_lb > 0 {
            if signed_ub < absolute_lb {
                context.post(
                    predicate![self.signed <= -absolute_lb],
                    (
                        conjunction!(
                            [self.signed <= absolute_lb - 1] & [self.absolute >= absolute_lb]
                        ),
                        &self.inference_code,
                    ),
                )?;
            }

            if signed_lb > -absolute_lb {
                context.post(
                    predicate![self.signed >= absolute_lb],
                    (
                        conjunction!(
                            [self.signed >= -(absolute_lb - 1)] & [self.absolute >= absolute_lb]
                        ),
                        &self.inference_code,
                    ),
                )?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::state::State;

    use super::*;
    use crate::StateExt;

    #[test]
    fn absolute_bounds_are_propagated_at_initialise() {
        let mut state = State::default();

        let signed = state.new_interval_variable(-3, 4, None);
        let absolute = state.new_interval_variable(-2, 10, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(absolute, 0, 4);
    }

    #[test]
    fn signed_bounds_are_propagated_at_initialise() {
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, 5, None);
        let absolute = state.new_interval_variable(0, 3, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(signed, -3, 3);
    }

    #[test]
    fn absolute_lower_bound_can_be_strictly_positive() {
        let mut state = State::default();

        let signed = state.new_interval_variable(3, 6, None);
        let absolute = state.new_interval_variable(0, 10, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(absolute, 3, 6);
    }

    #[test]
    fn strictly_negative_signed_value_can_propagate_lower_bound_on_absolute() {
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, -3, None);
        let absolute = state.new_interval_variable(1, 5, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(absolute, 3, 5);
    }

    #[test]
    fn lower_bound_on_absolute_can_propagate_negative_upper_bound_on_signed() {
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, 0, None);
        let absolute = state.new_interval_variable(1, 5, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(signed, -5, -1);
    }

    #[test]
    fn lower_bound_on_absolute_can_propagate_positive_lower_bound_on_signed() {
        let mut state = State::default();

        let signed = state.new_interval_variable(1, 5, None);
        let absolute = state.new_interval_variable(3, 5, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(signed, 3, 5);
    }

    #[test]
    fn positive_signed_conflicts_with_small_absolute() {
        let mut state = State::default();

        let signed = state.new_interval_variable(5, 10, None);
        let absolute = state.new_interval_variable(0, 3, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn mixed_sign_signed_conflicts_with_large_absolute() {
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, 5, None);
        let absolute = state.new_interval_variable(6, 10, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn negative_absolute_conflicts_at_root() {
        let mut state = State::default();

        let signed = state.new_interval_variable(0, 3, None);
        let absolute = state.new_interval_variable(-5, -1, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn mixed_sign_signed_does_not_propagate_lower_bound_of_absolute() {
        // The propagator is only bounds consistent, so with `signed` in [-5, 5] it cannot tighten
        // `signed` based on `[absolute >= 3]`, and nothing changes.
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, 5, None);
        let absolute = state.new_interval_variable(3, 5, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(signed, -5, 5);
        state.assert_bounds(absolute, 3, 5);
    }

    #[test]
    fn mixed_sign_signed_is_tightened_when_only_one_sign_is_feasible() {
        // With `signed` in [-5, 2] and `absolute` in [3, 4], no non-negative value of `signed` can
        // reach `[absolute >= 3]`, so the bounds consistent domain of `signed` is [-4, -3].
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, 2, None);
        let absolute = state.new_interval_variable(3, 4, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(signed, -4, -3);
    }

    #[test]
    fn signed_fixed_to_zero_fixes_absolute_to_zero() {
        let mut state = State::default();

        let signed = state.new_interval_variable(0, 0, None);
        let absolute = state.new_interval_variable(0, 10, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(absolute, 0, 0);
    }

    #[test]
    fn absolute_fixed_to_zero_fixes_signed_to_zero() {
        let mut state = State::default();

        let signed = state.new_interval_variable(-5, 5, None);
        let absolute = state.new_interval_variable(0, 0, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(AbsoluteValueArgs {
            signed,
            absolute,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(signed, 0, 0);
    }
}
