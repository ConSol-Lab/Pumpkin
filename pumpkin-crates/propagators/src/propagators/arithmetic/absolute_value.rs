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

        // A conflict is detected if the interval of the domains of |signed| `absolute` share no
        // value, i.e., their intersection is empty.
        // Note that a negative lower bound on `absolute` needs no special handling:
        // the lower bound of |signed| is at least zero, so the check then reduces to comparing
        // against the upper bound of `absolute`.
        computed_signed_upper < absolute_lower || absolute_upper < computed_signed_lower
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_checking::Comparison;
    use pumpkin_checking::TestAtomic;
    use pumpkin_checking::VariableState;
    use pumpkin_core::state::State;

    use super::*;
    use crate::StateExt;

    /// Helper function to build test cases.
    /// Uses the checker [`AbsoluteValueChecker`] to establish whether the provided instance is
    /// a conflict. The input contains the intervals of the domains of the variables.
    /// Returns true if the instance is infeasible.
    fn check_absolute_value_conflict(
        signed_bounds: (i32, i32),
        absolute_bounds: (i32, i32),
    ) -> bool {
        let (signed_lower, signed_upper) = signed_bounds;
        let (absolute_lower, absolute_upper) = absolute_bounds;

        let premises = [
            TestAtomic {
                name: "signed",
                comparison: Comparison::GreaterEqual,
                value: signed_lower,
            },
            TestAtomic {
                name: "signed",
                comparison: Comparison::LessEqual,
                value: signed_upper,
            },
            TestAtomic {
                name: "absolute",
                comparison: Comparison::GreaterEqual,
                value: absolute_lower,
            },
            TestAtomic {
                name: "absolute",
                comparison: Comparison::LessEqual,
                value: absolute_upper,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = AbsoluteValueChecker {
            signed: "signed",
            absolute: "absolute",
        };

        checker.check(state, &premises, None)
    }

    #[test]
    fn absolute_value_feasible1() {
        assert!(!check_absolute_value_conflict((-5, 5), (2, 5)));
    }

    #[test]
    fn absolute_value_feasible2() {
        assert!(!check_absolute_value_conflict((5, 20), (10, 30)));
    }

    #[test]
    fn absolute_value_feasible3() {
        assert!(!check_absolute_value_conflict((-20, 20), (10, 30)));
    }

    #[test]
    fn absolute_value_feasible4() {
        assert!(!check_absolute_value_conflict((-10, 10), (0, 9)));
    }

    #[test]
    fn absolute_value_feasible5() {
        assert!(!check_absolute_value_conflict((-20, -5), (10, 30)));
    }

    #[test]
    fn absolute_value_feasible6() {
        assert!(!check_absolute_value_conflict((-5, 5), (5, 8)));
    }

    #[test]
    fn absolute_value_feasible7() {
        assert!(!check_absolute_value_conflict((5, 10), (0, 5)));
    }

    #[test]
    fn absolute_value_feasible8() {
        assert!(!check_absolute_value_conflict((2, 5), (2, 5)));
    }

    #[test]
    fn absolute_value_feasible9() {
        assert!(!check_absolute_value_conflict((0, 5), (-2, 10)));
    }

    #[test]
    fn absolute_value_feasible10() {
        assert!(!check_absolute_value_conflict((-3, 3), (0, 0)));
    }

    #[test]
    fn absolute_value_feasible11() {
        let premises = [
            TestAtomic {
                name: "signed",
                comparison: Comparison::GreaterEqual,
                value: 1,
            },
            TestAtomic {
                name: "signed",
                comparison: Comparison::LessEqual,
                value: 2,
            },
            TestAtomic {
                name: "absolute",
                comparison: Comparison::LessEqual,
                value: 5,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomic constraints");

        let checker = AbsoluteValueChecker {
            signed: "signed",
            absolute: "absolute",
        };

        assert!(!checker.check(state, &premises, None));
    }

    #[test]
    fn absolute_value_infeasible1() {
        assert!(check_absolute_value_conflict((-5, 5), (6, 10)));
    }

    #[test]
    fn absolute_value_infeasible2() {
        assert!(check_absolute_value_conflict((5, 10), (0, 3)));
    }

    #[test]
    fn absolute_value_infeasible3() {
        assert!(check_absolute_value_conflict((-10, -5), (0, 3)));
    }

    #[test]
    fn absolute_value_infeasible4() {
        assert!(check_absolute_value_conflict((3, 5), (-10, -1)));
    }

    #[test]
    fn absolute_value_infeasible5() {
        assert!(check_absolute_value_conflict((-5, 5), (6, 6)));
    }

    #[test]
    fn absolute_value_infeasible6() {
        assert!(check_absolute_value_conflict((1, 3), (0, 0)));
    }

    #[test]
    fn absolute_value_infeasible7() {
        let premises = [TestAtomic {
            name: "absolute",
            comparison: Comparison::LessEqual,
            value: -1,
        }];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomic constraints");

        let checker = AbsoluteValueChecker {
            signed: "signed",
            absolute: "absolute",
        };

        assert!(checker.check(state, &premises, None));
    }

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
