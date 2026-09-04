use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;

#[derive(Clone, Debug)]
pub struct AbsoluteValueChecker<VA, VB> {
    pub(crate) signed: VA,
    pub(crate) absolute: VB,
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

    use super::*;

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
}
