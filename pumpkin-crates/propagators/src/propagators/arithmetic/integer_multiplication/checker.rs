use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use super::shared::compute_quotient_bound_ext;
use super::shared::product_bound_ext;

/// Verifies that a claimed inference for `a * b = c` is actually implied by its premises.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationChecker<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

impl<VA, VB, VC, Atomic> InferenceChecker<Atomic> for IntegerMultiplicationChecker<VA, VB, VC>
where
    Atomic: AtomicConstraint,
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    VC: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
        _premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        let a_min = self.a.induced_lower_bound(&state).into();
        let a_max = self.a.induced_upper_bound(&state).into();
        let b_min = self.b.induced_lower_bound(&state).into();
        let b_max = self.b.induced_upper_bound(&state).into();
        let c_min = self.c.induced_lower_bound(&state).into();
        let c_max = self.c.induced_upper_bound(&state).into();

        let check_c = || {
            let (lo, hi) = product_bound_ext(a_min, a_max, b_min, b_max);
            is_disjoint(lo, hi, c_min, c_max)
        };
        let check_a = || {
            compute_quotient_bound_ext(c_min, c_max, b_min, b_max)
                .is_some_and(|(lo, hi)| is_disjoint(lo, hi, a_min, a_max))
        };
        let check_b = || {
            compute_quotient_bound_ext(c_min, c_max, a_min, a_max)
                .is_some_and(|(lo, hi)| is_disjoint(lo, hi, b_min, b_max))
        };

        match consequent {
            Some(atomic) if self.c.does_atomic_constrain_self(atomic) => check_c(),
            Some(atomic) if self.a.does_atomic_constrain_self(atomic) => check_a(),
            Some(atomic) if self.b.does_atomic_constrain_self(atomic) => check_b(),
            Some(_) => unreachable!(),
            None => check_c() || check_a() || check_b(),
        }
    }
}

/// Returns whether `[induced_lo, induced_hi]` shares no value with `[lo, hi]`.
fn is_disjoint(
    lo: IntExt<i64>,
    hi: IntExt<i64>,
    induced_lo: IntExt<i64>,
    induced_hi: IntExt<i64>,
) -> bool {
    induced_hi < lo || induced_lo > hi
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checker_detects_a_pure_conflict_with_no_consequent() {
        // `consequent: None` is how the checker is invoked for a propagator-reported conflict
        // that isn't a single propagated predicate (see `VariableState::prepare_for_conflict_check`
        // and `State::check_conflict`). The checker must not just reject these outright: it needs
        // to confirm the premises alone are already contradictory.
        use pumpkin_checking::Comparison;
        use pumpkin_checking::TestAtomic;
        use pumpkin_checking::VariableState;

        let premises = [
            TestAtomic {
                name: "a",
                comparison: Comparison::Equal,
                value: 3,
            },
            TestAtomic {
                name: "b",
                comparison: Comparison::Equal,
                value: 4,
            },
            TestAtomic {
                name: "c",
                comparison: Comparison::Equal,
                value: 10,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = IntegerMultiplicationChecker {
            a: "a",
            b: "b",
            c: "c",
        };

        // 3 * 4 = 12 != 10, so this is a genuine conflict.
        assert!(checker.check(state, &premises, None));
    }

    #[test]
    fn checker_does_not_report_a_conflict_for_consistent_premises_with_no_consequent() {
        use pumpkin_checking::Comparison;
        use pumpkin_checking::TestAtomic;
        use pumpkin_checking::VariableState;

        let premises = [TestAtomic {
            name: "a",
            comparison: Comparison::Equal,
            value: 3,
        }];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = IntegerMultiplicationChecker {
            a: "a",
            b: "b",
            c: "c",
        };

        // `b` and `c` are unconstrained, so `a = 3` alone can't be a conflict.
        assert!(!checker.check(state, &premises, None));
    }
}
