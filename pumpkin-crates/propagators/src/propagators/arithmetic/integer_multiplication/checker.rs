use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;
use pumpkin_core::checkers::support::Support;
use pumpkin_core::checkers::support::SupportGenerator;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::checkers::support::UnsupportedValue;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

use super::constructor::ID_A;
use super::constructor::ID_B;
use super::constructor::ID_C;
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

impl<VA, VB, VC> SupportGenerator for IntegerMultiplicationChecker<VA, VB, VC>
where
    VA: IntegerVariable + SupportsValue<f32>,
    VB: IntegerVariable + SupportsValue<f32>,
    VC: IntegerVariable + SupportsValue<f32>,
{
    type Value = f32;

    fn support(
        &mut self,
        support: &mut Support<Self::Value>,
        local_id: LocalId,
        unsupported_value: UnsupportedValue,
        domains: &Domains<'_>,
    ) {
        let a_bounds = (
            domains.lower_bound(&self.a) as f32,
            domains.upper_bound(&self.a) as f32,
        );
        let b_bounds = (
            domains.lower_bound(&self.b) as f32,
            domains.upper_bound(&self.b) as f32,
        );
        let c_bounds = (
            domains.lower_bound(&self.c) as f32,
            domains.upper_bound(&self.c) as f32,
        );

        // The support is a real-valued assignment within the bounds, i.e. bounds(R) consistency is
        // checked. All three variables are always assigned: when no support exists, the assignment
        // violates either the product or a bound, which the retention checker then reports.
        let (value_a, value_b, value_c) = match local_id {
            ID_A => {
                let value_a = self.a.unpack(unsupported_value) as f32;
                let (value_b, value_c) = support_for_factor(value_a, b_bounds, c_bounds);
                (value_a, value_b, value_c)
            }
            ID_B => {
                let value_b = self.b.unpack(unsupported_value) as f32;
                let (value_a, value_c) = support_for_factor(value_b, a_bounds, c_bounds);
                (value_a, value_b, value_c)
            }
            ID_C => {
                let value_c = self.c.unpack(unsupported_value) as f32;
                let (value_a, value_b) = support_for_product(value_c, a_bounds, b_bounds);
                (value_a, value_b, value_c)
            }
            _ => unreachable!(),
        };

        self.a.assign(value_a, support);
        self.b.assign(value_b, support);
        self.c.assign(value_c, support);
    }

    fn is_solution(&self, support: &Support<Self::Value>) -> bool {
        let a = self.a.support_value(support);
        let b = self.b.support_value(support);
        let c = self.c.support_value(support);

        // One of the factors is obtained by division, so the product is compared up to rounding.
        (a * b - c).abs() <= rounding_tolerance(c)
    }
}

/// The absolute tolerance used when comparing `value` with a value computed by a division and a
/// multiplication in `f32`.
fn rounding_tolerance(value: f32) -> f32 {
    4.0 * f32::EPSILON * value.abs().max(1.0)
}

/// Returns whether `value` lies in `[lower, upper]` up to rounding.
fn is_within(value: f32, lower: f32, upper: f32) -> bool {
    let tolerance = rounding_tolerance(value);
    lower - tolerance <= value && value <= upper + tolerance
}

/// Given the value of one factor, selects a value for the other factor within `other_bounds` such
/// that the product lies within `product_bounds`. Returns the other factor and the product.
///
/// If no such value exists, the other factor is set to its lower bound; the product then lies
/// outside `product_bounds`, so the support is rejected.
fn support_for_factor(
    factor: f32,
    (other_min, other_max): (f32, f32),
    (product_min, product_max): (f32, f32),
) -> (f32, f32) {
    if factor == 0.0 {
        return (other_min, 0.0);
    }

    // The products reachable with `factor` form the interval between the products with the bounds
    // of the other factor.
    let (reachable_min, reachable_max) = {
        let with_min = factor * other_min;
        let with_max = factor * other_max;
        (with_min.min(with_max), with_min.max(with_max))
    };

    let product = reachable_min.max(product_min);
    if product > reachable_max.min(product_max) {
        return (other_min, factor * other_min);
    }

    let other = (product / factor).clamp(other_min, other_max);
    (other, product)
}

/// Selects values for both factors within their bounds whose product is `product`.
///
/// Every product attainable within the bounds is attained with at least one of the factors at one
/// of its bounds, so those four cases are exhaustive. If none applies, both factors are set to
/// their lower bounds; their product then differs from `product`, so the support is rejected.
fn support_for_product(
    product: f32,
    (a_min, a_max): (f32, f32),
    (b_min, b_max): (f32, f32),
) -> (f32, f32) {
    if product == 0.0 {
        if a_min <= 0.0 && 0.0 <= a_max {
            return (0.0, b_min);
        }
        if b_min <= 0.0 && 0.0 <= b_max {
            return (a_min, 0.0);
        }
        return (a_min, b_min);
    }

    for a in [a_min, a_max] {
        if a != 0.0 {
            let b = product / a;
            if is_within(b, b_min, b_max) {
                return (a, b.clamp(b_min, b_max));
            }
        }
    }

    for b in [b_min, b_max] {
        if b != 0.0 {
            let a = product / b;
            if is_within(a, a_min, a_max) {
                return (a.clamp(a_min, a_max), b);
            }
        }
    }

    (a_min, b_min)
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
