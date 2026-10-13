use super::IntegerDivisionChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::RetentionCheck;
use crate::RetentionChecker;

/// The bounds of a variable or of a view over it.
#[derive(Clone, Copy, Debug)]
struct Bounds {
    lower: i64,
    upper: i64,
}

impl Bounds {
    fn of<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
        variable: &Var,
        state: &dyn DomainView<Atomic>,
    ) -> Bounds {
        let lower: i32 = variable
            .induced_lower_bound(state)
            .try_into()
            .expect("the domains of a retention check are bounded");
        let upper: i32 = variable
            .induced_upper_bound(state)
            .try_into()
            .expect("the domains of a retention check are bounded");
        Bounds {
            lower: i64::from(lower),
            upper: i64::from(upper),
        }
    }

    /// The bounds of the view `-x`.
    fn negated(self) -> Bounds {
        Bounds {
            lower: -self.upper,
            upper: -self.lower,
        }
    }
}

/// Mirrors one pass of the propagation of the division propagator, which performs truncating
/// division: the propagator has nothing left to propagate if no step of that pass would tighten a
/// bound.
impl<VA, VB, VC, Atomic> RetentionChecker<Atomic> for IntegerDivisionChecker<VA, VB, VC>
where
    Atomic: AtomicConstraint,
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    VC: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        let mut numerator = Bounds::of(&self.numerator, state);
        let mut denominator = Bounds::of(&self.denominator, state);
        let rhs = Bounds::of(&self.rhs, state);

        if denominator.lower < 0 && denominator.upper > 0 {
            // The propagator does nothing until the sign of the denominator is fixed.
            return RetentionCheck::NothingToPropagate;
        }

        if denominator.upper < 0 {
            // A negative denominator is handled through the negated numerator and denominator.
            numerator = numerator.negated();
            denominator = denominator.negated();
        }
        let negated_numerator = numerator.negated();
        let negated_rhs = rhs.negated();

        let missed = signs_can_be_propagated(numerator, rhs)
            || (numerator.upper >= 0
                && rhs.upper >= 0
                && upper_bounds_can_be_propagated(numerator, denominator, rhs))
            || (negated_numerator.upper >= 0
                && negated_rhs.upper >= 0
                && upper_bounds_can_be_propagated(negated_numerator, denominator, negated_rhs))
            || (numerator.lower >= 0
                && rhs.lower >= 0
                && positive_domains_can_be_propagated(numerator, denominator, rhs))
            || (negated_numerator.lower >= 0
                && negated_rhs.lower >= 0
                && positive_domains_can_be_propagated(negated_numerator, denominator, negated_rhs));

        if missed {
            log::error!(
                "The division {:?} / {:?} = {:?} can still be propagated: {numerator:?} / {denominator:?} = {rhs:?}",
                self.numerator,
                self.denominator,
                self.rhs
            );
            RetentionCheck::PropagationMissed
        } else {
            RetentionCheck::NothingToPropagate
        }
    }
}

/// Whether the propagator would tighten a bound so that the signs of the numerator and the
/// right-hand side agree, given a positive denominator.
fn signs_can_be_propagated(numerator: Bounds, rhs: Bounds) -> bool {
    (numerator.lower >= 0 && rhs.lower < 0)
        || (numerator.lower <= 0 && rhs.lower > 0)
        || (numerator.upper <= 0 && rhs.upper > 0)
        || (numerator.upper >= 0 && rhs.upper < 0)
}

/// Whether the propagator would lower the upper bound of the right-hand side or of the numerator,
/// for a non-negative numerator and right-hand side and a positive denominator.
fn upper_bounds_can_be_propagated(numerator: Bounds, denominator: Bounds, rhs: Bounds) -> bool {
    let new_max_rhs = numerator.upper / denominator.lower;
    let new_max_numerator = (rhs.upper + 1) * denominator.upper - 1;

    rhs.upper > new_max_rhs || numerator.upper > new_max_numerator
}

/// Whether the propagator would tighten a bound of one of the three variables when all of them
/// are non-negative.
fn positive_domains_can_be_propagated(numerator: Bounds, denominator: Bounds, rhs: Bounds) -> bool {
    let new_min_rhs = numerator.lower / denominator.upper;
    if rhs.lower < new_min_rhs {
        return true;
    }

    let new_min_numerator = denominator.lower * rhs.lower;
    if numerator.lower < new_min_numerator {
        return true;
    }

    if rhs.lower > 0 && denominator.upper > numerator.upper / rhs.lower {
        return true;
    }

    // `ceil((lb(numerator) + 1) / (ub(rhs) + 1))`, the smallest denominator the propagator allows.
    let dividend = numerator.lower + 1;
    let positive_divisor = rhs.upper + 1;
    let new_min_denominator = dividend / positive_divisor
        + i64::from((dividend / positive_divisor) * positive_divisor < dividend);

    denominator.lower < new_min_denominator
}
