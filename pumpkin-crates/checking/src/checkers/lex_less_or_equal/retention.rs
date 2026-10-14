use super::LexLessOrEqualChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::RetentionCheck;
use crate::RetentionChecker;

/// At the fixpoint of the rule, let `alpha` be the first position at which `x` and `y` are not
/// both fixed to the same value. Every earlier position is equal, so `x[alpha] <= y[alpha]`, and
/// `x[alpha] < y[alpha]` when the positions after `alpha` cannot be lexicographically ordered: at
/// the first position after `alpha` where the lower bound of `x` differs from the upper bound of
/// `y`, the lower bound is greater. The bounds of `x[alpha]` and `y[alpha]` are those that these
/// inequalities leave; no position after `alpha` is propagated.
impl<Var, Atomic> RetentionChecker<Atomic> for LexLessOrEqualChecker<Var>
where
    Atomic: AtomicConstraint,
    Var: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        let length = self.x.len().min(self.y.len());

        let Some(alpha) = (0..length).find(|&position| {
            let x = self.x[position].induced_fixed_value(state);
            x.is_none() || x != self.y[position].induced_fixed_value(state)
        }) else {
            return RetentionCheck::NothingToPropagate;
        };

        let is_strict = (alpha + 1..length)
            .map(|position| {
                (
                    self.x[position].induced_lower_bound(state),
                    self.y[position].induced_upper_bound(state),
                )
            })
            .find(|(lower, upper)| lower != upper)
            .is_some_and(|(lower, upper)| lower > upper);
        let gap = i32::from(is_strict);

        let (x, y) = (&self.x[alpha], &self.y[alpha]);
        let x_lower = x.induced_lower_bound(state);
        let x_upper = x.induced_upper_bound(state);
        let y_lower = y.induced_lower_bound(state);
        let y_upper = y.induced_upper_bound(state);

        if x_upper > y_upper + (-gap) {
            log::error!(
                "The upper bound of {x:?} at position {alpha} could be lowered to {:?}, the upper \
                 bound of {y:?} minus {gap}",
                y_upper + (-gap)
            );
            return RetentionCheck::PropagationMissed;
        }

        if y_lower < x_lower + gap {
            log::error!(
                "The lower bound of {y:?} at position {alpha} could be raised to {:?}, the lower \
                 bound of {x:?} plus {gap}",
                x_lower + gap
            );
            return RetentionCheck::PropagationMissed;
        }

        RetentionCheck::NothingToPropagate
    }
}
