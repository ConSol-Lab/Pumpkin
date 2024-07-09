use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::engine::DomainEvents;
use crate::pumpkin_assert_simple;

/// A propagator for maintaining the constraint `numerator / denominator = rhs`; note that this
/// propagator performs truncating division (i.e. rounding towards 0).
///
/// The propagator assumes that the `denominator` is a (non-zero) number.
///
/// The implementation is ported from [OR-tools](https://github.com/google/or-tools/blob/870edf6f7bff6b8ff0d267d936be7e331c5b8c2d/ortools/sat/integer_expr.cc#L1209C1-L1209C19).
#[derive(Debug)]
pub(crate) struct DivisionPropagator<VA, VB, VC> {
    numerator: VA,
    denominator: VB,
    rhs: VC,
}

#[derive(Debug)]
pub(crate) struct DivisionConstructor<VA, VB, VC> {
    pub(crate) numerator: VA,
    pub(crate) denominator: VB,
    pub(crate) rhs: VC,
}

const ID_NUMERATOR: LocalId = LocalId::from(0);
const ID_DENOMINATOR: LocalId = LocalId::from(1);
const ID_RHS: LocalId = LocalId::from(2);

impl<VA, VB, VC> PropagatorConstructor for DivisionConstructor<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    type Propagator = DivisionPropagator<VA, VB, VC>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        DivisionPropagator {
            numerator: context.register(self.numerator, DomainEvents::BOUNDS, ID_NUMERATOR, false),
            denominator: context.register(
                self.denominator,
                DomainEvents::BOUNDS,
                ID_DENOMINATOR,
                false,
            ),
            rhs: context.register(self.rhs, DomainEvents::BOUNDS, ID_RHS, false),
        }
    }
}

impl<VA, VB, VC> Propagator for DivisionPropagator<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        perform_propagation(context, &self.numerator, &self.denominator, &self.rhs)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "Division"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        if context.contains(&self.denominator, 0) {
            pumpkin_assert_simple!(
                !context.contains(&self.denominator, 0),
                "Denominator cannot contain 0"
            );
        }
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        perform_propagation(context, &self.numerator, &self.denominator, &self.rhs)
    }
}

fn perform_propagation<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    context: &mut PropagationContextMut,
    numerator: &VA,
    denominator: &VB,
    rhs: &VC,
) -> PropagationStatusCP {
    if context.lower_bound(denominator) < 0 && context.upper_bound(denominator) > 0 {
        // For now we don't do anything in this case, note that this will not lead to incorrect
        // behaviour since any solution to this constraint will necessarily have to fix the
        // denominator.
        return Ok(());
    }

    let mut negated_numerator = &numerator.scaled(-1);
    let mut numerator = &numerator.scaled(1);

    let mut negated_denominator = &denominator.scaled(-1);
    let mut denominator = &denominator.scaled(1);

    if context.upper_bound(denominator) < 0 {
        // If the denominator is negative then we swap the numerator with its negated version and we
        // swap the denominator with its negated version.
        std::mem::swap(&mut numerator, &mut negated_numerator);
        std::mem::swap(&mut denominator, &mut negated_denominator);
    }

    let negated_rhs = &rhs.scaled(-1);

    // We propagate the domains to their appropriate signs (e.g. if the numerator is negative and
    // the denominator is positive then the rhs should also be negative)
    propagate_signs(context, numerator, denominator, rhs)?;

    // If the upper-bound of the numerator is positive and the upper-bound of the rhs is positive
    // then we can simply update the upper-bounds
    if context.upper_bound(numerator) >= 0 && context.upper_bound(rhs) >= 0 {
        propagate_upper_bounds(context, numerator, denominator, rhs)?;
    }

    // If the lower-bound of the numerator is negative and the lower-bound of the rhs is negative
    // then we negate these variables and update the upper-bounds
    if context.upper_bound(negated_numerator) >= 0 && context.upper_bound(negated_rhs) >= 0 {
        propagate_upper_bounds(context, negated_numerator, denominator, negated_rhs)?;
    }

    // If the domain of the numerator is positive and the domain of the rhs is positive (and we know
    // that our denominator is positive) then we can propagate based on the assumption that all the
    // domains are positive
    if context.lower_bound(numerator) >= 0 && context.lower_bound(rhs) >= 0 {
        propagate_positive_domains(context, numerator, denominator, rhs)?;
    }

    // If the domain of the numerator is negative and the domain of the rhs is negative (and we know
    // that our denominator is positive) then we propagate based on the views over the numerator and
    // rhs
    if context.lower_bound(negated_numerator) >= 0 && context.lower_bound(negated_rhs) >= 0 {
        propagate_positive_domains(context, negated_numerator, denominator, negated_rhs)?;
    }

    Ok(())
}

/// Propagates the domains of variables if all the domains are positive (if the variables are
/// sign-fixed then we simply transform them to positive domains using [`AffineView`]s); it performs
/// the following propagations:
/// - The minimum value that division can take on is the smallest value that `numerator /
///   denominator` can take on
/// - The numerator is at least as large as the smallest value that `denominator * rhs` can take on
/// - The value of the denominator is smaller than the largest value that `numerator / rhs` can take
///   on
/// - The denominator is at least as large as the ratio between the largest ceiled ratio between
///   `numerator + 1` and `rhs + 1`
fn propagate_positive_domains<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    context: &mut PropagationContextMut,
    numerator: &VA,
    denominator: &VB,
    rhs: &VC,
) -> PropagationStatusCP {
    let rhs_min = context.lower_bound(rhs);
    let rhs_max = context.upper_bound(rhs);
    let numerator_min = context.lower_bound(numerator);
    let numerator_max = context.upper_bound(numerator);
    let denominator_min = context.lower_bound(denominator);
    let denominator_max = context.upper_bound(denominator);

    // The new minimum value of the rhs is the minimum value that the division can take on
    let new_min_rhs = numerator_min / denominator_max;
    if rhs_min < new_min_rhs {
        context.set_lower_bound(
            rhs,
            new_min_rhs,
            conjunction!(
                [numerator >= numerator_min]
                    & [denominator <= denominator_max]
                    & [denominator >= 1]
            ),
        )?;
    }

    // numerator / denominator >= rhs_min
    // numerator >= rhs_min * denominator
    // numerator >= rhs_min * denominator_min
    // Note that we use rhs_min rather than new_min_rhs, this appears to be a heuristic
    let new_min_numerator = denominator_min * rhs_min;
    if numerator_min < new_min_numerator {
        context.set_lower_bound(
            numerator,
            new_min_numerator,
            conjunction!([denominator >= denominator_min] & [rhs >= rhs_min]),
        )?;
    }

    // numerator / denominator >= rhs_min
    // numerator >= rhs_min * denominator
    // If rhs_min == 0 -> no propagations
    // Otherwise, denominator <= numerator / rhs_min & denominator <= numerator_max / rhs_min
    if rhs_min > 0 {
        let new_max_denominator = numerator_max / rhs_min;
        if denominator_max > new_max_denominator {
            context.set_upper_bound(
                denominator,
                new_max_denominator,
                conjunction!(
                    [numerator <= numerator_max]
                        & [numerator >= 0]
                        & [rhs >= rhs_min]
                        & [denominator >= 1]
                ),
            )?;
        }
    }

    let new_min_denominator = {
        // Called the CeilRatio in OR-tools
        let dividend = numerator_min + 1;
        let positive_divisor = rhs_max + 1;

        let result = dividend / positive_divisor;
        let adjust = result * positive_divisor < dividend;
        result + adjust as i32
    };

    if denominator_min < new_min_denominator {
        context.set_lower_bound(
            denominator,
            new_min_denominator,
            conjunction!(
                [numerator >= numerator_min] & [rhs <= rhs_max] & [rhs >= 0] & [denominator >= 1]
            ),
        )?;
    }

    Ok(())
}

/// Propagates the upper-bounds of the right-hand side and the numerator, it performs the following
/// propagations
/// - The maximum value of the right-hand side can only be as large as the largest value that
///   `numerator / denominator` can take on
/// - The maximum value of the numerator is smaller than `(ub(rhs) + 1) * denominator - 1`, note
///   that this might not be the most constrictive bound
fn propagate_upper_bounds<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    context: &mut PropagationContextMut,
    numerator: &VA,
    denominator: &VB,
    rhs: &VC,
) -> PropagationStatusCP {
    let rhs_max = context.upper_bound(rhs);
    let numerator_max = context.upper_bound(numerator);
    let denominator_min = context.lower_bound(denominator);
    let denominator_max = context.upper_bound(denominator);

    // The new maximum value of the rhs is the maximum value that the division can take on (note
    // that numerator_max is positive and denominator_min is also positive)
    let new_max_rhs = numerator_max / denominator_min;
    if rhs_max > new_max_rhs {
        context.set_upper_bound(
            rhs,
            new_max_rhs,
            conjunction!([numerator <= numerator_max] & [denominator >= denominator_min]),
        )?;
    }

    // numerator / denominator <= rhs.max
    // numerator < (rhs.max + 1) * denominator
    // numerator + 1 <= (rhs.max + 1) * denominator.max
    // numerator <= (rhs.max + 1) * denominator.max - 1
    // Note that we use rhs_max here rather than the new upper-bound, this appears to be a heuristic
    let new_max_numerator = (rhs_max + 1) * denominator_max - 1;
    if numerator_max > new_max_numerator {
        context.set_upper_bound(
            numerator,
            new_max_numerator,
            conjunction!([denominator <= denominator_max] & [denominator >= 1] & [rhs <= rhs_max]),
        )?;
    }

    Ok(())
}

/// Propagates the signs of the variables, more specifically, it performs the following propagations
/// (assuming that the denominator is always > 0):
/// - If the numerator is non-negative then the right-hand side must be non-negative as well
/// - If the right-hand side is positive then the numerator must be positive as well
/// - If the numerator is non-positive then the right-hand side must be non-positive as well
/// - If the right-hand is negative then the numerator must be negative as well
fn propagate_signs<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    context: &mut PropagationContextMut,
    numerator: &VA,
    denominator: &VB,
    rhs: &VC,
) -> PropagationStatusCP {
    let rhs_min = context.lower_bound(rhs);
    let rhs_max = context.upper_bound(rhs);
    let numerator_min = context.lower_bound(numerator);
    let numerator_max = context.upper_bound(numerator);

    // First we propagate the signs
    // If the numerator >= 0 (and we know that denominator > 0) then the rhs must be >= 0
    if numerator_min >= 0 && rhs_min < 0 {
        context.set_lower_bound(rhs, 0, conjunction!([numerator >= 0] & [denominator >= 1]))?;
    }

    // If rhs > 0 (and we know that denominator > 0) then the numerator must be > 0
    if numerator_min <= 0 && rhs_min > 0 {
        context.set_lower_bound(numerator, 1, conjunction!([rhs >= 1] & [denominator >= 1]))?;
    }

    // If numerator <= 0 (and we know that denominator > 0) then the rhs must be <= 0
    if numerator_max <= 0 && rhs_max > 0 {
        context.set_upper_bound(rhs, 0, conjunction!([numerator <= 0] & [denominator >= 1]))?;
    }

    // If the rhs < 0 (and we know that denominator > 0) then the numerator must be < 0
    if numerator_max >= 0 && rhs_max < 0 {
        context.set_upper_bound(
            numerator,
            -1,
            conjunction!([rhs <= -1] & [denominator >= 1]),
        )?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::test_helper::TestSolver;

    #[test]
    fn detects_conflicts() {
        let mut solver = TestSolver::default();
        let numerator = solver.new_variable(1, 1);
        let denominator = solver.new_variable(2, 2);
        let rhs = solver.new_variable(2, 2);

        let propagator = solver.new_propagator(DivisionConstructor {
            numerator,
            denominator,
            rhs,
        });

        assert!(propagator.is_err());
    }
}
