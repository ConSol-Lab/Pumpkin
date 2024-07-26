use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::pumpkin_assert_simple;

/// A propagator for maintaining the constraint `a * b = c`. The propagator
/// (currently) only propagates the signs of the variables, the case where a, b, c >= 0, and detects
/// a conflict if the variables are fixed.
#[derive(Debug)]
pub(crate) struct IntegerMultiplicationPropagator<VA, VB, VC> {
    a: VA,
    b: VB,
    c: VC,
}

#[derive(Debug)]
pub(crate) struct IntegerMultiplicationConstructor<VA, VB, VC> {
    pub(crate) a: VA,
    pub(crate) b: VB,
    pub(crate) c: VC,
}

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

impl<VA, VB, VC> PropagatorConstructor for IntegerMultiplicationConstructor<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type Propagator = IntegerMultiplicationPropagator<VA, VB, VC>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        IntegerMultiplicationPropagator {
            a: context.register(self.a, DomainEvents::ANY_INT, ID_A),
            b: context.register(self.b, DomainEvents::ANY_INT, ID_B),
            c: context.register(self.c, DomainEvents::ANY_INT, ID_C),
        }
    }
}

impl<VA: 'static, VB: 'static, VC: 'static> Propagator
    for IntegerMultiplicationPropagator<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "IntTimes"
    }

    fn debug_propagate_from_scratch(&self, context: PropagationContextMut) -> PropagationStatusCP {
        perform_propagation(context, &self.a, &self.b, &self.c)
    }
}

fn perform_propagation<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    mut context: PropagationContextMut,
    a: &VA,
    b: &VB,
    c: &VC,
) -> PropagationStatusCP {
    // First we propagate the signs
    propagate_signs(&mut context, a, b, c)?;

    // Then we propagate on whether or not the variables are sign fixed
    // For now we only propagate in the case where a, b, c >= 0
    // TODO: We can likely perform much more propagation
    if context.lower_bound(a) >= 0 && context.lower_bound(b) >= 0 {
        let a_min = context.lower_bound(a);
        let a_max = context.upper_bound(a);
        let b_min = context.lower_bound(b);
        let b_max = context.upper_bound(b);
        let c_min = context.lower_bound(c);
        let c_max = context.upper_bound(c);

        let new_max_c = a_max * b_max;
        let new_min_c = a_min * b_min;

        // c is smaller than the maximum value that a * b can take
        if context.upper_bound(c) > new_max_c {
            // We need the lower-bounds in the explanation as well because the reasoning does not
            // hold in the case of a negative lower-bound
            context.set_upper_bound(
                c,
                new_max_c,
                conjunction!([a >= 0] & [a <= a_max] & [b >= 0] & [b <= b_max]),
            )?;
        }

        // c is larger than the minimum value that a * b can take
        if context.lower_bound(c) < new_min_c {
            context.set_lower_bound(c, new_min_c, conjunction!([a >= a_min] & [b >= b_min]))?;
        }

        if c_min >= 0 {
            // a >= ceil(c.min / b.max)
            if b_max >= 1 && c_min >= 1 {
                let bound = div_ceil_pos(c_min, b_max);
                if context.lower_bound(a) < bound {
                    context.set_lower_bound(
                        a,
                        bound,
                        conjunction!([c >= c_min] & [b >= 0] & [b <= b_max]),
                    )?;
                }
            }

            // a <= floor(c.max / b.min)
            if b_min >= 1 && c_max >= 1 {
                let bound = c_max / b_min;
                if context.upper_bound(a) > bound {
                    context.set_upper_bound(
                        a,
                        bound,
                        conjunction!([c >= 0] & [c <= c_max] & [b >= b_min]),
                    )?;
                }
            }

            // b >= ceil(c.min / a.max)
            if a_max >= 1 && c_min >= 1 {
                let bound = div_ceil_pos(c_min, a_max);

                if context.lower_bound(b) < bound {
                    context.set_lower_bound(
                        b,
                        bound,
                        conjunction!([c >= c_min] & [a >= 0] & [a <= a_max]),
                    )?;
                }
            }

            // b <= floor(c.max / a.min)
            if a_min >= 1 && c_max >= 1 {
                let bound = c_max / a_min;
                if context.upper_bound(b) > bound {
                    context.set_upper_bound(
                        b,
                        bound,
                        conjunction!([c >= 0] & [c <= c_max] & [a >= a_min]),
                    )?;
                }
            }
        }
    }

    if context.is_fixed(a)
        && context.is_fixed(b)
        && context.is_fixed(c)
        && (context.lower_bound(a) * context.lower_bound(b)) != context.lower_bound(c)
    {
        // All variables are assigned but the resulting value is not correct, so we report a
        // conflict
        return Err(conjunction!(
            [a == context.lower_bound(a)]
                & [b == context.lower_bound(b)]
                & [c == context.lower_bound(c)]
        )
        .into());
    }

    Ok(())
}

/// Propagates the signs of the variables, it performs the following propagations:
/// - Propagating based on positive bounds
///     - If a is positive and b is positive then c is positive
///     - If a is positive and c is positive then b is positive
///     - If b is positive and c is positive then a is positive
/// - Propagating based on negative bounds
///     - If a is negative and b is negative then c is positive
///     - If a is negative and c is negative then b is positive
///     - If b is negative and c is negative then b is positive
/// - Propagating based on mixed bounds
///     - Propagating c based on a and b
///         - If a is negative and b is positive then c is negative
///         - If a is positive and b is negative then c is negative
///     - Propagating b based on a and c
///         - If a is negative and c is positive then b is negative
///         - If a is positive and c is negative then b is negative
///     - Propagating a based on b and c
///         - If b is negative and c is positive then a is negative
///         - If b is positive and c is negative then a is negative
///
/// Note that this method does not propagate a value if 0 is in the domain as, for example, 0 * -3 =
/// 0 and 0 * 3 = 0 are both equally valid.
fn propagate_signs<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    context: &mut PropagationContextMut,
    a: &VA,
    b: &VB,
    c: &VC,
) -> PropagationStatusCP {
    let a_min = context.lower_bound(a);
    let a_max = context.upper_bound(a);
    let b_min = context.lower_bound(b);
    let b_max = context.upper_bound(b);
    let c_min = context.lower_bound(c);
    let c_max = context.upper_bound(c);

    // Propagating based on positive bounds
    // a is positive and b is positive -> c is positive
    if a_min >= 0 && b_min >= 0 {
        context.set_lower_bound(c, 0, conjunction!([a >= 0] & [b >= 0]))?;
    }

    // a is positive and c is positive -> b is positive
    if a_min >= 1 && c_min >= 1 {
        context.set_lower_bound(b, 1, conjunction!([a >= 1] & [c >= 1]))?;
    }

    // b is positive and c is positive -> a is positive
    if b_min >= 1 && c_min >= 1 {
        context.set_lower_bound(a, 1, conjunction!([b >= 1] & [c >= 1]))?;
    }

    // Propagating based on negative bounds
    // a is negative and b is negative -> c is positive
    if a_max <= 0 && b_max <= 0 {
        context.set_lower_bound(c, 0, conjunction!([a <= 0] & [b <= 0]))?;
    }

    // a is negative and c is negative -> b is positive
    if a_max <= -1 && c_max <= -1 {
        context.set_lower_bound(b, 1, conjunction!([a <= -1] & [c <= -1]))?;
    }

    // b is negative and c is negative -> a is positive
    if b_max <= -1 && c_max <= -1 {
        context.set_lower_bound(a, 1, conjunction!([b <= -1] & [c <= -1]))?;
    }

    // Propagating based on mixed bounds (i.e. one positive and one negative)
    // Propagating c based on a and b
    // a is negative and b is positive -> c is negative
    if a_max <= 0 && b_min >= 0 {
        context.set_upper_bound(c, 0, conjunction!([a <= 0] & [b >= 0]))?;
    }

    // a is positive and b is negative -> c is negative
    if a_min >= 0 && b_max <= 0 {
        context.set_upper_bound(c, 0, conjunction!([a >= 0] & [b <= 0]))?;
    }

    // Propagating b based on a and c
    // a is negative and c is positive -> b is negative
    if a_max <= -1 && c_min >= 1 {
        context.set_upper_bound(b, -1, conjunction!([a <= -1] & [c >= 1]))?;
    }

    // a is positive and c is negative -> b is negative
    if a_min >= 1 && c_max <= -1 {
        context.set_upper_bound(b, -1, conjunction!([a >= 1] & [c <= -1]))?;
    }

    // Propagating a based on b and c
    // b is negative and c is positive -> a is negative
    if b_max <= -1 && c_min >= 1 {
        context.set_upper_bound(a, -1, conjunction!([b <= -1] & [c >= 1]))?;
    }

    // b is positive and c is negative -> a is negative
    if b_min >= 1 && c_max <= -1 {
        context.set_upper_bound(a, -1, conjunction!([b >= 1] & [c <= -1]))?;
    }

    Ok(())
}

/// Compute `ceil(numerator / denominator)`.
///
/// Assumes `numerator, denominator > 0`.
#[inline]
fn div_ceil_pos(numerator: i32, denominator: i32) -> i32 {
    pumpkin_assert_simple!(numerator > 0 && denominator > 0, "Either the numerator {numerator} was non-positive or the denominator {denominator} was non-positive");
    numerator / denominator + (numerator % denominator).signum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;

    #[test]
    fn bounds_of_a_and_b_propagate_bounds_c() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(0, 4);
        let c = solver.new_variable(-10, 20);

        let mut propagator = solver
            .new_propagator(IntegerMultiplicationConstructor { a, b, c })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(0, solver.lower_bound(b));
        assert_eq!(4, solver.upper_bound(b));
        assert_eq!(0, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![c >= 0]);
        assert_eq!(conjunction!([a >= 0] & [b >= 0]), *reason_lb);

        let reason_ub = solver.get_reason_int(predicate![c <= 12]);
        assert_eq!(
            conjunction!([a >= 0] & [a <= 3] & [b >= 0] & [b <= 4]),
            *reason_ub
        );
    }

    #[test]
    fn bounds_of_a_and_c_propagate_bounds_b() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(2, 3);
        let b = solver.new_variable(0, 12);
        let c = solver.new_variable(2, 12);

        let mut propagator = solver
            .new_propagator(IntegerMultiplicationConstructor { a, b, c })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(2, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(1, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(2, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![b >= 1]);
        assert_eq!(conjunction!([a >= 1] & [c >= 1]), *reason_lb);

        let reason_ub = solver.get_reason_int(predicate![b <= 6]);
        assert_eq!(conjunction!([a >= 2] & [c >= 0] & [c <= 12]), *reason_ub);
    }

    #[test]
    fn bounds_of_b_and_c_propagate_bounds_a() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 10);
        let b = solver.new_variable(3, 6);
        let c = solver.new_variable(2, 12);

        let mut propagator = solver
            .new_propagator(IntegerMultiplicationConstructor { a, b, c })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(4, solver.upper_bound(a));
        assert_eq!(3, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(3, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![a >= 1]);
        assert_eq!(conjunction!([b >= 1] & [c >= 1]), *reason_lb);

        let reason_ub = solver.get_reason_int(predicate![a <= 4]);
        assert_eq!(conjunction!([b >= 3] & [c >= 0] & [c <= 12]), *reason_ub);
    }
}
