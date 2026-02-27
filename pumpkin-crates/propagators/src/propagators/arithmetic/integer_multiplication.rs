use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::asserts::pumpkin_assert_simple;
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
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

declare_inference_label!(IntegerMultiplication);

/// The [`PropagatorConstructor`] for [`IntegerMultiplicationPropagator`].
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationArgs<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB, VC> PropagatorConstructor for IntegerMultiplicationArgs<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type PropagatorImpl = IntegerMultiplicationPropagator<VA, VB, VC>;

    fn add_inference_checkers(
        &self,
        mut checkers: InferenceCheckers<'_>,
    ) -> impl ConsistencyChecker + 'static {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, IntegerMultiplication),
            Box::new(IntegerMultiplicationChecker {
                a: self.a.clone(),
                b: self.b.clone(),
                c: self.c.clone(),
            }),
        );

        #[allow(deprecated, reason = "TODO to implement for reified")]
        pumpkin_core::propagation::checkers::DefaultChecker
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        } = self;

        context.register(a.clone(), DomainEvents::ANY_INT, ID_A);
        context.register(b.clone(), DomainEvents::ANY_INT, ID_B);
        context.register(c.clone(), DomainEvents::ANY_INT, ID_C);

        IntegerMultiplicationPropagator {
            a,
            b,
            c,
            inference_code: InferenceCode::new(constraint_tag, IntegerMultiplication),
        }
    }
}

/// A propagator for maintaining the constraint `a * b = c`. The propagator
/// (currently) only propagates the signs of the variables, the case where a, b, c >= 0, and detects
/// a conflict if the variables are fixed.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationPropagator<VA, VB, VC> {
    a: VA,
    b: VB,
    c: VC,
    inference_code: InferenceCode,
}

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

impl<VA: 'static, VB: 'static, VC: 'static> Propagator
    for IntegerMultiplicationPropagator<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "IntTimes"
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        perform_propagation(context, &self.a, &self.b, &self.c, &self.inference_code)
    }
}

fn perform_propagation<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    mut context: PropagationContext,
    a: &VA,
    b: &VB,
    c: &VC,
    inference_code: &InferenceCode,
) -> PropagationStatusCP {
    // First we propagate the signs
    propagate_signs(&mut context, a, b, c, inference_code)?;

    let a_min = context.lower_bound(a);
    let a_max = context.upper_bound(a);
    let b_min = context.lower_bound(b);
    let b_max = context.upper_bound(b);
    let c_min = context.lower_bound(c);
    let c_max = context.upper_bound(c);

    if a_min >= 0 && b_min >= 0 {
        let new_max_c = a_max.saturating_mul(b_max);
        let new_min_c = a_min.saturating_mul(b_min);

        // c is smaller than the maximum value that a * b can take
        //
        // We need the lower-bounds in the explanation as well because the reasoning does not
        // hold in the case of a negative lower-bound
        context.post(
            predicate![c <= new_max_c],
            conjunction!([a >= 0] & [a <= a_max] & [b >= 0] & [b <= b_max]),
            inference_code,
        )?;

        // c is larger than the minimum value that a * b can take
        context.post(
            predicate![c >= new_min_c],
            conjunction!([a >= a_min] & [b >= b_min]),
            inference_code,
        )?;
    }

    if b_min >= 0 && b_max >= 1 && c_min >= 1 {
        // a >= ceil(c.min / b.max)
        let bound = div_ceil_pos(c_min, b_max);
        context.post(
            predicate![a >= bound],
            conjunction!([c >= c_min] & [b >= 0] & [b <= b_max]),
            inference_code,
        )?;
    }

    if b_min >= 1 && c_min >= 0 && c_max >= 1 {
        // a <= floor(c.max / b.min)
        let bound = c_max / b_min;
        context.post(
            predicate![a <= bound],
            conjunction!([c >= 0] & [c <= c_max] & [b >= b_min]),
            inference_code,
        )?;
    }

    if a_min >= 1 && c_min >= 0 && c_max >= 1 {
        // b <= floor(c.max / a.min)
        let bound = c_max / a_min;
        context.post(
            predicate![b <= bound],
            conjunction!([c >= 0] & [c <= c_max] & [a >= a_min]),
            inference_code,
        )?;
    }

    // b >= ceil(c.min / a.max)
    if a_min >= 0 && a_max >= 1 && c_min >= 1 {
        let bound = div_ceil_pos(c_min, a_max);

        context.post(
            predicate![b >= bound],
            conjunction!([c >= c_min] & [a >= 0] & [a <= a_max]),
            inference_code,
        )?;
    }

    if context.is_fixed(a)
        && context.is_fixed(b)
        && context.is_fixed(c)
        && (context.lower_bound(a) * context.lower_bound(b)) != context.lower_bound(c)
    {
        // All variables are assigned but the resulting value is not correct, so we report a
        // conflict
        return Err(PropagatorConflict {
            conjunction: conjunction!(
                [a == context.lower_bound(a)]
                    & [b == context.lower_bound(b)]
                    & [c == context.lower_bound(c)]
            ),
            inference_code: inference_code.clone(),
        }
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
    context: &mut PropagationContext,
    a: &VA,
    b: &VB,
    c: &VC,
    inference_code: &InferenceCode,
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
        context.post(
            predicate![c >= 0],
            conjunction!([a >= 0] & [b >= 0]),
            inference_code,
        )?;
    }

    // a is positive and c is positive -> b is positive
    if a_min >= 1 && c_min >= 1 {
        context.post(
            predicate![b >= 1],
            conjunction!([a >= 1] & [c >= 1]),
            inference_code,
        )?;
    }

    // b is positive and c is positive -> a is positive
    if b_min >= 1 && c_min >= 1 {
        context.post(
            predicate![a >= 1],
            conjunction!([b >= 1] & [c >= 1]),
            inference_code,
        )?;
    }

    // Propagating based on negative bounds
    // a is negative and b is negative -> c is positive
    if a_max <= 0 && b_max <= 0 {
        context.post(
            predicate![c >= 0],
            conjunction!([a <= 0] & [b <= 0]),
            inference_code,
        )?;
    }

    // a is negative and c is negative -> b is positive
    if a_max <= -1 && c_max <= -1 {
        context.post(
            predicate![b >= 1],
            conjunction!([a <= -1] & [c <= -1]),
            inference_code,
        )?;
    }

    // b is negative and c is negative -> a is positive
    if b_max <= -1 && c_max <= -1 {
        context.post(
            predicate![a >= 1],
            conjunction!([b <= -1] & [c <= -1]),
            inference_code,
        )?;
    }

    // Propagating based on mixed bounds (i.e. one positive and one negative)
    // Propagating c based on a and b
    // a is negative and b is positive -> c is negative
    if a_max <= 0 && b_min >= 0 {
        context.post(
            predicate![c <= 0],
            conjunction!([a <= 0] & [b >= 0]),
            inference_code,
        )?;
    }

    // a is positive and b is negative -> c is negative
    if a_min >= 0 && b_max <= 0 {
        context.post(
            predicate![c <= 0],
            conjunction!([a >= 0] & [b <= 0]),
            inference_code,
        )?;
    }

    // Propagating b based on a and c
    // a is negative and c is positive -> b is negative
    if a_max <= -1 && c_min >= 1 {
        context.post(
            predicate![b <= -1],
            conjunction!([a <= -1] & [c >= 1]),
            inference_code,
        )?;
    }

    // a is positive and c is negative -> b is negative
    if a_min >= 1 && c_max <= -1 {
        context.post(
            predicate![b <= -1],
            conjunction!([a >= 1] & [c <= -1]),
            inference_code,
        )?;
    }

    // Propagating a based on b and c
    // b is negative and c is positive -> a is negative
    if b_max <= -1 && c_min >= 1 {
        context.post(
            predicate![a <= -1],
            conjunction!([b <= -1] & [c >= 1]),
            inference_code,
        )?;
    }

    // b is positive and c is negative -> a is negative
    if b_min >= 1 && c_max <= -1 {
        context.post(
            predicate![a <= -1],
            conjunction!([b >= 1] & [c <= -1]),
            inference_code,
        )?;
    }

    Ok(())
}

/// Compute `ceil(numerator / denominator)`.
///
/// Assumes `numerator, denominator > 0`.
#[inline]
fn div_ceil_pos(numerator: i32, denominator: i32) -> i32 {
    pumpkin_assert_simple!(
        numerator > 0 && denominator > 0,
        "Either the numerator {numerator} was non-positive or the denominator {denominator} was non-positive"
    );
    numerator / denominator + (numerator % denominator).signum()
}

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
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // We apply interval arithmetic to determine that the computed interval `a times b`
        // does not intersect with the domain of `c`.
        //
        // See https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_operators.

        let x1 = self.a.induced_lower_bound(&state);
        let x2 = self.a.induced_upper_bound(&state);
        let y1 = self.b.induced_lower_bound(&state);
        let y2 = self.b.induced_upper_bound(&state);

        let c_lower = self.c.induced_lower_bound(&state);
        let c_upper = self.c.induced_upper_bound(&state);

        let x1y1 = x1 * y1;
        let x1y2 = x1 * y2;
        let x2y1 = x2 * y1;
        let x2y2 = x2 * y2;

        let computed_c_lower = x1y1.min(x1y2).min(x2y1).min(x2y2);
        let computed_c_upper = x1y1.max(x1y2).max(x2y1).max(x2y2);

        computed_c_upper < c_lower || computed_c_lower > c_upper
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_core::TestSolver;

    use super::*;

    #[test]
    fn bounds_of_a_and_b_propagate_bounds_c() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(0, 4);
        let c = solver.new_variable(-10, 20);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(0, solver.lower_bound(b));
        assert_eq!(4, solver.upper_bound(b));
        assert_eq!(0, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![c >= 0]);
        assert_eq!(conjunction!([a >= 0] & [b >= 0]), reason_lb);

        let reason_ub = solver.get_reason_int(predicate![c <= 12]);
        assert_eq!(
            conjunction!([a >= 0] & [a <= 3] & [b >= 0] & [b <= 4]),
            reason_ub
        );
    }

    #[test]
    fn bounds_of_a_and_c_propagate_bounds_b() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(2, 3);
        let b = solver.new_variable(0, 12);
        let c = solver.new_variable(2, 12);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("no empty domains");

        assert_eq!(2, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(1, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(2, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![b >= 1]);
        assert_eq!(conjunction!([a >= 1] & [c >= 1]), reason_lb);

        let reason_ub = solver.get_reason_int(predicate![b <= 6]);
        assert_eq!(conjunction!([a >= 2] & [c >= 0] & [c <= 12]), reason_ub);
    }

    #[test]
    fn bounds_of_b_and_c_propagate_bounds_a() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 10);
        let b = solver.new_variable(3, 6);
        let c = solver.new_variable(2, 12);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(4, solver.upper_bound(a));
        assert_eq!(3, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(3, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![a >= 1]);
        assert_eq!(conjunction!([b >= 1] & [c >= 1]), reason_lb);

        let reason_ub = solver.get_reason_int(predicate![a <= 4]);
        assert_eq!(conjunction!([b >= 3] & [c >= 0] & [c <= 12]), reason_ub);
    }

    #[test]
    fn b_unbounded_does_not_panic() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(12, 12);
        let b = solver.new_variable(i32::MIN, i32::MAX);
        let c = solver.new_variable(144, 144);

        let constraint_tag = solver.new_constraint_tag();
        let _ = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("No empty domains");
    }

    #[test]
    fn a_unbounded_does_not_panic() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(i32::MIN, i32::MAX);
        let b = solver.new_variable(12, 12);
        let c = solver.new_variable(144, 144);

        let constraint_tag = solver.new_constraint_tag();
        let _ = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("No empty domains");
    }

    #[test]
    fn c_unbounded_does_not_panic() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(12, 12);
        let b = solver.new_variable(12, 12);
        let c = solver.new_variable(i32::MIN, i32::MAX);

        let constraint_tag = solver.new_constraint_tag();
        let _ = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("No empty domains");
    }

    #[test]
    fn all_unbounded_does_not_panic() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(i32::MIN, i32::MAX);
        let b = solver.new_variable(i32::MIN, i32::MAX);
        let c = solver.new_variable(i32::MIN, i32::MAX);

        let constraint_tag = solver.new_constraint_tag();
        let _ = solver
            .new_propagator(IntegerMultiplicationArgs {
                a,
                b,
                c,
                constraint_tag,
            })
            .expect("No empty domains");
    }
}
