use log::warn;

use crate::basic_types::variables::IntVar;
use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::DomainEvents;
use crate::engine::LocalId;
use crate::engine::PropagationContext;
use crate::engine::PropagationContextMut;
use crate::engine::PropagatorConstructorContext;
use crate::engine::PropagatorVariable;
use crate::engine::ReadDomains;

/// A bounds-consistent propagator for maintaining the constraint `a * b = c`. The propagator
/// assumes `a, b, c >= 0`.
pub struct IntTimesProp<VA, VB, VC> {
    a: PropagatorVariable<VA>,
    b: PropagatorVariable<VB>,
    c: PropagatorVariable<VC>,
}

pub struct IntTimes<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

impl<VA, VB, VC> CPPropagatorConstructor for IntTimes<VA, VB, VC>
where
    VA: IntVar,
    VB: IntVar,
    VC: IntVar,
{
    type Propagator = IntTimesProp<VA, VB, VC>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        IntTimesProp {
            a: context.register(self.a, DomainEvents::ANY_INT, ID_A),
            b: context.register(self.b, DomainEvents::ANY_INT, ID_B),
            c: context.register(self.c, DomainEvents::ANY_INT, ID_C),
        }
    }
}

impl<VA, VB, VC> ConstraintProgrammingPropagator for IntTimesProp<VA, VB, VC>
where
    VA: IntVar,
    VB: IntVar,
    VC: IntVar,
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        perform_propagation(context, &self.a, &self.b, &self.c)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "IntTimes"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        if context.lower_bound(&self.a).is_negative() || context.lower_bound(&self.b).is_negative()
        {
            warn!("IntTimes does not support variables with negative bounds, but it is instantiated with them. The behavior will likely not be correct.");
        }

        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        perform_propagation(context, &self.a, &self.b, &self.c)
    }
}

fn perform_propagation<VA: IntVar, VB: IntVar, VC: IntVar>(
    context: &mut PropagationContextMut,
    a: &PropagatorVariable<VA>,
    b: &PropagatorVariable<VB>,
    c: &PropagatorVariable<VC>,
) -> PropagationStatusCP {
    let a_min = context.lower_bound(a);
    let a_max = context.upper_bound(a);
    let b_min = context.lower_bound(b);
    let b_max = context.upper_bound(b);
    let c_min = context.lower_bound(c);
    let c_max = context.upper_bound(c);

    // TODO: Remove these assertions? Or do we handle these cases with views to simplify this
    // implementation.
    assert!(
        a_min >= 0,
        "The IntTimes propagator assumes a to be non-negative."
    );
    assert!(
        b_min >= 0,
        "The IntTimes propagator assumes b to be non-negative."
    );

    let new_max_c = a_max * b_max;
    let new_min_c = a_min * b_min;

    if context.upper_bound(c) > new_max_c {
        context.set_upper_bound(c, new_max_c, conjunction!([a <= a_max] & [b <= b_max]))?;
    }

    if context.lower_bound(c) < new_min_c {
        context.set_lower_bound(c, new_min_c, conjunction!([a >= a_min] & [b >= b_min]))?;
    }

    // a >= ceil(c.min / b.max)
    if b_max >= 1 {
        let bound = div_ceil_pos(c_min, b_max);
        if context.lower_bound(a) < bound {
            context.set_lower_bound(a, bound, conjunction!([c >= c_min] & [b <= b_max]))?;
        }
    }

    // a <= floor(c.max / b.min)
    if b_min >= 1 {
        let bound = c_max / b_min;
        if context.upper_bound(a) > bound {
            context.set_upper_bound(a, bound, conjunction!([c <= c_max] & [b >= b_min]))?;
        }
    }

    // b >= ceil(c.min / a.max)
    if a_max >= 1 {
        let bound = div_ceil_pos(c_min, a_max);

        if context.lower_bound(b) < bound {
            context.set_lower_bound(b, bound, conjunction!([c >= c_min] & [a <= a_max]))?;
        }
    }

    // b <= floor(c.max / a.min)
    if a_min >= 1 {
        let bound = c_max / a_min;
        if context.upper_bound(b) > bound {
            context.set_upper_bound(b, bound, conjunction!([c <= c_max] & [a >= a_min]))?;
        }
    }
    Ok(())
}

/// Compute `ceil(numerator / denominator)`.
///
/// Assumes `numerator, denominator > 0`.
#[inline]
fn div_ceil_pos(numerator: i32, denominator: i32) -> i32 {
    (numerator + denominator - 1) / denominator
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::predicate;

    #[test]
    fn bounds_of_a_and_b_propagate_bounds_c() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(0, 4);
        let c = solver.new_variable(-10, 20);

        let mut propagator = solver
            .new_propagator(IntTimes { a, b, c })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(0, solver.lower_bound(b));
        assert_eq!(4, solver.upper_bound(b));
        assert_eq!(0, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![c >= 0]);
        assert_eq!(conjunction!([a >= 1] & [b >= 0]), *reason_lb);

        let reason_ub = solver.get_reason_int(predicate![c <= 12]);
        assert_eq!(conjunction!([a <= 3] & [b <= 4]), *reason_ub);
    }

    #[test]
    fn bounds_of_a_and_c_propagate_bounds_b() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(2, 3);
        let b = solver.new_variable(0, 12);
        let c = solver.new_variable(2, 12);

        let mut propagator = solver
            .new_propagator(IntTimes { a, b, c })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(2, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(1, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(2, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![b >= 1]);
        assert_eq!(conjunction!([a <= 3] & [c >= 2]), *reason_lb);

        let reason_ub = solver.get_reason_int(predicate![b <= 6]);
        assert_eq!(conjunction!([a >= 2] & [c <= 12]), *reason_ub);
    }

    #[test]
    fn bounds_of_b_and_c_propagate_bounds_a() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 10);
        let b = solver.new_variable(3, 6);
        let c = solver.new_variable(2, 12);

        let mut propagator = solver
            .new_propagator(IntTimes { a, b, c })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(4, solver.upper_bound(a));
        assert_eq!(3, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(3, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason_int(predicate![a >= 1]);
        assert_eq!(conjunction!([b <= 6] & [c >= 2]), *reason_lb);

        let reason_ub = solver.get_reason_int(predicate![a <= 4]);
        assert_eq!(conjunction!([b >= 3] & [c <= 12]), *reason_ub);
    }
}
