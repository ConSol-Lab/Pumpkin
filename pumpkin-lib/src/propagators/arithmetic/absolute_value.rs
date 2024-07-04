use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::PropagatorVariable;
use crate::engine::variables::IntegerVariable;

#[derive(Debug)]
pub struct AbsoluteValueConstructor<VA, VB> {
    /// The side of the equality where the sign matters.
    pub(crate) signed: VA,
    /// The absolute of `signed`.
    pub(crate) absolute: VB,
}

impl<VA: IntegerVariable, VB: IntegerVariable> PropagatorConstructor
    for AbsoluteValueConstructor<VA, VB>
{
    type Propagator = AbsoluteValuePropagator<VA, VB>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        let signed = context.register(self.signed, DomainEvents::BOUNDS, LocalId::from(0));
        let absolute = context.register(self.absolute, DomainEvents::BOUNDS, LocalId::from(1));

        AbsoluteValuePropagator { signed, absolute }
    }
}

/// Propagator for `absolute = |signed|`, where `absolute` and `signed` are integer variables.
///
/// The propagator is bounds consistent wrt signed. That means that if `signed \in {-2, -1, 1, 2}`,
/// the propagator will not propagate `[absolute >= 1]`.
#[derive(Debug)]
pub struct AbsoluteValuePropagator<VA, VB> {
    signed: PropagatorVariable<VA>,
    absolute: PropagatorVariable<VB>,
}

impl<VA: IntegerVariable, VB: IntegerVariable> Propagator for AbsoluteValuePropagator<VA, VB> {
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.debug_propagate_from_scratch(context)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "IntAbs"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        // The bound of absolute may be tightened further during propagation, but it is at least
        // zero at the root.
        context.set_lower_bound(&self.absolute, 0, conjunction!())?;
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
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

        context.set_upper_bound(
            &self.absolute,
            signed_absolute_ub,
            conjunction!([self.signed >= signed_lb] & [self.signed <= signed_ub]),
        )?;

        if signed_lb > 0 {
            context.set_lower_bound(
                &self.absolute,
                signed_lb,
                conjunction!([self.signed >= signed_lb]),
            )?;
        } else if signed_ub < 0 {
            context.set_lower_bound(
                &self.absolute,
                signed_ub.abs(),
                conjunction!([self.signed <= signed_ub]),
            )?;
        }

        let absolute_ub = context.upper_bound(&self.absolute);
        let absolute_lb = context.lower_bound(&self.absolute);
        context.set_lower_bound(
            &self.signed,
            -absolute_ub,
            conjunction!([self.absolute <= absolute_ub]),
        )?;
        context.set_upper_bound(
            &self.signed,
            absolute_ub,
            conjunction!([self.absolute <= absolute_ub]),
        )?;

        if signed_ub <= 0 {
            context.set_upper_bound(
                &self.signed,
                -absolute_lb,
                conjunction!([self.signed <= 0] & [self.absolute >= absolute_lb]),
            )?;
        } else if signed_lb >= 0 {
            context.set_lower_bound(
                &self.signed,
                absolute_lb,
                conjunction!([self.signed >= 0] & [self.absolute >= absolute_lb]),
            )?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::test_helper::TestSolver;

    #[test]
    fn absolute_bounds_are_propagated_at_initialise() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-3, 4);
        let absolute = solver.new_variable(-2, 10);

        let _ = solver
            .new_propagator(AbsoluteValueConstructor { signed, absolute })
            .expect("no empty domains");

        solver.assert_bounds(absolute, 0, 4);
    }

    #[test]
    fn signed_bounds_are_propagated_at_initialise() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-5, 5);
        let absolute = solver.new_variable(0, 3);

        let _ = solver
            .new_propagator(AbsoluteValueConstructor { signed, absolute })
            .expect("no empty domains");

        solver.assert_bounds(signed, -3, 3);
    }

    #[test]
    fn absolute_lower_bound_can_be_strictly_positive() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(3, 6);
        let absolute = solver.new_variable(0, 10);

        let _ = solver
            .new_propagator(AbsoluteValueConstructor { signed, absolute })
            .expect("no empty domains");

        solver.assert_bounds(absolute, 3, 6);
    }

    #[test]
    fn strictly_negative_signed_value_can_propagate_lower_bound_on_absolute() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-5, -3);
        let absolute = solver.new_variable(1, 5);

        let _ = solver
            .new_propagator(AbsoluteValueConstructor { signed, absolute })
            .expect("no empty domains");

        solver.assert_bounds(absolute, 3, 5);
    }

    #[test]
    fn lower_bound_on_absolute_can_propagate_negative_upper_bound_on_signed() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(-5, 0);
        let absolute = solver.new_variable(1, 5);

        let _ = solver
            .new_propagator(AbsoluteValueConstructor { signed, absolute })
            .expect("no empty domains");

        solver.assert_bounds(signed, -5, -1);
    }

    #[test]
    fn lower_bound_on_absolute_can_propagate_positive_lower_bound_on_signed() {
        let mut solver = TestSolver::default();

        let signed = solver.new_variable(1, 5);
        let absolute = solver.new_variable(3, 5);

        let _ = solver
            .new_propagator(AbsoluteValueConstructor { signed, absolute })
            .expect("no empty domains");

        solver.assert_bounds(signed, 3, 5);
    }
}
