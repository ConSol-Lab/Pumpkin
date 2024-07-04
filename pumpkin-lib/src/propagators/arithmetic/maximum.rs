use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
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
use crate::predicate;

/// Bounds-consistent propagator which enforces `max(array) = rhs`.
#[derive(Debug)]
pub(crate) struct MaximumConstructor<ElementVar, Rhs> {
    pub(crate) array: Box<[ElementVar]>,
    pub(crate) rhs: Rhs,
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> PropagatorConstructor
    for MaximumConstructor<ElementVar, Rhs>
{
    type Propagator = MaximumPropagator<ElementVar, Rhs>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let array = self
            .array
            .iter()
            .cloned()
            .enumerate()
            .map(|(idx, var)| {
                context.register(var, DomainEvents::BOUNDS, LocalId::from(idx as u32))
            })
            .collect::<Box<_>>();

        let rhs = context.register(
            self.rhs,
            DomainEvents::UPPER_BOUND,
            LocalId::from(array.len() as u32),
        );

        MaximumPropagator { array, rhs }
    }
}

/// Bounds-consistent propagator which enforces `max(array) = rhs`. Can be constructed through
/// [`MaximumConstructor`].
#[derive(Debug)]
pub(crate) struct MaximumPropagator<ElementVar, Rhs> {
    array: Box<[PropagatorVariable<ElementVar>]>,
    rhs: PropagatorVariable<Rhs>,
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> Propagator
    for MaximumPropagator<ElementVar, Rhs>
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.debug_propagate_from_scratch(context)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "Maximum"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        let rhs_ub = context.upper_bound(&self.rhs);
        let mut max_ub = i32::MIN;
        let mut max_lb = i32::MIN;
        let mut lb_reason = vec![];
        let mut ub_reason = vec![];

        for var in self.array.iter() {
            context.set_upper_bound(var, rhs_ub, conjunction!([self.rhs <= rhs_ub]))?;
            lb_reason.push(predicate![var >= context.lower_bound(var)]);
            ub_reason.push(predicate![var <= context.upper_bound(var)]);

            max_ub = i32::max(context.upper_bound(var), max_ub);
            max_lb = i32::max(context.lower_bound(var), max_lb);
        }

        context.set_upper_bound(&self.rhs, max_ub, PropositionalConjunction::new(ub_reason))?;
        context.set_lower_bound(&self.rhs, max_lb, PropositionalConjunction::new(lb_reason))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::test_helper::TestSolver;

    #[test]
    fn upper_bound_of_rhs_matches_maximum_upper_bound_of_array_at_initialise() {
        let mut solver = TestSolver::default();

        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(1, 4);
        let c = solver.new_variable(1, 5);

        let rhs = solver.new_variable(1, 10);

        let _ = solver
            .new_propagator(MaximumConstructor {
                array: [a, b, c].into(),
                rhs,
            })
            .expect("no empty domain");

        solver.assert_bounds(rhs, 1, 5);

        let reason = solver.get_reason_int(predicate![rhs <= 5].try_into().unwrap());
        assert_eq!(conjunction!([a <= 3] & [b <= 4] & [c <= 5]), reason.clone());
    }

    #[test]
    fn lower_bound_of_rhs_is_maximum_of_lower_bounds_in_array() {
        let mut solver = TestSolver::default();

        let a = solver.new_variable(3, 10);
        let b = solver.new_variable(4, 10);
        let c = solver.new_variable(5, 10);

        let rhs = solver.new_variable(1, 10);

        let _ = solver
            .new_propagator(MaximumConstructor {
                array: [a, b, c].into(),
                rhs,
            })
            .expect("no empty domain");

        solver.assert_bounds(rhs, 5, 10);

        let reason = solver.get_reason_int(predicate![rhs >= 5].try_into().unwrap());
        assert_eq!(conjunction!([a >= 3] & [b >= 4] & [c >= 5]), reason.clone());
    }

    #[test]
    fn upper_bound_of_all_array_elements_at_most_rhs_max_at_initialise() {
        let mut solver = TestSolver::default();

        let array = (1..=5)
            .map(|idx| solver.new_variable(1, 4 + idx))
            .collect::<Box<_>>();

        let rhs = solver.new_variable(1, 3);

        let _ = solver
            .new_propagator(MaximumConstructor {
                array: array.clone(),
                rhs,
            })
            .expect("no empty domain");

        for var in array.iter() {
            solver.assert_bounds(*var, 1, 3);
            let reason = solver.get_reason_int(predicate![var <= 3].try_into().unwrap());
            assert_eq!(conjunction!([rhs <= 3]), reason.clone());
        }
    }
}
