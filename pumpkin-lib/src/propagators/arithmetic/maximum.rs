use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
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
            DomainEvents::BOUNDS,
            LocalId::from(array.len() as u32),
        );

        MaximumPropagator { array, rhs }
    }
}

/// Bounds-consistent propagator which enforces `max(array) = rhs`. Can be constructed through
/// [`MaximumConstructor`].
#[derive(Debug)]
pub(crate) struct MaximumPropagator<ElementVar, Rhs> {
    array: Box<[ElementVar]>,
    rhs: Rhs,
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> Propagator
    for MaximumPropagator<ElementVar, Rhs>
{
    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "Maximum"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        // This is the constraint that is being propagated:
        // max(a_0, a_1, ..., a_{n-1}) = rhs

        let rhs_ub = context.upper_bound(&self.rhs);
        let mut max_ub = context.upper_bound(&self.array[0]);
        let mut max_lb = context.lower_bound(&self.array[0]);
        let mut lb_reason = predicate![self.array[0] >= max_lb];
        for var in self.array.iter() {
            // Rule 1.
            // UB(a_i) <= UB(rhs)
            context.set_upper_bound(var, rhs_ub, conjunction!([self.rhs <= rhs_ub]))?;

            let var_lb = context.lower_bound(var);
            let var_ub = context.upper_bound(var);

            if var_lb > max_lb {
                max_lb = var_lb;
                lb_reason = predicate![var >= var_lb];
            }

            if var_ub > max_ub {
                max_ub = var_ub;
            }
        }
        // Rule 2.
        // LB(rhs) >= max{LB(a_i)}.
        context.set_lower_bound(&self.rhs, max_lb, PropositionalConjunction::from(lb_reason))?;

        // Rule 3.
        // UB(rhs) <= max{UB(a_i)}.
        // Note that this implicitly also covers the rule:
        // 'if LB(rhs) > UB(a_i) for all i, then conflict'.
        if rhs_ub > max_ub {
            let ub_reason: PropositionalConjunction = self
                .array
                .iter()
                .map(|var| predicate![var <= max_ub])
                .collect();
            context.set_upper_bound(&self.rhs, max_ub, ub_reason)?;
        }

        // Rule 4.
        // If there is only one variable with UB(a_i) >= LB(rhs),
        // then the bounds for rhs and that variable should be intersected.
        let rhs_lb = context.lower_bound(&self.rhs);
        let mut propagating_variable: Option<&ElementVar> = None;
        let mut propagation_reason = PropositionalConjunction::default();
        for var in self.array.iter() {
            if context.upper_bound(var) >= rhs_lb {
                if propagating_variable.is_none() {
                    propagating_variable = Some(var);
                } else {
                    propagating_variable = None;
                    break;
                }
            } else {
                propagation_reason.add(predicate![var <= rhs_lb - 1]);
            }
        }
        // If there is exactly one variable UB(a_i) >= LB(rhs), then the propagating variable is
        // Some. In that case, intersect the bounds of that variable and the rhs. Given previous
        // rules, only the lower bound of the propagated variable needs to be propagated.
        if let Some(propagating_variable) = propagating_variable {
            let var_lb = context.lower_bound(propagating_variable);
            if var_lb < rhs_lb {
                propagation_reason.add(predicate![self.rhs >= rhs_lb]);
                context.set_lower_bound(propagating_variable, rhs_lb, propagation_reason)?;
            }
        }

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
        assert_eq!(conjunction!([a <= 5] & [b <= 5] & [c <= 5]), reason.clone());
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
        assert_eq!(conjunction!([c >= 5]), reason.clone());
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

    #[test]
    fn single_variable_propagate() {
        let mut solver = TestSolver::default();

        let array = (1..=5)
            .map(|idx| solver.new_variable(1, 1 + 10 * idx))
            .collect::<Box<_>>();

        let rhs = solver.new_variable(45, 60);

        let _ = solver
            .new_propagator(MaximumConstructor {
                array: array.clone(),
                rhs,
            })
            .expect("no empty domain");

        solver.assert_bounds(*array.last().unwrap(), 45, 51);
        solver.assert_bounds(rhs, 45, 51);
    }
}
