use super::linear_less_or_equal_constructor::LinearLessOrEqualConstructor;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::predicate;

/// Propagator for the constraint `reif => \sum x_i <= rhs`.
///
/// This propagator is non-incremental and reasons about the upper-bounds of tasks based on the
/// slack which is defined as follows:
///
/// `Slack(x_i) = ub(x_i) - (rhs - (lb_lhs - lb(x_i)))`
/// where `lb_lhs` is the lower-bound of the left-hand side.
///
/// If the slack is *positive* then it means that `ub(x_i) > rhs - lb_lhs - lb(x_i)` which means
/// that a bound update can take place (since the upper-bound is larger than its maximum
/// allowed value).
///
/// If the slack is *non-positive* then it means that `ub(x_i) <= rhs -
/// lb_lhs - lb(x_i)` which means that no propagation can take place since the upper-bound of
/// `x_i` is in its allowed range.
#[derive(Debug)]
pub(crate) struct LinearLessOrEqualPropagator<Var> {
    /// The terms in the linear inequality (also called the left-hand side), often represented by
    /// `x_i`.
    terms: Box<[Var]>,
    /// The right-hand side of the linear inequality, a constant term which does not change
    /// throughout the search.
    rhs: i32,
}

impl<Var> PropagatorConstructor
    for LinearLessOrEqualConstructor<Var, LinearLessOrEqualPropagator<Var>>
where
    Var: IntegerVariable + 'static,
{
    type Propagator = LinearLessOrEqualPropagator<Var>;

    fn create(self, context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        let x: Box<[_]> = self
            .x
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(
                    x_i.clone(),
                    DomainEvents::LOWER_BOUND,
                    LocalId::from(i as u32),
                )
            })
            .collect();

        LinearLessOrEqualPropagator::<Var> {
            terms: x,
            rhs: self.c,
        }
    }
}

impl<Var> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable + 'static,
{
    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        propagate_linear_less_or_equal_from_scratch(&self.terms, &mut context, self.rhs)
    }
}

pub(crate) fn propagate_linear_less_or_equal_from_scratch<Var: IntegerVariable + 'static>(
    terms: &[Var],
    context: &mut PropagationContextMut,
    rhs: i32,
) -> PropagationStatusCP {
    let lb_lhs = terms
        .iter()
        .map(|var| context.lower_bound(var))
        .sum::<i32>();
    if rhs < lb_lhs {
        let reason: PropositionalConjunction = terms
            .iter()
            .map(|var| predicate![var >= context.lower_bound(var)])
            .collect();
        return Err(reason.into());
    }

    for (i, x_i) in terms.iter().enumerate() {
        let bound = rhs - (lb_lhs - context.lower_bound(x_i));

        if context.upper_bound(x_i) > bound {
            let reason: PropositionalConjunction = terms
                .iter()
                .enumerate()
                .filter_map(|(j, x_j)| {
                    if j != i {
                        Some(predicate![x_j >= context.lower_bound(x_j)])
                    } else {
                        None
                    }
                })
                .collect();

            context.set_upper_bound(x_i, bound, reason)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::propagators::linear_less_or_equal::linear_less_or_equal_constructor::LinearLessOrEqual;

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(LinearLessOrEqual::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_explanations() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(LinearLessOrEqual::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y <= 6].try_into().unwrap());

        assert_eq!(conjunction!([x >= 1]), *reason);
    }
}
