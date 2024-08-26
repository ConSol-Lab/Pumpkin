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

#[derive(Clone, Debug)]
pub(crate) struct LinearLessOrEqualConstructor<Var> {
    pub(crate) x: Box<[Var]>,
    pub(crate) c: i32,
}

impl<Var: IntegerVariable + 'static> LinearLessOrEqualConstructor<Var> {
    pub(crate) fn new(x: Box<[Var]>, c: i32) -> Self {
        LinearLessOrEqualConstructor { x, c }
    }
}

/// Propagator for the constraint `reif => \sum x_i <= c`.
#[derive(Debug)]
pub(crate) struct LinearLessOrEqualPropagator<Var> {
    x: Box<[Var]>,
    c: i32,
}

impl<Var> PropagatorConstructor for LinearLessOrEqualConstructor<Var>
where
    Var: IntegerVariable,
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
        LinearLessOrEqualPropagator::<Var> { x, c: self.c }
    }
}

impl<Var> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn debug_propagate_from_scratch(&self, context: PropagationContextMut) -> PropagationStatusCP {
        perform_propagation(context, &self.x, self.c)
    }
}

fn perform_propagation<Var: IntegerVariable>(
    mut context: PropagationContextMut,
    x: &[Var],
    c: i32,
) -> PropagationStatusCP {
    let lb_lhs = x.iter().map(|var| context.lower_bound(var)).sum::<i32>();
    if c < lb_lhs {
        let reason: PropositionalConjunction = x
            .iter()
            .map(|var| predicate![var >= context.lower_bound(var)])
            .collect();
        return Err(reason.into());
    }

    for (i, x_i) in x.iter().enumerate() {
        let bound = c - (lb_lhs - context.lower_bound(x_i));

        if context.upper_bound(x_i) > bound {
            let reason: PropositionalConjunction = x
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

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(LinearLessOrEqualConstructor::new([x, y].into(), 7))
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
            .new_propagator(LinearLessOrEqualConstructor::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y <= 6].try_into().unwrap());

        assert_eq!(conjunction!([x >= 1]), *reason);
    }
}
