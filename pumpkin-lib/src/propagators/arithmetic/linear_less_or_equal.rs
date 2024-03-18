use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropositionalConjunction;
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
use crate::engine::variables::Literal;
use crate::predicate;

#[derive(Debug)]
pub struct LinearLessOrEqualConstructor<Var> {
    pub x: Box<[Var]>,
    pub c: i32,
    pub reif: Option<Literal>,
}

impl<Var: IntegerVariable + 'static> LinearLessOrEqualConstructor<Var> {
    pub fn new(x: Box<[Var]>, c: i32) -> Self {
        LinearLessOrEqualConstructor { x, c, reif: None }
    }

    pub fn reified(x: Box<[Var]>, c: i32, reif: Literal) -> Self {
        LinearLessOrEqualConstructor {
            x,
            c,
            reif: Some(reif),
        }
    }
}

/// Propagator for the constraint `reif => \sum x_i <= c`.
#[derive(Debug)]
pub struct LinearLessOrEqualPropagator<Var> {
    x: Box<[PropagatorVariable<Var>]>,
    c: i32,
    pub reif: Option<PropagatorVariable<Literal>>,
}

impl<Var> PropagatorConstructor for LinearLessOrEqualConstructor<Var>
where
    Var: IntegerVariable,
{
    type Propagator = LinearLessOrEqualPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
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
        let reif = self.reif.map(|literal| {
            context.register_literal(
                literal,
                DomainEvents::ANY_BOOL,
                LocalId::from(self.x.len() as u32),
            )
        });
        LinearLessOrEqualPropagator::<Var> { x, c: self.c, reif }
    }
}

impl<Var> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        perform_propagation(context, &self.x, self.c, &self.reif)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        perform_propagation(context, &self.x, self.c, &self.reif)
    }
}

fn perform_propagation<Var: IntegerVariable>(
    context: &mut PropagationContextMut,
    x: &[PropagatorVariable<Var>],
    c: i32,
    reif: &Option<PropagatorVariable<Literal>>,
) -> PropagationStatusCP {
    let lb_lhs = x.iter().map(|var| context.lower_bound(var)).sum::<i32>();
    let reified = reif.is_some();
    if c < lb_lhs {
        if reified && !context.is_literal_fixed(reif.as_ref().unwrap()) {
            let reason: PropositionalConjunction = x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect();
            context.assign_literal(reif.as_ref().unwrap(), false, reason)?;
        } else if !reified || context.is_literal_true(reif.as_ref().unwrap()) {
            let reason: Vec<_> = x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect();
            let conjunction = if reified {
                let x1 = reif.as_ref().unwrap().get_literal();
                PropositionalConjunction::new(reason.into_boxed_slice(), Box::new([x1]))
            } else {
                reason.into()
            };
            return Err(conjunction.into());
        }
    }

    if reified
        && (!context.is_literal_fixed(reif.as_ref().unwrap())
            || context.is_literal_false(reif.as_ref().unwrap()))
    {
        return Ok(());
    }

    for (i, x_i) in x.iter().enumerate() {
        let bound = c - (lb_lhs - context.lower_bound(x_i));

        if context.upper_bound(x_i) > bound {
            let cp_reason: Vec<_> = x
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
            let reason = if reified {
                PropositionalConjunction::new(
                    cp_reason.into_boxed_slice(),
                    Box::new([reif.as_ref().unwrap().get_literal()]),
                )
            } else {
                PropositionalConjunction::new(cp_reason.into_boxed_slice(), Default::default())
            };

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

        let reason = solver.get_reason_int(predicate![y <= 6]);

        assert_eq!(conjunction!([x >= 1]), *reason);
    }

    #[test]
    fn test_reified_does_not_propagate() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);
        let reif = solver.new_literal();

        let mut propagator = solver
            .new_propagator(LinearLessOrEqualConstructor::reified(
                [x, y].into(),
                7,
                reif,
            ))
            .expect("No conflict");

        solver.set_literal(reif, false);

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 10);
    }

    #[test]
    fn test_reified_does_propagate() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);
        let reif = solver.new_literal();

        let mut propagator = solver
            .new_propagator(LinearLessOrEqualConstructor::reified(
                [x, y].into(),
                7,
                reif,
            ))
            .expect("No conflict");

        solver.set_literal(reif, true);

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_reified_propagate_literal_reason() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(10, 10);
        let y = solver.new_variable(10, 10);
        let reif = solver.new_literal();

        let _ = solver
            .new_propagator(LinearLessOrEqualConstructor::reified(
                [x, y].into(),
                1,
                reif,
            ))
            .expect("No conflict");

        assert!(solver.is_literal_false(reif));

        let reason = solver.get_reason_bool(reif, false);

        assert_eq!(conjunction!([x >= 10] & [y >= 10]), *reason);
    }
}
