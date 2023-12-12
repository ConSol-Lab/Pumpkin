use std::collections::HashMap;

use crate::engine::DomainEvents;
use crate::{
    basic_types::{variables::IntVar, Literal, PropagationStatusCP, PropositionalConjunction},
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, LocalId,
        PropagationContext, PropagatorConstructorContext, PropagatorVariable,
    },
    predicate,
};

pub struct LinearLeq<Var> {
    pub x: Box<[Var]>,
    pub c: i32,
    pub reif: Option<Literal>,
}

impl<Var: IntVar + 'static> LinearLeq<Var> {
    pub fn new(x: Box<[Var]>, c: i32) -> Self {
        LinearLeq { x, c, reif: None }
    }

    pub fn reified(x: Box<[Var]>, c: i32, reif: Literal) -> Self {
        LinearLeq {
            x,
            c,
            reif: Some(reif),
        }
    }
}

/// Propagator for the constraint `reif => \sum x_i <= c`.
pub struct LinearLeqProp<Var> {
    x: Box<[PropagatorVariable<Var>]>,
    c: i32,
    propagations: Box<[HashMap<i32, PropositionalConjunction>]>,
    pub reif: Option<PropagatorVariable<Literal>>,
}

impl<Var> CPPropagatorConstructor for LinearLeq<Var>
where
    Var: IntVar,
{
    type Propagator = LinearLeqProp<Var>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        if let Some(literal) = self.reif {
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
            let propagations = (0..x.len() + 1).map(|_| HashMap::new()).collect();
            LinearLeqProp::<Var> {
                x,
                c: self.c,
                propagations,
                reif: Some(context.register_literal(
                    literal,
                    DomainEvents::ANY_BOOL,
                    LocalId::from(self.x.len() as u32),
                )),
            }
        } else {
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
            let propagations = (0..x.len()).map(|_| HashMap::new()).collect();
            LinearLeqProp::<Var> {
                x,
                c: self.c,
                propagations,
                reif: None,
            }
        }
    }
}

impl<Var> ConstraintProgrammingPropagator for LinearLeqProp<Var>
where
    Var: IntVar,
{
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        perform_propagation(context, &self.x, self.c, &mut self.propagations, &self.reif)
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        for (i, x_i) in self.x.iter().enumerate() {
            self.propagations[i].retain(|bound, _reason| *bound >= context.upper_bound(x_i))
        }
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        let i = delta.affected_local_id().unpack();
        let bound = if i == self.x.len() as u32 {
            //Reification literal has been retrieved
            let DomainChange::LiteralAssignedFalse = self.reif.as_ref().unwrap().unpack(delta)
            else {
                unreachable!();
            };
            0
        } else {
            let DomainChange::UpperBound(bound) = self.x[i as usize].unpack(delta) else {
                unreachable!();
            };
            bound
        };

        self.propagations[i as usize]
            .get_mut(&bound)
            .unwrap()
            .clone()
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        let mut propagation_store = vec![HashMap::new(); self.x.len()];
        perform_propagation(context, &self.x, self.c, &mut propagation_store, &self.reif)
    }
}

fn perform_propagation<Var: IntVar>(
    context: &mut PropagationContext<'_>,
    x: &[PropagatorVariable<Var>],
    c: i32,
    propagation_store: &mut [HashMap<i32, PropositionalConjunction>],
    reif: &Option<PropagatorVariable<Literal>>,
) -> PropagationStatusCP {
    let lb_lhs = x.iter().map(|var| context.lower_bound(var)).sum::<i32>();
    let reified = reif.is_some();
    if c < lb_lhs {
        if reified && !context.is_literal_fixed(reif.as_ref().unwrap()) {
            context.assign_literal(reif.as_ref().unwrap(), false)?;
            let reason = x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect::<Vec<_>>();
            propagation_store[x.len()].insert(0, reason.into());
        } else if !reified || context.is_literal_true(reif.as_ref().unwrap()) {
            let reason = x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect::<Vec<_>>();
            let mut conjunction: PropositionalConjunction = reason.into();
            if reified {
                conjunction.and_literal(reif.as_ref().unwrap().get_literal());
            }
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
            let reason = x
                .iter()
                .enumerate()
                .filter_map(|(j, x_j)| {
                    if j != i {
                        Some(predicate![x_j >= context.lower_bound(x_j)])
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            propagation_store[i].insert(bound, reason.into());
            if reified {
                let conjunction = propagation_store[i].get_mut(&bound).unwrap();
                conjunction.and_literal(reif.as_ref().unwrap().get_literal());
            }
            context.set_upper_bound(x_i, bound)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{conjunction, engine::test_helper::TestSolver};

    use super::*;

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver
            .new_propagator(LinearLeq::new([x, y].into(), 7))
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
            .new_propagator(LinearLeq::new([x, y].into(), 7))
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::UpperBound(6)),
        );

        assert_eq!(conjunction!([x >= 1]), reason);
    }

    #[test]
    fn test_reified_does_not_propagate() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);
        let reif = solver.new_literal();

        let mut propagator = solver
            .new_propagator(LinearLeq::reified([x, y].into(), 7, reif))
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
            .new_propagator(LinearLeq::reified([x, y].into(), 7, reif))
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

        let mut propagator = solver
            .new_propagator(LinearLeq::reified([x, y].into(), 1, reif))
            .expect("No conflict");

        assert!(solver.is_literal_false(reif));

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(2), DomainChange::LiteralAssignedFalse),
        );

        assert_eq!(conjunction!([x >= 10] & [y >= 10]), reason);
    }
}
