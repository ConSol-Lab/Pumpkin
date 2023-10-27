use std::collections::HashMap;

use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, DomainEvent,
        LocalId, PropagationContext, PropagatorConstructorContext, PropagatorVariable,
    },
    predicate,
};

pub struct LinearLeqArgs<Var> {
    pub x: Box<[Var]>,
    pub c: i32,
}

/// Propagator for the constraint `\sum x_i <= c`.
pub struct LinearLeq<Var> {
    x: Box<[PropagatorVariable<Var>]>,
    c: i32,
    propagations: Box<[HashMap<i32, PropositionalConjunction>]>,
}

impl<Var> CPPropagatorConstructor for LinearLeq<Var>
where
    Var: IntVar + 'static,
{
    type Args = LinearLeqArgs<Var>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        let x: Box<[_]> = args
            .x
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(
                    x_i.clone(),
                    DomainEvent::LowerBound,
                    LocalId::from(i as u32),
                )
            })
            .collect();

        let propagations = (0..x.len()).map(|_| HashMap::new()).collect();

        Box::new(LinearLeq {
            x,
            c: args.c,
            propagations,
        })
    }
}

impl<Var> ConstraintProgrammingPropagator for LinearLeq<Var>
where
    Var: IntVar,
{
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        let lb_lhs = self
            .x
            .iter()
            .map(|var| context.lower_bound(var))
            .sum::<i32>();

        if self.c - lb_lhs < 0 {
            let reason = self
                .x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect::<Vec<_>>();

            return Err(reason.into());
        }

        for (i, x_i) in self.x.iter().enumerate() {
            let bound = self.c - (lb_lhs - context.lower_bound(x_i));

            if context.upper_bound(x_i) > bound {
                let reason = self
                    .x
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

                self.propagations[i].insert(bound, reason.into());
                context.set_upper_bound(x_i, bound)?;
            }
        }

        Ok(())
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
        let DomainChange::UpperBound(bound) = self.x[i as usize].unpack(delta) else {
            unreachable!();
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
        let lb_lhs = self
            .x
            .iter()
            .map(|var| context.lower_bound(var))
            .sum::<i32>();

        if self.c - lb_lhs < 0 {
            let reason = self
                .x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect::<Vec<_>>();

            return Err(reason.into());
        }

        for x_i in self.x.iter() {
            let bound = self.c - (lb_lhs - context.lower_bound(x_i));

            if context.upper_bound(x_i) > bound {
                context.set_upper_bound(x_i, bound)?;
            }
        }

        Ok(())
    }
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

        let mut propagator = solver.new_propagator::<LinearLeq<_>>(LinearLeqArgs {
            x: [x, y].into(),
            c: 7,
        });

        solver.propagate(&mut propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_explanations() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let mut propagator = solver.new_propagator::<LinearLeq<_>>(LinearLeqArgs {
            x: [x, y].into(),
            c: 7,
        });

        solver.propagate(&mut propagator).expect("non-empty domain");

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::UpperBound(6)),
        );

        assert_eq!(conjunction!([x >= 1]), reason);
    }
}
