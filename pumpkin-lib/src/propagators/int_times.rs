use std::collections::HashMap;

use log::warn;

use crate::{
    basic_types::{variables::IntVar, Predicate, PropagationStatusCP, PropositionalConjunction},
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, DomainEvent,
        LocalId, PropagationContext, PropagatorConstructorContext, PropagatorVariable,
    },
    predicate,
};

/// A bounds-consistent propagator for maintaining the constraint `a * b = c`. The propagator
/// assumes `a, b, c >= 0`.
pub struct IntTimes<VA, VB, VC> {
    a: PropagatorVariable<VA>,
    b: PropagatorVariable<VB>,
    c: PropagatorVariable<VC>,

    propagations_a: HashMap<DomainChange, [Predicate; 2]>,
    propagations_b: HashMap<DomainChange, [Predicate; 2]>,
    propagations_c: HashMap<DomainChange, [Predicate; 2]>,
}

pub struct IntTimesArgs<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

impl<VA, VB, VC> CPPropagatorConstructor for IntTimes<VA, VB, VC>
where
    VA: IntVar + 'static,
    VB: IntVar + 'static,
    VC: IntVar + 'static,
{
    type Args = IntTimesArgs<VA, VB, VC>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        Box::new(IntTimes {
            a: context.register(args.a, DomainEvent::Any, ID_A),
            b: context.register(args.b, DomainEvent::Any, ID_B),
            c: context.register(args.c, DomainEvent::Any, ID_C),

            propagations_a: Default::default(),
            propagations_b: Default::default(),
            propagations_c: Default::default(),
        })
    }
}

impl<VA, VB, VC> ConstraintProgrammingPropagator for IntTimes<VA, VB, VC>
where
    VA: IntVar,
    VB: IntVar,
    VC: IntVar,
{
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        let a_min = context.lower_bound(&self.a);
        let a_max = context.upper_bound(&self.a);
        let b_min = context.lower_bound(&self.b);
        let b_max = context.upper_bound(&self.b);
        let c_min = context.lower_bound(&self.c);
        let c_max = context.upper_bound(&self.c);

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

        if context.upper_bound(&self.c) > new_max_c {
            self.propagations_c.insert(
                DomainChange::UpperBound(new_max_c),
                [predicate![self.a <= a_max], predicate![self.b <= b_max]],
            );
            context.set_upper_bound(&self.c, new_max_c)?;
        }

        if context.lower_bound(&self.c) < new_min_c {
            self.propagations_c.insert(
                DomainChange::LowerBound(new_min_c),
                [predicate![self.a >= a_min], predicate![self.b >= b_min]],
            );
            context.set_lower_bound(&self.c, new_min_c)?;
        }

        // a >= ceil(c.min / b.max)
        if b_max >= 1 {
            let bound = (c_min + b_max - 1) / b_max;
            if context.lower_bound(&self.a) < bound {
                self.propagations_a.insert(
                    DomainChange::LowerBound(bound),
                    [predicate![self.c >= c_min], predicate![self.b <= b_max]],
                );
                context.set_lower_bound(&self.a, bound)?;
            }
        }

        // a <= floor(c.max / b.min)
        if b_min >= 1 {
            let bound = c_max / b_min;
            if context.upper_bound(&self.a) > bound {
                self.propagations_a.insert(
                    DomainChange::UpperBound(bound),
                    [predicate![self.c <= c_max], predicate![self.b >= b_min]],
                );
                context.set_upper_bound(&self.a, bound)?;
            }
        }

        // b >= ceil(c.min / a.max)
        if a_max >= 1 {
            let bound = (c_min + a_max - 1) / a_max;

            if context.lower_bound(&self.b) < bound {
                self.propagations_b.insert(
                    DomainChange::LowerBound(bound),
                    [predicate![self.c >= c_min], predicate![self.a <= a_max]],
                );
                context.set_lower_bound(&self.b, bound)?;
            }
        }

        // b <= floor(c.max / a.min)
        if a_min >= 1 {
            let bound = c_max / a_min;
            if context.upper_bound(&self.b) > bound {
                self.propagations_b.insert(
                    DomainChange::UpperBound(bound),
                    [predicate![self.c <= c_max], predicate![self.a >= a_min]],
                );
                context.set_upper_bound(&self.b, bound)?;
            }
        }

        Ok(())
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        fn cleanup<Var: IntVar>(
            context: &PropagationContext,
            var: &PropagatorVariable<Var>,
            propagations: &mut HashMap<DomainChange, [Predicate; 2]>,
        ) {
            propagations.retain(|change, _reason| match change {
                DomainChange::Removal(value) => !context.contains(var, *value),
                DomainChange::LowerBound(bound) => context.lower_bound(var) >= *bound,
                DomainChange::UpperBound(bound) => context.upper_bound(var) <= *bound,
            });
        }

        cleanup(context, &self.a, &mut self.propagations_a);
        cleanup(context, &self.b, &mut self.propagations_b);
        cleanup(context, &self.c, &mut self.propagations_c);
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        let reason = match delta.affected_local_id() {
            ID_A => self.propagations_a[&self.a.unpack(delta)],
            ID_B => self.propagations_b[&self.b.unpack(delta)],
            ID_C => self.propagations_c[&self.c.unpack(delta)],
            _ => unreachable!(),
        };

        PropositionalConjunction::from(reason.to_vec())
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "IntTimes"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        if context.lower_bound(&self.a).is_negative() || context.lower_bound(&self.b).is_negative()
        {
            warn!("IntTimes does not support variables with negative bounds, but it is instantiated with them. The behavior will likely not be correct.");
        }

        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        let a_min = context.lower_bound(&self.a);
        let a_max = context.upper_bound(&self.a);
        let b_min = context.lower_bound(&self.b);
        let b_max = context.upper_bound(&self.b);
        let c_min = context.lower_bound(&self.c);
        let c_max = context.upper_bound(&self.c);

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

        context.set_upper_bound(&self.c, a_max * b_max)?;
        context.set_lower_bound(&self.c, a_min * b_min)?;

        // a >= ceil(c.min / b.max)
        if b_max >= 1 {
            context.set_lower_bound(&self.a, (c_min + b_max - 1) / b_max)?;
        }

        // a <= floor(c.max / b.min)
        if b_min >= 1 {
            context.set_upper_bound(&self.a, c_max / b_min)?;
        }

        // b >= ceil(c.min / a.max)
        if a_max >= 1 {
            context.set_lower_bound(&self.b, (c_min + a_max - 1) / a_max)?;
        }

        // b <= floor(c.max / a.min)
        if a_min >= 1 {
            context.set_upper_bound(&self.b, c_max / a_min)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{conjunction, engine::test_helper::TestSolver};

    use super::*;

    #[test]
    fn bounds_of_a_and_b_propagate_bounds_c() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(0, 4);
        let c = solver.new_variable(-10, 20);

        let mut propagator = solver.new_propagator::<IntTimes<_, _, _>>(IntTimesArgs { a, b, c });

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(0, solver.lower_bound(b));
        assert_eq!(4, solver.upper_bound(b));
        assert_eq!(0, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason(
            &mut propagator,
            Delta::new(ID_C, DomainChange::LowerBound(0)),
        );
        assert_eq!(conjunction!([a >= 1] & [b >= 0]), reason_lb);

        let reason_ub = solver.get_reason(
            &mut propagator,
            Delta::new(ID_C, DomainChange::UpperBound(12)),
        );
        assert_eq!(conjunction!([a <= 3] & [b <= 4]), reason_ub);
    }

    #[test]
    fn bounds_of_a_and_c_propagate_bounds_b() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(2, 3);
        let b = solver.new_variable(0, 12);
        let c = solver.new_variable(2, 12);

        let mut propagator = solver.new_propagator::<IntTimes<_, _, _>>(IntTimesArgs { a, b, c });

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(2, solver.lower_bound(a));
        assert_eq!(3, solver.upper_bound(a));
        assert_eq!(1, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(2, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason(
            &mut propagator,
            Delta::new(ID_B, DomainChange::LowerBound(1)),
        );
        assert_eq!(conjunction!([a <= 3] & [c >= 2]), reason_lb);

        let reason_ub = solver.get_reason(
            &mut propagator,
            Delta::new(ID_B, DomainChange::UpperBound(6)),
        );
        assert_eq!(conjunction!([a >= 2] & [c <= 12]), reason_ub);
    }

    #[test]
    fn bounds_of_b_and_c_propagate_bounds_a() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 10);
        let b = solver.new_variable(3, 6);
        let c = solver.new_variable(2, 12);

        let mut propagator = solver.new_propagator::<IntTimes<_, _, _>>(IntTimesArgs { a, b, c });

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(a));
        assert_eq!(4, solver.upper_bound(a));
        assert_eq!(3, solver.lower_bound(b));
        assert_eq!(6, solver.upper_bound(b));
        assert_eq!(2, solver.lower_bound(c));
        assert_eq!(12, solver.upper_bound(c));

        let reason_lb = solver.get_reason(
            &mut propagator,
            Delta::new(ID_A, DomainChange::LowerBound(1)),
        );
        assert_eq!(conjunction!([b <= 6] & [c >= 2]), reason_lb);

        let reason_ub = solver.get_reason(
            &mut propagator,
            Delta::new(ID_A, DomainChange::UpperBound(4)),
        );
        assert_eq!(conjunction!([b >= 3] & [c <= 12]), reason_ub);
    }
}
