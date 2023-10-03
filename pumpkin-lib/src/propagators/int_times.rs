use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    conjunction,
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, DomainEvent,
        LocalId, PropagationContext, PropagatorConstructorContext, PropagatorVariable,
    },
};

/// A bounds-consistent propagator for maintaining the constraint `a * b = c`. The propagator
/// assumes `a, b, c >= 0`.
pub struct IntTimes<VA, VB, VC> {
    a: PropagatorVariable<VA>,
    b: PropagatorVariable<VB>,
    c: PropagatorVariable<VC>,

    propagations: [Vec<[i32; 2]>; 3],
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

            propagations: Default::default(),
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
        self.debug_propagate_from_scratch(context)
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        use DomainChange::*;

        match delta.affected_local_id() {
            ID_A => match self.a.unpack(delta) {
                LowerBound(val) => {
                    let [b_max, c_min] = self.propagations[ID_A.unpack() as usize][val as usize];
                    conjunction!([self.b <= b_max] & [self.c >= c_min])
                }

                UpperBound(val) => {
                    let [b_min, c_max] = self.propagations[ID_A.unpack() as usize][val as usize];
                    conjunction!([self.b >= b_min] & [self.c <= c_max])
                }

                Removal(_) => unreachable!(),
            },
            ID_B => match self.b.unpack(delta) {
                LowerBound(val) => {
                    let [a_max, c_min] = self.propagations[ID_B.unpack() as usize][val as usize];
                    conjunction!([self.a <= a_max] & [self.c >= c_min])
                }

                UpperBound(val) => {
                    let [a_min, c_max] = self.propagations[ID_B.unpack() as usize][val as usize];
                    conjunction!([self.a >= a_min] & [self.c <= c_max])
                }

                Removal(_) => unreachable!(),
            },
            ID_C => match self.c.unpack(delta) {
                LowerBound(val) => {
                    let [a_min, b_min] = self.propagations[ID_C.unpack() as usize][val as usize];
                    conjunction!([self.a >= a_min] & [self.b >= b_min])
                }

                UpperBound(val) => {
                    let [a_max, b_max] = self.propagations[ID_C.unpack() as usize][val as usize];
                    conjunction!([self.a <= a_max] & [self.b <= b_max])
                }

                Removal(_) => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn priority(&self) -> u32 {
        todo!()
    }

    fn name(&self) -> &str {
        todo!()
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
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
            context.set_upper_bound(&self.a, c_max / a_min)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::test_helper::TestSolver;

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
    }
}
