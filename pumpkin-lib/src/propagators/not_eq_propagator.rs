use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    conjunction,
    engine::{
        CPPropagatorConstructor, Change, ConstraintProgrammingPropagator, Delta, DomainEvent,
        LocalId, PropagationContext, PropagatorConstructorContext, PropagatorVariable,
    },
};

pub struct NotEq<VX, VY> {
    x: PropagatorVariable<VX>,
    y: PropagatorVariable<VY>,
}

pub struct NotEqArgs<VX, VY> {
    x: VX,
    y: VY,
}

const ID_X: LocalId = LocalId::from(0);
const ID_Y: LocalId = LocalId::from(1);

impl<VX: IntVar + 'static, VY: IntVar + 'static> CPPropagatorConstructor for NotEq<VX, VY> {
    type Args = NotEqArgs<VX, VY>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        Box::new(NotEq {
            x: context.register(args.x, DomainEvent::Assign, ID_X),
            y: context.register(args.y, DomainEvent::Assign, ID_Y),
        })
    }
}

impl<VX: IntVar, VY: IntVar> ConstraintProgrammingPropagator for NotEq<VX, VY> {
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        propagate_one_direction(&self.x, &self.y, context)?;
        propagate_one_direction(&self.y, &self.x, context)?;

        Ok(())
    }

    fn synchronise(&mut self, _: &PropagationContext) {}

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        match delta.affected_local_id() {
            ID_X => {
                if let Change::Removal(value) = self.x.unpack(delta) {
                    conjunction!([self.y == value])
                } else {
                    unreachable!("Only a singular value can be removed by this propagator.");
                }
            }
            ID_Y => {
                if let Change::Removal(value) = self.y.unpack(delta) {
                    conjunction!([self.x == value])
                } else {
                    unreachable!("Only a singular value can be removed by this propagator.");
                }
            }
            id => unreachable!("Unknown explanation code {id} for NotEq propagator"),
        }
    }

    fn priority(&self) -> u32 {
        1
    }

    fn name(&self) -> &str {
        "NotEq"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        _context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        todo!()
    }
}

fn propagate_one_direction<VX: IntVar, VY: IntVar>(
    x: &PropagatorVariable<VX>,
    y: &PropagatorVariable<VY>,
    context: &mut PropagationContext,
) -> PropagationStatusCP {
    if !context.is_fixed(x) {
        return Ok(());
    }

    let value = context.lower_bound(x);
    if context.contains(y, value) {
        context.remove(y, value);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{conjunction, engine::test_helper::TestSolver};

    use super::*;

    #[test]
    fn propagator_removes_from_x_the_fixed_value_of_y() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 10);
        let y = solver.new_variable(4, 4);

        let mut propagator = solver.new_propagator::<NotEq<_, _>>(NotEqArgs { x, y });
        solver.propagate(&mut propagator).expect("no inconsistency");

        assert!(!solver.contains(x, 4));

        let reason = solver.get_reason(&mut propagator, Delta::new(ID_X, Change::Removal(4)));
        assert_eq!(conjunction!([y == 4]), reason);
    }

    #[test]
    fn propagator_removes_from_y_the_fixed_value_of_x() {
        let mut solver = TestSolver::default();
        let y = solver.new_variable(1, 10);
        let x = solver.new_variable(4, 4);

        let mut propagator = solver.new_propagator::<NotEq<_, _>>(NotEqArgs { x, y });
        solver.propagate(&mut propagator).expect("no inconsistency");

        assert!(!solver.contains(y, 4));

        let reason = solver.get_reason(&mut propagator, Delta::new(ID_Y, Change::Removal(4)));
        assert_eq!(conjunction!([x == 4]), reason);
    }
}
