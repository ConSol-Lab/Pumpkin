use crate::basic_types::variables::IntVar;
use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::CPPropagatorConstructor;
use crate::engine::ConstraintProgrammingPropagator;
use crate::engine::DomainEvents;
use crate::engine::LocalId;
use crate::engine::PropagationContext;
use crate::engine::PropagationContextMut;
use crate::engine::PropagatorConstructorContext;
use crate::engine::PropagatorVariable;
use crate::engine::ReadDomains;

pub struct NotEqProp<VX, VY> {
    x: PropagatorVariable<VX>,
    y: PropagatorVariable<VY>,
}

pub struct NotEq<VX, VY> {
    pub x: VX,
    pub y: VY,
}

const ID_X: LocalId = LocalId::from(0);
const ID_Y: LocalId = LocalId::from(1);

impl<VX: IntVar, VY: IntVar> CPPropagatorConstructor for NotEq<VX, VY> {
    type Propagator = NotEqProp<VX, VY>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        NotEqProp {
            x: context.register(self.x, DomainEvents::ASSIGN, ID_X),
            y: context.register(self.y, DomainEvents::ASSIGN, ID_Y),
        }
    }
}

impl<VX: IntVar, VY: IntVar> ConstraintProgrammingPropagator for NotEqProp<VX, VY> {
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        propagate_one_direction(&self.x, &self.y, context)?;
        propagate_one_direction(&self.y, &self.x, context)?;

        Ok(())
    }

    fn synchronise(&mut self, _: &PropagationContext) {}

    fn priority(&self) -> u32 {
        1
    }

    fn name(&self) -> &str {
        "NotEq"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        propagate_one_direction(&self.x, &self.y, context)?;
        propagate_one_direction(&self.y, &self.x, context)?;

        Ok(())
    }
}

fn propagate_one_direction<VX: IntVar, VY: IntVar>(
    x: &PropagatorVariable<VX>,
    y: &PropagatorVariable<VY>,
    context: &mut PropagationContextMut,
) -> PropagationStatusCP {
    if !context.is_fixed(x) {
        return Ok(());
    }

    let value = context.lower_bound(x);
    if context.contains(y, value) {
        context.remove(y, value, conjunction!([x == value]))?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;
    use crate::predicate;

    #[test]
    fn propagator_removes_from_x_the_fixed_value_of_y() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 10);
        let y = solver.new_variable(4, 4);

        let mut propagator = solver
            .new_propagator(NotEq { x, y })
            .expect("no empty domains");
        solver.propagate(&mut propagator).expect("no inconsistency");

        assert!(!solver.contains(x, 4));

        let reason = solver.get_reason_int(predicate![x != 4]);
        assert_eq!(conjunction!([y == 4]), *reason);
    }

    #[test]
    fn propagator_removes_from_y_the_fixed_value_of_x() {
        let mut solver = TestSolver::default();
        let y = solver.new_variable(1, 10);
        let x = solver.new_variable(4, 4);

        let mut propagator = solver
            .new_propagator(NotEq { x, y })
            .expect("no empty domains");
        solver.propagate(&mut propagator).expect("no inconsistency");

        assert!(!solver.contains(y, 4));

        let reason = solver.get_reason_int(predicate![y != 4]);
        assert_eq!(conjunction!([x == 4]), *reason);
    }
}
