use crate::{
    basic_types::{variables::IntVar, Predicate, PropagationStatusCP, PropositionalConjunction},
    engine::{DomainEvent, DomainManager, Watchers},
};

use super::ConstraintProgrammingPropagator;

pub struct NotEq<VX, VY> {
    x: VX,
    y: VY,
}

impl<VX, VY> NotEq<VX, VY> {
    pub fn new(x: VX, y: VY) -> Self {
        Self { x, y }
    }
}

impl<VX: IntVar, VY: IntVar> ConstraintProgrammingPropagator for NotEq<VX, VY> {
    fn propagate(&mut self, domains: &mut DomainManager) -> PropagationStatusCP {
        propagate_one_direction(&self.x, &self.y, domains)?;
        propagate_one_direction(&self.y, &self.x, domains)?;

        Ok(())
    }

    fn synchronise(&mut self, _: &DomainManager) {}

    fn get_reason_for_propagation(&mut self, _predicate: Predicate) -> PropositionalConjunction {
        todo!()
    }

    fn priority(&self) -> u32 {
        1
    }

    fn name(&self) -> &str {
        "NotEq"
    }

    fn initialise_at_root(&mut self, domains: &mut DomainManager) -> PropagationStatusCP {
        self.propagate(domains)
    }

    fn debug_propagate_from_scratch(&self, _domains: &mut DomainManager) -> PropagationStatusCP {
        todo!()
    }

    fn register_watches(&self, watchers: &mut Watchers<'_>) {
        self.x.watch(watchers, DomainEvent::Any);
        self.y.watch(watchers, DomainEvent::Any);
    }
}

fn propagate_one_direction<VX: IntVar, VY: IntVar>(
    x: &VX,
    y: &VY,
    domains: &mut DomainManager,
) -> PropagationStatusCP {
    if x.lower_bound(domains) != x.upper_bound(domains) {
        return Ok(());
    }

    let value = x.lower_bound(domains);
    if !y.contains(domains, value) {
        let x_predicate = x.equality_predicate(value);
        let y_predicate = y.disequality_predicate(value);

        return Err(vec![x_predicate, y_predicate].into());
    }

    y.remove(domains, value);

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::engine::ConstraintSatisfactionSolver;

    use super::*;

    #[test]
    fn propagator_removes_from_y_the_fixed_value_of_x() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let x = solver.create_new_integer_variable(1, 10);
        let y = solver.create_new_integer_variable(4, 4);

        solver.add_propagator(Box::new(NotEq::new(x, y)));

        assert!(!solver.get_integer_assignments().is_value_in_domain(x, 4));
    }

    #[test]
    fn propagator_removes_from_x_the_fixed_value_of_y() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let y = solver.create_new_integer_variable(1, 10);
        let x = solver.create_new_integer_variable(4, 4);

        solver.add_propagator(Box::new(NotEq::new(x, y)));

        assert!(!solver.get_integer_assignments().is_value_in_domain(y, 4));
    }
}
