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
        propagate_one_direction(&self.x, &self.y, domains)
            .and_then(|| propagate_one_direction(&self.y, &self.x, domains))
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
        return PropagationStatusCP::NoConflictDetected;
    }

    let value = x.lower_bound(domains);
    if !y.contains(domains, value) {
        let x_predicate = x.equality_predicate(value);
        let y_predicate = y.disequality_predicate(value);

        return PropagationStatusCP::ConflictDetected {
            failure_reason: vec![x_predicate, y_predicate].into(),
        };
    }

    y.remove(domains, value);
    PropagationStatusCP::NoConflictDetected
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::DomainId,
        engine::{ConstraintSatisfactionSolver, DomainOperationOutcome},
    };

    use super::*;

    #[test]
    fn propagator_removes_from_y_the_fixed_value_of_x() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let x = solver.create_new_integer_variable(1, 10);
        let y = ConstantView(4);

        solver.add_propagator(Box::new(NotEq::new(x, y)));

        assert!(!solver.get_integer_assignments().is_value_in_domain(x, 4));
    }

    #[test]
    fn propagator_removes_from_x_the_fixed_value_of_y() {
        let mut solver = ConstraintSatisfactionSolver::default();
        let y = solver.create_new_integer_variable(1, 10);
        let x = ConstantView(4);

        solver.add_propagator(Box::new(NotEq::new(x, y)));

        assert!(!solver.get_integer_assignments().is_value_in_domain(y, 4));
    }

    /// Model a variable as a constant. This does not fully implement the IntVar trait, but should
    /// be good enough for tests.
    #[derive(Clone)]
    pub struct ConstantView(i32);

    impl IntVar for ConstantView {
        fn lower_bound(&self, _: &DomainManager) -> i32 {
            self.0
        }

        fn upper_bound(&self, _: &DomainManager) -> i32 {
            self.0
        }

        fn contains(&self, _: &DomainManager, value: i32) -> bool {
            self.0 == value
        }

        fn remove(&self, _: &mut DomainManager, _: i32) -> DomainOperationOutcome {
            todo!()
        }

        fn equality_predicate(&self, value: i32) -> Predicate {
            assert_eq!(
                value, self.0,
                "Can only create equality predicate if the value {value} matches the constant {}.",
                self.0
            );

            Predicate::Equal {
                integer_variable: DomainId { id: 0 },
                equality_constant: value,
            }
        }

        fn disequality_predicate(&self, value: i32) -> Predicate {
            assert_ne!(
                value, self.0,
                "Can only create disequality predicate if the value {value} differs from the constant {}.",
                self.0
            );

            Predicate::NotEqual {
                integer_variable: DomainId { id: 0 },
                not_equal_constant: value,
            }
        }

        fn watch(&self, _: &mut Watchers<'_>, _: DomainEvent) {}

        fn lower_bound_predicate(&self, _: i32) -> Predicate {
            todo!()
        }

        fn upper_bound_predicate(&self, _: i32) -> Predicate {
            todo!()
        }

        fn set_lower_bound(&self, _: &mut DomainManager, _: i32) -> DomainOperationOutcome {
            todo!()
        }

        fn set_upper_bound(&self, _: &mut DomainManager, _: i32) -> DomainOperationOutcome {
            todo!()
        }
    }
}
