use fixedbitset::FixedBitSet;
use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

use crate::circuit::CircuitChecker;

#[derive(Debug)]
pub struct CircuitConstructor<Var> {
    pub successors: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor for CircuitConstructor<Var> {
    type PropagatorImpl = CircuitPropagator<Var>;

    fn create(
        self,
        mut context: pumpkin_core::propagation::PropagatorConstructorContext,
    ) -> Self::PropagatorImpl {
        self.successors
            .iter()
            .enumerate()
            .for_each(|(index, successor)| {
                context.register(
                    successor.clone(),
                    DomainEvents::ASSIGN,
                    LocalId::from(index as u32),
                );
                context.register_backtrack(
                    successor.clone(),
                    DomainEvents::ASSIGN,
                    LocalId::from(index as u32),
                );
            });

        let mut recently_fixed = FixedBitSet::with_capacity(self.successors.len());
        for (index, var) in self.successors.iter().enumerate() {
            if context.is_fixed(var) {
                recently_fixed.insert(index);
            }
        }

        CircuitPropagator {
            successors: self.successors,
            inference_code: InferenceCode::new(self.constraint_tag, CircuitPrevent),
            recently_fixed,
        }
    }

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, CircuitPrevent),
            Box::new(CircuitChecker {
                successors: self.successors.clone(),
            }),
        );
    }
}

declare_inference_label!(CircuitPrevent);

#[derive(Debug, Clone)]
pub struct CircuitPropagator<Var> {
    successors: Box<[Var]>,
    inference_code: InferenceCode,

    recently_fixed: FixedBitSet,
}

impl<Var: IntegerVariable + 'static> Propagator for CircuitPropagator<Var> {
    fn name(&self) -> &str {
        "Circuit"
    }

    fn priority(&self) -> Priority {
        // TODO
        Priority::Medium
    }

    fn notify(
        &mut self,
        _context: NotificationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        self.recently_fixed.insert(local_id.unpack() as usize);
        EnqueueDecision::Enqueue
    }

    fn notify_backtrack(
        &mut self,
        _context: Domains,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) {
        self.recently_fixed.remove(local_id.unpack() as usize);
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        self.check(context.domains())
        // self.prevent(context)
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        todo!()
    }
}

impl<Var: IntegerVariable + 'static> CircuitPropagator<Var> {
    fn prevent(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        todo!()
    }

    fn create_prevent_explanation(
        &self,
        context: Domains,
        path: &[usize],
    ) -> PropositionalConjunction {
        todo!()
    }
}

impl<Var: IntegerVariable + 'static> CircuitPropagator<Var> {
    fn check(&mut self, context: Domains) -> PropagationStatusCP {
        // We keep track of:
        // 1. `cycle` - The elements which are in the (potential) current cycle; these are used in
        //    the explanation
        // 2. `explored` - The elements which have been visited; once we encounter any of these
        //    nodes, we can stop since we have already explored them
        // 3. `explored_current_iteration` - The elements which have been visited as part of the
        //    (potential) current cycle; used to detect when a cycle has occurred. Note that using
        //    `explored` for this purpose would be incorrect and would lead to conflicts being
        //    detected which are not actual cycles.
        let mut cycle = Vec::default();
        let mut explored = FixedBitSet::with_capacity(self.successors.len());
        let mut explored_current_iteration = FixedBitSet::with_capacity(self.successors.len());

        // We look at the variables which were recently fixed and use them as potential starts of
        // cycles
        while let Some(start) = self.recently_fixed.ones().next() {
            self.recently_fixed.remove(start);

            // If we have already explored this node before, then we can continue
            if explored.contains(start) {
                continue;
            }

            // We consider a new cycle
            explored_current_iteration.clear();
            cycle.clear();
            let mut current = start;

            // We will traverse the fixed path until we find a cycle
            loop {
                let var = &self.successors[current];

                // If we have seen this node befor in the current iteration, then we can simply
                // return here
                if explored_current_iteration.contains(current) {
                    // Of course, if it is a cycle containing all nodes, then we do not need to
                    // report an error
                    if cycle.len() == self.successors.len() {
                        return Ok(());
                    }

                    // But if it is a cycle which contains all, then we should
                    return Err(Conflict::Propagator(PropagatorConflict {
                        conjunction: self.create_check_explanation(context, &cycle),
                        inference_code: self.inference_code.clone(),
                    }));
                }

                // If the current variable is fixed, then we continue looking for a cycle by going
                // to the next node; if not, then we can break from this loop, since it is not a
                // cycle
                if context.is_fixed(var) {
                    let next = (context.lower_bound(var) - 1) as usize;

                    // If we have already encountered this node, then we know that a cycle cannot
                    // be found from this node.
                    if explored.contains(current) {
                        break;
                    }

                    // Next, we mark the current node as explored and as part of the potential
                    // cycle
                    explored.insert(current);
                    explored_current_iteration.insert(current);
                    cycle.push(current);

                    // Then we move on to the next node
                    current = next;
                } else {
                    break;
                }
            }
        }

        Ok(())
    }

    fn create_check_explanation(
        &self,
        context: Domains,
        cycle: &[usize],
    ) -> PropositionalConjunction {
        cycle
            .iter()
            .map(|&index| {
                let var = &self.successors[index];

                pumpkin_assert_moderate!(context.is_fixed(var));

                predicate!(var == context.lower_bound(var))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::state::State;

    use crate::circuit::CircuitConstructor;

    #[test]
    fn circuit_hamiltonian_path_conflict_detection() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 2, None);
        let y = state.new_interval_variable(3, 3, None);
        let z = state.new_interval_variable(1, 1, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(CircuitConstructor {
            successors: vec![x, y, z].into(),
            constraint_tag,
        });

        let result = state.propagate_to_fixed_point();

        assert!(
            result.is_ok(),
            "If there is a cycle concerning all variables, then no conflict should be reported"
        )
    }

    #[test]
    fn circuit_conflict_detection_simple() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 2, None);
        let y = state.new_interval_variable(1, 1, None);
        let z = state.new_interval_variable(1, 3, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(CircuitConstructor {
            successors: vec![x, y, z].into(),
            constraint_tag,
        });

        let result = state.propagate_to_fixed_point();

        assert!(
            result.is_err(),
            "If there is a cycle concerning all variables, then no conflict should be reported"
        )
    }
}
