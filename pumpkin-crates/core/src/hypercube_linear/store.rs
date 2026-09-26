use crate::basic_types::PredicateId;
use crate::containers::HashMap;
use crate::engine::PropagationStatusCP;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearPropagator;
use crate::hypercube_linear::LinearInequality;
use crate::proof::InferenceCode;
use crate::propagation::EnqueueDecision;
use crate::propagation::EventsToRegister;
use crate::propagation::ExplanationContext;
use crate::propagation::LocalId;
use crate::propagation::NotificationContext;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorSpec;
use crate::propagation::RuntimeCheckers;

/// The [`PropagatorConstructor`] for the [`HypercubeLinearStore`], which starts without
/// constraints.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct HypercubeLinearStoreConstructor;

impl PropagatorConstructor for HypercubeLinearStoreConstructor {
    type PropagatorImpl = HypercubeLinearStore;

    fn create(
        self,
        _context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        PropagatorSpec {
            registration: EventsToRegister::empty(),
            // The members register their own checkers when they are added.
            checkers: RuntimeCheckers::empty(),
            propagator: HypercubeLinearStore::default(),
        }
    }
}

/// A single propagator for all hypercube linear constraints, in the style of the nogood
/// propagator.
///
/// Every constraint is a [`HypercubeLinearPropagator`] whose member index identifies it: the store
/// routes the notifications for the predicates it watches and the domain events of its terms to
/// it, and propagates only the constraints that were notified.
#[derive(Clone, Debug, Default)]
pub struct HypercubeLinearStore {
    members: Vec<HypercubeLinearPropagator>,
    /// For every watched predicate, the members that watch it.
    watchers: HashMap<PredicateId, Vec<u32>>,
    /// The members that were notified since they were last propagated.
    to_propagate: Vec<u32>,
    is_to_propagate: Vec<bool>,
}

impl HypercubeLinearStore {
    /// The index that the next added member gets.
    pub(crate) fn next_member_index(&self) -> u32 {
        u32::try_from(self.members.len()).expect("fewer than u32::MAX hypercube linears")
    }

    /// Adds a member, whose member index must be [`Self::next_member_index`], and schedules it
    /// for propagation.
    pub(crate) fn add_member(&mut self, member: HypercubeLinearPropagator) {
        let index = self.next_member_index();

        for predicate_id in member.watched_predicate_ids() {
            self.watchers.entry(predicate_id).or_default().push(index);
        }

        self.members.push(member);
        self.is_to_propagate.push(false);
        let _ = self.schedule(index);
    }

    fn schedule(&mut self, index: u32) -> bool {
        let is_to_propagate = &mut self.is_to_propagate[index as usize];
        if *is_to_propagate {
            return false;
        }

        *is_to_propagate = true;
        self.to_propagate.push(index);
        true
    }

    /// Updates the watch map after the watchers of `index` changed from `before` to `after`.
    fn update_watchers(&mut self, index: u32, before: &[PredicateId], after: &[PredicateId]) {
        for predicate_id in before.iter().filter(|p| !after.contains(p)) {
            let members = self
                .watchers
                .get_mut(predicate_id)
                .expect("a watched predicate is in the watch map");
            let position = members
                .iter()
                .position(|&member| member == index)
                .expect("the member watches the predicate");
            let _ = members.swap_remove(position);
        }

        for &predicate_id in after.iter().filter(|p| !before.contains(p)) {
            self.watchers.entry(predicate_id).or_default().push(index);
        }
    }
}

impl Propagator for HypercubeLinearStore {
    fn name(&self) -> &str {
        "HypercubeLinearStore"
    }

    fn notify(
        &mut self,
        _context: NotificationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        // The local id of a domain event is the member index.
        let _ = self.schedule(local_id.unpack());
        EnqueueDecision::Enqueue
    }

    fn notify_predicate_id_satisfied(
        &mut self,
        _context: NotificationContext,
        predicate_id: PredicateId,
    ) -> EnqueueDecision {
        let members = self
            .watchers
            .get(&predicate_id)
            .cloned()
            .unwrap_or_default();

        let mut scheduled = false;
        for index in members {
            scheduled |= self.schedule(index);
        }

        if scheduled || !self.to_propagate.is_empty() {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    fn synchronise(&mut self, _context: NotificationContext<'_>) {
        for index in self.to_propagate.drain(..) {
            self.is_to_propagate[index as usize] = false;
        }
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        while let Some(index) = self.to_propagate.pop() {
            self.is_to_propagate[index as usize] = false;

            let before = self.members[index as usize].watched_predicate_ids();
            let result = self.members[index as usize].propagate(context.reborrow());
            let after = self.members[index as usize].watched_predicate_ids();

            if before != after {
                self.update_watchers(index, &before, &after);
            }

            result?;
        }

        Ok(())
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        for member in &self.members {
            member.propagate_from_scratch(context.reborrow())?;
        }

        Ok(())
    }

    fn explain_as_hypercube_linear(
        &mut self,
        code: u64,
        context: ExplanationContext,
    ) -> Option<(Hypercube, LinearInequality, InferenceCode)> {
        // The code of a lazy explanation is the member index.
        self.members[code as usize].explain_as_hypercube_linear(code, context)
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;
    use crate::hypercube_linear::HypercubeLinearConstructor;
    use crate::predicate;
    use crate::state::State;
    use crate::variables::DomainId;

    fn aggregating_state() -> State {
        let mut state = State::default();
        state.hypercube_linear_aggregate = true;
        state
    }

    fn add(state: &mut State, hypercube: Hypercube, terms: &[(i32, DomainId)], bound: i32) {
        let linear = LinearInequality::new(
            terms
                .iter()
                .map(|&(weight, domain)| (NonZero::new(weight).unwrap(), domain)),
            bound,
        )
        .expect("not trivially satisfiable");
        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_hypercube_linear(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });
    }

    #[test]
    fn a_propagation_of_one_member_triggers_another() {
        let mut state = aggregating_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);

        // [x >= 2] -> y <= 3, and -x <= -2, which makes the hypercube of the first true.
        add(
            &mut state,
            Hypercube::from_single_predicate(predicate![x >= 2]),
            &[(1, y)],
            3,
        );
        add(&mut state, Hypercube::default(), &[(-1, x)], -2);

        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.lower_bound(x), 2);
        assert_eq!(state.upper_bound(y), 3);
    }

    #[test]
    fn members_propagate_again_after_backtracking() {
        let mut state = aggregating_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);
        let z = state.new_interval_variable(0, 10, None);

        // [x >= 2] -> y + z <= 5.
        add(
            &mut state,
            Hypercube::from_single_predicate(predicate![x >= 2]),
            &[(1, y), (1, z)],
            5,
        );
        assert!(state.propagate_to_fixed_point().is_ok());

        state.new_checkpoint();
        assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
        assert!(state.post(predicate![y >= 4]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(z), 1);

        let _ = state.restore_to(0);
        assert_eq!(state.upper_bound(z), 10);

        state.new_checkpoint();
        assert!(state.post(predicate![y >= 1]).expect("not empty domain"));
        assert!(state.post(predicate![x >= 3]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(z), 4);
    }

    #[test]
    fn a_bound_change_by_one_member_reaches_another() {
        let mut state = aggregating_state();

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);

        // x + y <= 7 propagates first; then -x <= -5 raises the lower bound of x, after which the
        // first constraint must tighten y to at most 2.
        add(&mut state, Hypercube::default(), &[(1, x), (1, y)], 7);
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(y), 7);

        add(&mut state, Hypercube::default(), &[(-1, x)], -5);
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.lower_bound(x), 5);
        assert_eq!(state.upper_bound(y), 2);
    }

    fn upper_bound_of_negative_term_reaches(aggregate: bool) {
        let mut state = State::default();
        state.hypercube_linear_aggregate = aggregate;

        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);

        // -x + y <= 1 depends on the upper bound of x.
        add(&mut state, Hypercube::default(), &[(-1, x), (1, y)], 1);
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(y), 10);

        state.new_checkpoint();
        assert!(state.post(predicate![x <= 3]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        assert_eq!(state.upper_bound(y), 4);
    }

    #[test]
    fn upper_bound_of_a_negative_term_reaches_a_member() {
        upper_bound_of_negative_term_reaches(true);
    }

    #[test]
    fn upper_bound_of_a_negative_term_reaches_a_propagator() {
        upper_bound_of_negative_term_reaches(false);
    }
}
