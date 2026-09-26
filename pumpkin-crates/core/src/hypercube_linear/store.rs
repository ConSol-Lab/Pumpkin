use crate::basic_types::PredicateId;
use crate::containers::KeyedVec;
use crate::engine::PropagationStatusCP;
use crate::engine::notifications::DomainEvents;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::HypercubeLinearPropagator;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::propagator::member_index_of_lazy_code;
use crate::predicates::Predicate;
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
use crate::variables::AffineView;
use crate::variables::DomainId;

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
/// Every constraint is a [`HypercubeLinearPropagator`] whose member index identifies it. The store
/// watches the predicates that its members watch, and it registers the domain events of the terms
/// of their linears once per domain and direction. It routes the notifications to the members,
/// and propagates only the members that were notified.
#[derive(Clone, Debug, Default)]
pub struct HypercubeLinearStore {
    members: Vec<HypercubeLinearPropagator>,
    /// For every watched predicate, the members that watch it.
    predicate_watchers: KeyedVec<PredicateId, Vec<u32>>,
    /// For every domain event that the store is registered for, see [`event_key`], the members
    /// with a term that the event concerns. A member is added when it first watches its linear
    /// and is never removed; the notifications skip the members that do not watch their linear.
    event_watchers: Vec<Vec<u32>>,
    /// Whether a member was added to `event_watchers`.
    is_in_event_watchers: Vec<bool>,
    /// The members that were notified since they were last propagated.
    to_propagate: Vec<u32>,
    is_to_propagate: Vec<bool>,
}

/// The key of the domain event that raises the lower bound of `term`: the lower bound of its
/// domain for a positive weight and the upper bound for a negative weight. It is the local id with
/// which the store registers for the event.
fn event_key(term: AffineView<DomainId>) -> usize {
    term.inner.id() as usize * 2 + usize::from(term.scale < 0)
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
            self.watch_predicate(predicate_id, index);
        }

        self.members.push(member);
        self.is_to_propagate.push(false);
        self.is_in_event_watchers.push(false);
        let _ = self.schedule(index);
    }

    fn watch_predicate(&mut self, predicate_id: PredicateId, index: u32) {
        self.predicate_watchers.accomodate(predicate_id, vec![]);
        self.predicate_watchers[predicate_id].push(index);
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
        for &predicate_id in before.iter().filter(|p| !after.contains(p)) {
            let members = &mut self.predicate_watchers[predicate_id];
            let position = members
                .iter()
                .position(|&member| member == index)
                .expect("the member watches the predicate");
            let _ = members.swap_remove(position);
        }

        for &predicate_id in after.iter().filter(|p| !before.contains(p)) {
            self.watch_predicate(predicate_id, index);
        }
    }

    /// Adds the member at `index` to the watchers of the domain events of its terms, and registers
    /// the store for the events that no member watched before.
    fn add_to_event_watchers(&mut self, index: u32, context: &mut PropagationContext<'_>) {
        self.is_in_event_watchers[index as usize] = true;

        for term in self.members[index as usize].linear().terms() {
            let key = event_key(term);
            if key >= self.event_watchers.len() {
                self.event_watchers.resize(key + 1, vec![]);
            }

            if self.event_watchers[key].is_empty() {
                let events = if term.scale > 0 {
                    DomainEvents::LOWER_BOUND
                } else {
                    DomainEvents::UPPER_BOUND
                };
                let local_id = LocalId::from(u32::try_from(key).expect("fewer than 2^31 domains"));
                context.register_domain_event(term.inner, events, local_id);
            }

            self.event_watchers[key].push(index);
        }
    }

    /// Schedules the members in `members` that watch their linear, if `only_watching_linear`, or
    /// all of them otherwise.
    fn schedule_all(&mut self, members: &[u32], only_watching_linear: bool) -> EnqueueDecision {
        let mut scheduled = false;
        for &member in members {
            if !only_watching_linear || self.members[member as usize].is_watching_linear() {
                scheduled |= self.schedule(member);
            }
        }

        if scheduled || !self.to_propagate.is_empty() {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
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
        // The local id of a domain event is its key. The list of members is taken out while they
        // are scheduled, which does not change it.
        let key = local_id.unpack() as usize;
        let members = std::mem::take(&mut self.event_watchers[key]);
        let decision = self.schedule_all(&members, true);
        self.event_watchers[key] = members;
        decision
    }

    fn notify_predicate_id_satisfied(
        &mut self,
        _context: NotificationContext,
        predicate_id: PredicateId,
    ) -> EnqueueDecision {
        let Some(members) = self.predicate_watchers.get_mut(predicate_id) else {
            return self.schedule_all(&[], false);
        };

        let members = std::mem::take(members);
        let decision = self.schedule_all(&members, false);
        self.predicate_watchers[predicate_id] = members;
        decision
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

            if !self.is_in_event_watchers[index as usize]
                && self.members[index as usize].is_watching_linear()
            {
                self.add_to_event_watchers(index, &mut context);
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
        predicate: Predicate,
        context: ExplanationContext,
    ) -> Option<(Hypercube, LinearInequality, InferenceCode)> {
        self.members[member_index_of_lazy_code(code)]
            .explain_as_hypercube_linear(code, predicate, context)
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
