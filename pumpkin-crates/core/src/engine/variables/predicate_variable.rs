//! A [`Predicate`] can be used as a Boolean variable which is true if and only if the predicate
//! is true.
//!
//! When used as an [`IntegerVariable`], a predicate is a 0-1 variable which is 1 if the predicate
//! is true and 0 if the predicate is false.
//!
//! As for a 0-1 domain, a propagator registered for events on a predicate is notified of the
//! lower-bound and assign events when the predicate becomes true, and of the upper-bound and assign
//! events when it becomes false; each registered event is triggered once. Similarly, each
//! registered event is triggered once on backtracking when the predicate becomes unassigned.

use enumset::EnumSet;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use super::IntegerVariable;
use super::TransformableVariable;
use crate::engine::Assignments;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::variables::AffineView;
use crate::propagation::EventDispatcher;
use crate::propagation::EventTarget;
use crate::propagation::LocalId;

/// Returns whether `value` is in the 0-1 domain of a predicate with the given truth value.
fn truth_value_contains(truth_value: Option<bool>, value: i32) -> bool {
    match value {
        0 => truth_value != Some(true),
        1 => truth_value != Some(false),
        _ => false,
    }
}

impl EventTarget for Predicate {
    fn register(
        &self,
        registration: &mut impl EventDispatcher,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    ) {
        registration.register_predicate_events(*self, events, local_id);
    }
}

impl CheckerVariable<Predicate> for Predicate {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        atomic.get_domain() == self.get_domain()
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        self.upper_bound_predicate(value)
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        self.lower_bound_predicate(value)
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        self.disequality_predicate(value)
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        self.equality_predicate(value)
    }

    fn induced_lower_bound(&self, domains: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(domains.is_true(self) as i32)
    }

    fn induced_upper_bound(&self, domains: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(!domains.is_true(&!*self) as i32)
    }

    fn induced_fixed_value(&self, domains: &VariableState<Predicate>) -> Option<i32> {
        if domains.is_true(self) {
            Some(1)
        } else if domains.is_true(&!*self) {
            Some(0)
        } else {
            None
        }
    }

    fn induced_domain_contains(&self, domains: &VariableState<Predicate>, value: i32) -> bool {
        let truth_value = self
            .induced_fixed_value(domains)
            .map(|fixed_value| fixed_value == 1);
        truth_value_contains(truth_value, value)
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        _domains: &'state VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        std::iter::empty()
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        domains: &'state VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        Some((0..=1).filter(|&value| self.induced_domain_contains(domains, value)))
    }
}

impl IntegerVariable for Predicate {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    /// Predicates which are true have a lower bound of 1.
    /// Predicates which are false have a lower bound of 0.
    /// Unassigned predicates have a lower bound of 0.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        (assignment.evaluate_predicate(*self) == Some(true)) as i32
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        (assignment.evaluate_predicate_at_trail_position(*self, trail_position) == Some(true))
            as i32
    }

    /// Returns the upper bound represented as a 0-1 value.
    /// Predicates which are true have an upper bound of 1.
    /// Predicates which are false have a upper bound of 0.
    /// Unassigned predicates have a upper bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        (assignment.evaluate_predicate(*self) != Some(false)) as i32
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        (assignment.evaluate_predicate_at_trail_position(*self, trail_position) != Some(false))
            as i32
    }

    /// Returns whether the input value, when interpreted as a bool,
    /// can be considered for the predicate.
    /// Predicates which are true only contain value 1.
    /// Predicates which are false only contain value 0.
    /// Unassigned predicates contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        truth_value_contains(assignment.evaluate_predicate(*self), value)
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        truth_value_contains(
            assignment.evaluate_predicate_at_trail_position(*self, trail_position),
            value,
        )
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        let truth_value = assignment.evaluate_predicate(*self);
        (0..=1).filter(move |&value| truth_value_contains(truth_value, value))
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        watchers.unwatch_predicate_events(*self)
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        event.unwrap()
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_predicate_events_backtrack(*self, events)
    }

    fn get_holes_at_current_checkpoint(
        &self,
        _assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }

    fn get_holes(&self, _assignments: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }
}

impl PredicateConstructor for Predicate {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            ..=0 => Predicate::trivially_true(),
            1 => *self,
            _ => Predicate::trivially_false(),
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            1.. => Predicate::trivially_true(),
            0 => !*self,
            _ => Predicate::trivially_false(),
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            1 => *self,
            0 => !*self,
            _ => Predicate::trivially_false(),
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        !self.equality_predicate(bound)
    }
}

impl TransformableVariable<AffineView<Predicate>> for Predicate {
    fn scaled(&self, scale: i32) -> AffineView<Predicate> {
        AffineView::new(*self, scale, 0)
    }

    fn offset(&self, offset: i32) -> AffineView<Predicate> {
        AffineView::new(*self, 1, offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::PropagationStatusCP;
    use crate::engine::State;
    use crate::predicate;
    use crate::propagation::DomainEvents;
    use crate::propagation::Domains;
    use crate::propagation::EnqueueDecision;
    use crate::propagation::EventsToRegister;
    use crate::propagation::NotificationContext;
    use crate::propagation::PropagationContext;
    use crate::propagation::Propagator;
    use crate::propagation::PropagatorConstructor;
    use crate::propagation::PropagatorConstructorContext;
    use crate::propagation::PropagatorSpec;
    use crate::propagation::RuntimeCheckers;
    use crate::state::PropagatorHandle;

    #[test]
    fn bounds_follow_the_truth_value_of_the_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        assert_eq!(predicate.lower_bound(&state.assignments), 0);
        assert_eq!(predicate.upper_bound(&state.assignments), 1);
        assert_eq!(
            predicate
                .iterate_domain(&state.assignments)
                .collect::<Vec<_>>(),
            vec![0, 1]
        );

        let _ = state.post(predicate![x >= 7]).expect("non-empty domain");

        assert_eq!(predicate.lower_bound(&state.assignments), 1);
        assert_eq!(predicate.upper_bound(&state.assignments), 1);
        assert!(!predicate.contains(&state.assignments, 0));
        assert!((!predicate).contains(&state.assignments, 0));
        assert_eq!((!predicate).upper_bound(&state.assignments), 0);
    }

    #[test]
    fn bounds_at_trail_position_use_the_old_truth_value() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x != 5];

        let trail_position = state.assignments.num_trail_entries() - 1;
        state.new_checkpoint();
        let _ = state.post(predicate![x <= 4]).expect("non-empty domain");

        assert_eq!(predicate.lower_bound(&state.assignments), 1);
        assert_eq!(
            predicate.lower_bound_at_trail_position(&state.assignments, trail_position),
            0
        );
        assert!(predicate.contains_at_trail_position(&state.assignments, 0, trail_position));
    }

    #[test]
    fn negation_is_canonical() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);

        for predicate in [
            predicate![x >= 3],
            predicate![x <= 3],
            predicate![x == 3],
            predicate![x != 3],
            Predicate::trivially_true(),
        ] {
            assert_eq!(!!predicate, predicate);
        }
    }

    #[test]
    fn predicates_over_the_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let p = predicate![x >= 3];

        assert_eq!(predicate![p >= 0], Predicate::trivially_true());
        assert_eq!(predicate![p >= 1], p);
        assert_eq!(predicate![p >= 2], Predicate::trivially_false());
        assert_eq!(predicate![p <= 1], Predicate::trivially_true());
        assert_eq!(predicate![p <= 0], !p);
        assert_eq!(predicate![p <= -1], Predicate::trivially_false());
        assert_eq!(predicate![p == 1], p);
        assert_eq!(predicate![p == 0], !p);
        assert_eq!(predicate![p == 2], Predicate::trivially_false());
        assert_eq!(predicate![p != 1], !p);
        assert_eq!(predicate![p != 0], p);
        assert_eq!(predicate![p != 2], Predicate::trivially_true());
    }

    /// Records the events and backtrack events it receives for its predicate.
    #[derive(Clone, Debug)]
    struct EventRecorder {
        predicate: Predicate,
        registered_events: DomainEvents,
        events: Vec<DomainEvent>,
        backtrack_events: Vec<DomainEvent>,
    }

    impl PropagatorConstructor for EventRecorder {
        type PropagatorImpl = Self;

        fn create(self, mut context: PropagatorConstructorContext) -> PropagatorSpec<Self> {
            context.register_backtrack(self.predicate, self.registered_events, LocalId::from(0));

            PropagatorSpec {
                registration: EventsToRegister::builder()
                    .add(&self.predicate, self.registered_events, LocalId::from(0))
                    .build(),
                checkers: RuntimeCheckers::empty(),
                propagator: self,
            }
        }
    }

    impl Propagator for EventRecorder {
        fn name(&self) -> &str {
            "EventRecorder"
        }

        fn propagate_from_scratch(&self, _context: PropagationContext) -> PropagationStatusCP {
            Ok(())
        }

        fn notify(
            &mut self,
            _context: NotificationContext,
            _local_id: LocalId,
            event: OpaqueDomainEvent,
        ) -> EnqueueDecision {
            self.events.push(self.predicate.unpack_event(event));
            EnqueueDecision::Skip
        }

        fn notify_backtrack(
            &mut self,
            _context: Domains,
            _local_id: LocalId,
            event: OpaqueDomainEvent,
        ) {
            self.backtrack_events
                .push(self.predicate.unpack_event(event));
        }
    }

    /// Posts each predicate of `to_post` at its own checkpoint, backtracks to the root, and returns
    /// the events and backtrack events received for `predicate`, which is registered for the given
    /// events.
    fn record_events(
        predicate: Predicate,
        registered_events: DomainEvents,
        state: &mut State,
        to_post: &[Predicate],
    ) -> (Vec<DomainEvent>, Vec<DomainEvent>) {
        let handle: PropagatorHandle<EventRecorder> = state.add_propagator(EventRecorder {
            predicate,
            registered_events,
            events: vec![],
            backtrack_events: vec![],
        });

        state.propagate_to_fixed_point().expect("no conflict");
        for &posted in to_post {
            state.new_checkpoint();
            let _ = state.post(posted).expect("non-empty domain");
            state.propagate_to_fixed_point().expect("no conflict");
        }
        let _ = state.restore_to(0);

        let recorder = state.get_propagator(handle).expect("propagator exists");
        (recorder.events.clone(), recorder.backtrack_events.clone())
    }

    #[test]
    fn single_events_when_predicate_becomes_true_and_is_undone() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[
                predicate![x >= 3],
                predicate![x >= 6],
                predicate![x <= 8],
                predicate![x >= 7],
            ],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::LowerBound], vec![DomainEvent::LowerBound])
        );
    }

    #[test]
    fn single_events_when_predicate_becomes_false_and_is_undone() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x <= 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[predicate![x >= 7], predicate![x >= 8]],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::UpperBound], vec![DomainEvent::UpperBound])
        );
    }

    #[test]
    fn single_events_for_equality_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x == 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[predicate![x != 4], predicate![x >= 5], predicate![x <= 5]],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::LowerBound], vec![DomainEvent::LowerBound])
        );
    }

    #[test]
    fn no_events_when_predicate_stays_unassigned() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[predicate![x >= 3], predicate![x <= 8]],
        );

        assert_eq!(events, (vec![], vec![]));
    }

    #[test]
    fn assign_events_are_triggered_alongside_bound_events() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::ANY_INT,
            &mut state,
            &[predicate![x >= 3], predicate![x >= 6]],
        );

        assert_eq!(
            events,
            (
                vec![DomainEvent::LowerBound, DomainEvent::Assign],
                vec![DomainEvent::LowerBound, DomainEvent::Assign]
            )
        );
    }

    #[test]
    fn only_assign_events_are_triggered_when_only_assign_is_registered() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::ASSIGN,
            &mut state,
            &[predicate![x <= 4]],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::Assign], vec![DomainEvent::Assign])
        );
    }

    #[test]
    fn no_events_for_predicate_which_is_fixed_at_registration() {
        // `[x != 2]` is true at the root, and its negation `[x == 2]` is false at the root.
        for negate in [false, true] {
            let mut state = State::default();
            let x = state.new_sparse_variable(vec![1, 3], None);
            let predicate = predicate![x != 2];
            let predicate = if negate { !predicate } else { predicate };

            let events = record_events(
                predicate,
                DomainEvents::ANY_INT,
                &mut state,
                &[predicate![x >= 3]],
            );

            assert_eq!(events, (vec![], vec![]), "negated: {negate}");
        }
    }

    #[test]
    fn no_events_for_predicate_which_is_fixed_at_the_root_when_registered_during_search() {
        // `[x != 2]` is true at the root, and its negation `[x == 2]` is false at the root.
        for negate in [false, true] {
            let mut state = State::default();
            let x = state.new_sparse_variable(vec![1, 3], None);
            let predicate = predicate![x != 2];
            let predicate = if negate { !predicate } else { predicate };
            state.new_checkpoint();

            let events = record_events(
                predicate,
                DomainEvents::ANY_INT,
                &mut state,
                &[predicate![x >= 3]],
            );

            assert_eq!(events, (vec![], vec![]), "negated: {negate}");
        }
    }
}
