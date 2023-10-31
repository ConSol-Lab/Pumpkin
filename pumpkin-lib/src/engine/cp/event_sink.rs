use crate::basic_types::DomainId;
use enumset::EnumSet;

use super::DomainEvent;

/// While a propagator runs, the propagations it performs are captured as events in the event sink.
/// When the propagator finishes, the event sink is drained to notify all the propagators that
/// subscribe to those events.
///
/// Triggering any [`DomainEvent`] will also trigger the event [`DomainEvent::Any`].
///
/// The event sink will ensure duplicate events are ignored.
#[derive(Default, Clone)]
pub struct EventSink {
    present: Vec<EnumSet<DomainEvent>>,
    events: Vec<(DomainEvent, DomainId)>,
}

impl EventSink {
    pub fn grow(&mut self) {
        self.present.push(EnumSet::new());
    }

    pub fn event_occurred(&mut self, event: DomainEvent, domain: DomainId) {
        let elem = &mut self.present[domain];

        if elem.contains(event) {
            // The event was already triggered.
            return;
        }

        if event != DomainEvent::Any && elem.is_empty() {
            // Ensure the any event always is triggered when a modification happens.
            self.events.push((DomainEvent::Any, domain));
        }

        elem.insert_all(event | DomainEvent::Any);
        self.events.push((event, domain));
    }

    pub fn drain(&mut self) -> impl Iterator<Item = (DomainEvent, DomainId)> + '_ {
        self.events.drain(..).inspect(|&(event, domain)| {
            self.present[domain].remove(event);
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_default_sink_is_empty() {
        let mut sink = EventSink::default();

        let events = sink.drain().collect::<Vec<_>>();
        assert!(events.is_empty());
    }

    #[test]
    fn an_any_event_is_pushed_once() {
        let mut sink = EventSink::default();
        sink.grow();

        sink.event_occurred(DomainEvent::Any, DomainId::new(0));

        let events = sink.drain().collect::<Vec<_>>();
        assert_eq!(events.len(), 1);
        assert!(events.contains(&(DomainEvent::Any, DomainId::new(0))));
    }

    #[test]
    fn a_captured_event_is_observed_in_the_drain() {
        let mut sink = EventSink::default();
        sink.grow();
        sink.grow();

        sink.event_occurred(DomainEvent::LowerBound, DomainId::new(0));
        sink.event_occurred(DomainEvent::UpperBound, DomainId::new(1));

        let events = sink.drain().collect::<Vec<_>>();

        assert_eq!(events.len(), 4);
        assert!(events.contains(&(DomainEvent::LowerBound, DomainId::new(0))));
        assert!(events.contains(&(DomainEvent::Any, DomainId::new(0))));
        assert!(events.contains(&(DomainEvent::UpperBound, DomainId::new(1))));
        assert!(events.contains(&(DomainEvent::Any, DomainId::new(1))));
    }

    #[test]
    fn after_draining_the_event_sink_is_empty() {
        let mut sink = EventSink::default();
        sink.grow();
        sink.grow();

        sink.event_occurred(DomainEvent::LowerBound, DomainId::new(0));
        sink.event_occurred(DomainEvent::UpperBound, DomainId::new(1));

        let _ = sink.drain().collect::<Vec<_>>();

        let events = sink.drain().collect::<Vec<_>>();
        assert!(events.is_empty());
    }

    #[test]
    fn duplicate_events_are_ignored() {
        let mut sink = EventSink::default();
        sink.grow();

        sink.event_occurred(DomainEvent::LowerBound, DomainId::new(0));
        sink.event_occurred(DomainEvent::LowerBound, DomainId::new(0));

        let events = sink.drain().collect::<Vec<_>>();

        assert_eq!(events.len(), 2);
    }
}
