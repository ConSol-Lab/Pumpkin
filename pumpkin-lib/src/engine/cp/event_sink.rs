use enumset::EnumSet;

use super::IntDomainEvent;
use crate::basic_types::KeyedVec;
use crate::engine::variables::DomainId;
#[cfg(doc)]
use crate::engine::DomainEvents;
#[cfg(doc)]
use crate::propagators;

/// While a propagator runs (see [`propagators`]), the propagations it performs
/// are captured as events in the event sink. When the propagator finishes, the event sink is
/// drained to notify all the propagators that subscribe to those [`IntDomainEvent`].
///
/// Triggering any [`DomainEvents`] will also trigger the event [`DomainEvents::ANY_INT`].
///
/// The event sink will ensure duplicate events are ignored.
#[derive(Clone, Debug, Default)]
pub(crate) struct EventSink {
    present: KeyedVec<DomainId, EnumSet<IntDomainEvent>>,
    events: Vec<(IntDomainEvent, DomainId)>,
}

impl EventSink {
    pub(crate) fn new(num_domains: usize) -> Self {
        let mut event_sink = EventSink::default();
        for _ in 0..num_domains {
            event_sink.grow();
        }
        event_sink
    }
    pub(crate) fn grow(&mut self) {
        self.present.push(EnumSet::new());
    }

    pub(crate) fn event_occurred(&mut self, event: IntDomainEvent, domain: DomainId) {
        let elem = &mut self.present[domain];

        if elem.insert(event) {
            self.events.push((event, domain));
        }
    }

    pub(crate) fn drain(&mut self) -> impl Iterator<Item = (IntDomainEvent, DomainId)> + '_ {
        self.events.drain(..).inspect(|&(event, domain)| {
            let _ = self.present[domain].remove(event);
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::IntDomainEvent;

    #[test]
    fn the_default_sink_is_empty() {
        let mut sink = EventSink::default();

        let events = sink.drain().collect::<Vec<_>>();
        assert!(events.is_empty());
    }

    #[test]
    fn a_captured_event_is_observed_in_the_drain() {
        let mut sink = EventSink::default();
        sink.grow();
        sink.grow();

        sink.event_occurred(IntDomainEvent::LowerBound, DomainId::new(0));
        sink.event_occurred(IntDomainEvent::UpperBound, DomainId::new(1));

        let events = sink.drain().collect::<Vec<_>>();

        assert_eq!(events.len(), 2);
        assert!(events.contains(&(IntDomainEvent::LowerBound, DomainId::new(0))));
        assert!(events.contains(&(IntDomainEvent::UpperBound, DomainId::new(1))));
    }

    #[test]
    fn after_draining_the_event_sink_is_empty() {
        let mut sink = EventSink::default();
        sink.grow();
        sink.grow();

        sink.event_occurred(IntDomainEvent::LowerBound, DomainId::new(0));
        sink.event_occurred(IntDomainEvent::UpperBound, DomainId::new(1));

        let _ = sink.drain().collect::<Vec<_>>();

        let events = sink.drain().collect::<Vec<_>>();
        assert!(events.is_empty());
    }

    #[test]
    fn duplicate_events_are_ignored() {
        let mut sink = EventSink::default();
        sink.grow();

        sink.event_occurred(IntDomainEvent::LowerBound, DomainId::new(0));
        sink.event_occurred(IntDomainEvent::LowerBound, DomainId::new(0));

        let events = sink.drain().collect::<Vec<_>>();

        assert_eq!(events.len(), 1);
    }
}
