use crate::basic_types::DomainId;

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
    events: Vec<u8>,
}

impl EventSink {
    pub fn grow(&mut self) {
        self.events.push(0);
    }

    pub fn event_occurred(&mut self, event: DomainEvent, domain: DomainId) {
        let elem = &mut self.events[domain];

        *elem = (*elem | event as u8) | DomainEvent::Any as u8;
    }

    pub fn drain(&mut self) -> impl Iterator<Item = (DomainEvent, DomainId)> + '_ {
        EventSinkIter {
            events: &mut self.events,
            domain_idx: 0,
            event_idx: 0,
        }
    }
}

pub struct EventSinkIter<'a> {
    events: &'a mut Vec<u8>,
    domain_idx: u32,
    event_idx: u32,
}

impl<'a> Iterator for EventSinkIter<'a> {
    type Item = (DomainEvent, DomainId);

    fn next(&mut self) -> Option<Self::Item> {
        while self.domain_idx < self.events.len() as u32 {
            let domain = DomainId {
                id: self.domain_idx,
            };

            let elem = self.events[domain];
            let mut event_bit = 1u8 << self.event_idx;

            while (elem & event_bit) != event_bit && event_bit <= DomainEvent::MAX {
                self.event_idx += 1;
                event_bit <<= 1;
            }

            if event_bit <= DomainEvent::MAX {
                // Safety: Event bit cannot be larger than DomainEvent::MAX.
                let event: DomainEvent = unsafe { std::mem::transmute(event_bit) };
                self.event_idx += 1;

                return Some((event, domain));
            }

            self.events[domain] = 0;
            self.domain_idx += 1;
            self.event_idx = 0;
        }

        None
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
