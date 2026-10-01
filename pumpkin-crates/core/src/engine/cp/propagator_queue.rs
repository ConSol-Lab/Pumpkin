use std::collections::VecDeque;

use crate::containers::KeyedVec;
use crate::propagation::Priority;
use crate::propagation::PropagatorId;
use crate::pumpkin_assert_moderate;

/// The number of different [`Priority`] levels.
const DEFAULT_NUM_PRIORITY_LEVELS: usize = Priority::Lowest as usize + 1;

/// A queue of propagators which supports `NUM_PRIORITY_LEVELS` different priority levels (at most
/// 64), where propagators with a [`Priority`] closer to [`Priority::High`] are popped first.
#[derive(Debug, Clone)]
pub(crate) struct PropagatorQueue<const NUM_PRIORITY_LEVELS: usize = DEFAULT_NUM_PRIORITY_LEVELS> {
    /// For every [`Priority`], the propagators which are enqueued with that priority.
    queues: [VecDeque<PropagatorId>; NUM_PRIORITY_LEVELS],
    is_enqueued: KeyedVec<PropagatorId, bool>,
    num_enqueued: usize,
    /// A bitmask where bit `i` is set if and only if `queues[i]` is not empty.
    present_priorities: u64,
}

impl<const NUM_PRIORITY_LEVELS: usize> Default for PropagatorQueue<NUM_PRIORITY_LEVELS> {
    fn default() -> Self {
        const {
            assert!(
                NUM_PRIORITY_LEVELS <= u64::BITS as usize,
                "the bitmask of present priorities supports at most 64 priority levels"
            )
        };

        PropagatorQueue {
            queues: std::array::from_fn(|_| VecDeque::new()),
            is_enqueued: KeyedVec::default(),
            num_enqueued: 0,
            present_priorities: 0,
        }
    }
}

impl<const NUM_PRIORITY_LEVELS: usize> PropagatorQueue<NUM_PRIORITY_LEVELS> {
    pub(crate) fn is_empty(&self) -> bool {
        self.num_enqueued == 0
    }

    pub(crate) fn enqueue_propagator(&mut self, propagator_id: PropagatorId, priority: Priority) {
        pumpkin_assert_moderate!((priority as usize) < NUM_PRIORITY_LEVELS);

        self.is_enqueued.accomodate(propagator_id, false);
        if !self.is_enqueued[propagator_id] {
            self.is_enqueued[propagator_id] = true;
            self.num_enqueued += 1;

            self.present_priorities |= 1 << priority as u64;
            self.queues[priority as usize].push_back(propagator_id);
        }
    }

    pub(crate) fn pop(&mut self) -> Option<PropagatorId> {
        if self.present_priorities == 0 {
            return None;
        }

        // The lowest set bit corresponds to the highest priority which has enqueued propagators.
        let top_priority = self.present_priorities.trailing_zeros() as usize;
        pumpkin_assert_moderate!(!self.queues[top_priority].is_empty());

        let propagator_id = self.queues[top_priority].pop_front()?;
        self.is_enqueued[propagator_id] = false;

        if self.queues[top_priority].is_empty() {
            self.present_priorities &= !(1 << top_priority);
        }

        self.num_enqueued -= 1;

        Some(propagator_id)
    }

    pub(crate) fn clear(&mut self) {
        // Only the enqueued propagators need to be reset, rather than all propagators.
        for queue in self.queues.iter_mut() {
            for propagator_id in queue.drain(..) {
                self.is_enqueued[propagator_id] = false;
            }
        }

        self.present_priorities = 0;
        self.num_enqueued = 0;
    }

    pub(crate) fn is_propagator_enqueued(&self, propagator_id: PropagatorId) -> bool {
        self.is_enqueued
            .get(propagator_id)
            .copied()
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::PropagatorQueue;
    use crate::propagation::Priority;
    use crate::state::PropagatorId;

    #[test]
    fn test_ordering() {
        let mut queue: PropagatorQueue = PropagatorQueue::default();

        queue.enqueue_propagator(PropagatorId(1), Priority::High);
        queue.enqueue_propagator(PropagatorId(0), Priority::Medium);
        queue.enqueue_propagator(PropagatorId(3), Priority::VeryLow);
        queue.enqueue_propagator(PropagatorId(4), Priority::Low);

        assert_eq!(PropagatorId(1), queue.pop().unwrap());
        assert_eq!(PropagatorId(0), queue.pop().unwrap());
        assert_eq!(PropagatorId(4), queue.pop().unwrap());
        assert_eq!(PropagatorId(3), queue.pop().unwrap());
        assert_eq!(None, queue.pop());
    }

    #[test]
    fn custom_number_of_priority_levels() {
        let mut queue = PropagatorQueue::<2>::default();

        queue.enqueue_propagator(PropagatorId(0), Priority::Medium);
        queue.enqueue_propagator(PropagatorId(1), Priority::High);

        assert_eq!(PropagatorId(1), queue.pop().unwrap());
        assert_eq!(PropagatorId(0), queue.pop().unwrap());
        assert_eq!(None, queue.pop());
    }

    #[test]
    fn clear_resets_enqueued_propagators() {
        let mut queue: PropagatorQueue = PropagatorQueue::default();

        queue.enqueue_propagator(PropagatorId(2), Priority::Lowest);
        queue.enqueue_propagator(PropagatorId(0), Priority::UltraLow);
        queue.enqueue_propagator(PropagatorId(1), Priority::High);

        queue.clear();

        assert!(queue.is_empty());
        assert!(!queue.is_propagator_enqueued(PropagatorId(0)));
        assert!(!queue.is_propagator_enqueued(PropagatorId(1)));
        assert!(!queue.is_propagator_enqueued(PropagatorId(2)));
        assert_eq!(None, queue.pop());

        queue.enqueue_propagator(PropagatorId(2), Priority::Lowest);
        queue.enqueue_propagator(PropagatorId(0), Priority::UltraLow);

        assert_eq!(PropagatorId(0), queue.pop().unwrap());
        assert_eq!(PropagatorId(2), queue.pop().unwrap());
        assert_eq!(None, queue.pop());
    }
}
