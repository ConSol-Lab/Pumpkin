use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::VecDeque;

use crate::basic_types::HashSet;
use crate::engine::cp::propagation::PropagatorId;
use crate::pumpkin_assert_moderate;

#[derive(Debug)]
pub(crate) struct PropagatorQueue {
    queues: Vec<VecDeque<PropagatorId>>,
    present_propagators: HashSet<PropagatorId>,
    present_priorities: BinaryHeap<Reverse<u32>>,
}

impl PropagatorQueue {
    pub(crate) fn new(num_priority_levels: u32) -> PropagatorQueue {
        PropagatorQueue {
            queues: vec![VecDeque::new(); num_priority_levels as usize],
            present_propagators: HashSet::default(),
            present_priorities: BinaryHeap::new(),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.present_propagators.is_empty()
    }

    pub(crate) fn enqueue_propagator(&mut self, propagator_id: PropagatorId, priority: u32) {
        pumpkin_assert_moderate!((priority as usize) < self.queues.len());

        if !self.is_propagator_enqueued(propagator_id) {
            if self.queues[priority as usize].is_empty() {
                self.present_priorities.push(Reverse(priority));
            }
            self.queues[priority as usize].push_back(propagator_id);
            let _ = self.present_propagators.insert(propagator_id);
        }
    }

    pub(crate) fn pop(&mut self) -> PropagatorId {
        pumpkin_assert_moderate!(!self.is_empty());

        let top_priority = self.present_priorities.peek().unwrap().0 as usize;
        pumpkin_assert_moderate!(!self.queues[top_priority].is_empty());

        let next_propagator_id = self.queues[top_priority].pop_front().unwrap();

        let _ = self.present_propagators.remove(&next_propagator_id);

        if self.queues[top_priority].is_empty() {
            let _ = self.present_priorities.pop();
        }

        next_propagator_id
    }

    pub(crate) fn clear(&mut self) {
        while !self.present_priorities.is_empty() {
            let priority = self.present_priorities.pop().unwrap().0 as usize;
            pumpkin_assert_moderate!(!self.queues[priority].is_empty());
            self.queues[priority].clear();
        }
        self.present_propagators.clear();
        self.present_priorities.clear();
    }

    fn is_propagator_enqueued(&self, propagator_id: PropagatorId) -> bool {
        self.present_propagators.contains(&propagator_id)
    }
}
