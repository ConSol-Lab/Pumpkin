use crate::basic_types::KeyValueHeap;
use crate::basic_types::PropositionalVariable;
use crate::basic_types::StorageKey;
use crate::engine::sat::AssignmentsPropositional;

#[derive(Debug)]
pub struct PropositionalVariableSelector {
    heap: KeyValueHeap,
    increment: f64,
    max_threshold: f64,
    decay_factor: f64,
}

impl Default for PropositionalVariableSelector {
    fn default() -> Self {
        PropositionalVariableSelector {
            heap: KeyValueHeap::new(),
            increment: 1.0,
            max_threshold: 1e100,
            decay_factor: 0.95,
        }
    }
}

impl PropositionalVariableSelector {
    pub fn reset(&mut self, random_seed: i64) {
        self.heap.reset(random_seed);
        self.increment = 1.0;
    }

    pub fn bump_activity(&mut self, variable: PropositionalVariable) {
        // scale the activities if the values are too large
        let activity = self.heap.get_value(variable.index() as u32);
        if activity + self.increment >= self.max_threshold {
            self.heap.divide_values(self.max_threshold);
            self.increment /= self.max_threshold;
        }
        // now perform the standard bumping
        self.heap.increment(variable.index() as u32, self.increment);
    }

    pub fn restore(&mut self, variable: PropositionalVariable) {
        self.heap.restore_key(variable.index() as u32);
    }

    pub fn decay_activities(&mut self) {
        // note that decaying activities is implemented as increasing the 'increment'
        //  so that future bumps are more impactful
        //  this is cheaper than dividing each activity value
        self.increment *= 1.0 / self.decay_factor;
    }

    pub fn grow(&mut self) {
        self.heap.grow(0.0);
    }

    pub fn peek_next_variable(
        &mut self,
        assignments: &AssignmentsPropositional,
    ) -> Option<PropositionalVariable> {
        loop {
            if let Some(candidate_variable) = self.heap.peek_max().map(PropositionalVariable::new) {
                // note that some variables on the heap may already be assigned because a lazy data
                // structure is used  in case an assigned variable is next, remove
                // it from the heap, and loop again
                if assignments.is_variable_assigned(candidate_variable) {
                    let _ = self.heap.pop_max();
                } else {
                    return Some(candidate_variable);
                }
            // otherwise there are no variables left in the heap
            } else {
                return None;
            }
        }
    }
}
