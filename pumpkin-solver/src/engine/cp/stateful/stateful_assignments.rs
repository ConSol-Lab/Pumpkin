use super::StateChange;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;

#[derive(Debug, Clone, Copy)]
pub(crate) struct StatefulInteger {
    id: u32,
}

impl Default for StatefulInteger {
    fn default() -> Self {
        Self { id: u32::MAX }
    }
}

impl StorageKey for StatefulInteger {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        Self { id: index as u32 }
    }
}

#[derive(Default, Debug, Clone)]
pub(crate) struct StatefulAssignments {
    trail: Trail<StateChange>,
    values: KeyedVec<StatefulInteger, i64>,
}

impl StatefulAssignments {
    pub(crate) fn grow(&mut self, initial_value: i64) -> StatefulInteger {
        self.values.push(initial_value)
    }

    pub(crate) fn increase_decision_level(&mut self) {
        self.trail.increase_decision_level()
    }

    pub(crate) fn read(&self, stateful_int: StatefulInteger) -> i64 {
        self.values[stateful_int]
    }

    pub(crate) fn synchronise(&mut self, new_decision_level: usize) {
        self.trail
            .synchronise(new_decision_level)
            .for_each(|state_change| self.values[state_change.reference] = state_change.old_value)
    }

    fn write(&mut self, stateful_int: StatefulInteger, value: i64) {
        let old_value = self.values[stateful_int];
        if old_value == value {
            return;
        }
        let entry = StateChange {
            old_value,
            reference: stateful_int,
        };
        self.trail.push(entry);
        self.values[stateful_int] = value;
    }

    pub(crate) fn add_assign(&mut self, stateful_int: StatefulInteger, addition: i64) {
        self.write(stateful_int, self.values[stateful_int] + addition);
    }

    pub(crate) fn assign(&mut self, stateful_int: StatefulInteger, value: i64) {
        self.write(stateful_int, value);
    }

    pub(crate) fn debug_create_empty_clone(&self) -> Self {
        let mut new_trail = self.trail.clone();
        let mut new_values = self.values.clone();
        new_trail
            .synchronise(0)
            .for_each(|state_change| new_values[state_change.reference] = state_change.old_value);
        Self {
            trail: new_trail,
            values: new_values,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::cp::stateful::stateful_assignments::StatefulAssignments;

    #[test]
    fn test_write_resets() {
        let mut assignments = StatefulAssignments::default();
        let trailed_int = assignments.grow(0);

        assert_eq!(assignments.read(trailed_int), 0);

        assignments.increase_decision_level();
        assignments.add_assign(trailed_int, 5);

        assert_eq!(assignments.read(trailed_int), 5);

        assignments.add_assign(trailed_int, 5);
        assert_eq!(assignments.read(trailed_int), 10);

        assignments.increase_decision_level();
        assignments.add_assign(trailed_int, 1);

        assert_eq!(assignments.read(trailed_int), 11);

        assignments.synchronise(1);
        assert_eq!(assignments.read(trailed_int), 10);

        assignments.synchronise(0);
        assert_eq!(assignments.read(trailed_int), 0);
    }
}
