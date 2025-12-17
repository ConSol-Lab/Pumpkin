use super::TrailedChange;
use super::TrailedInteger;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;

#[derive(Default, Debug, Clone)]
pub(crate) struct TrailedValues {
    trail: Trail<TrailedChange>,
    values: KeyedVec<TrailedInteger, i64>,
}

impl TrailedValues {
    pub(crate) fn grow(&mut self, initial_value: i64) -> TrailedInteger {
        self.values.push(initial_value)
    }

    pub(crate) fn new_checkpoint(&mut self) {
        self.trail.new_checkpoint()
    }

    pub(crate) fn read(&self, trailed_integer: TrailedInteger) -> i64 {
        self.values[trailed_integer]
    }

    pub(crate) fn synchronise(&mut self, new_checkpoint: usize) {
        self.trail
            .synchronise(new_checkpoint)
            .for_each(|state_change| self.values[state_change.reference] = state_change.old_value)
    }

    fn write(&mut self, trailed_integer: TrailedInteger, value: i64) {
        let old_value = self.values[trailed_integer];
        if old_value == value {
            return;
        }
        let entry = TrailedChange {
            old_value,
            reference: trailed_integer,
        };
        self.trail.push(entry);
        self.values[trailed_integer] = value;
    }

    #[cfg(test)]
    pub(crate) fn add_assign(&mut self, trailed_integer: TrailedInteger, addition: i64) {
        self.write(trailed_integer, self.values[trailed_integer] + addition);
    }

    pub(crate) fn assign(&mut self, trailed_integer: TrailedInteger, value: i64) {
        self.write(trailed_integer, value);
    }

    pub(crate) fn debug_create_empty_clone(&self) -> Self {
        let mut new_trail = self.trail.clone();
        let mut new_values = self.values.clone();
        if new_trail.get_checkpoint() > 0 {
            new_trail.synchronise(0).for_each(|state_change| {
                new_values[state_change.reference] = state_change.old_value
            });
        }
        Self {
            trail: new_trail,
            values: new_values,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::TrailedValues;

    #[test]
    fn test_write_resets() {
        let mut assignments = TrailedValues::default();
        let trailed_integer = assignments.grow(0);

        assert_eq!(assignments.read(trailed_integer), 0);

        assignments.new_checkpoint();
        assignments.add_assign(trailed_integer, 5);

        assert_eq!(assignments.read(trailed_integer), 5);

        assignments.add_assign(trailed_integer, 5);
        assert_eq!(assignments.read(trailed_integer), 10);

        assignments.new_checkpoint();
        assignments.add_assign(trailed_integer, 1);

        assert_eq!(assignments.read(trailed_integer), 11);

        assignments.synchronise(1);
        assert_eq!(assignments.read(trailed_integer), 10);

        assignments.synchronise(0);
        assert_eq!(assignments.read(trailed_integer), 0);
    }
}
