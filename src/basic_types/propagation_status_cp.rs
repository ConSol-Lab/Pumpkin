use super::PropositionalConjunction;

pub enum PropagationStatusCP {
    ConflictDetected {
        failure_reason: PropositionalConjunction,
    },
    NoConflictDetected,
}

impl PropagationStatusCP {
    pub fn no_conflict(&self) -> bool {
        matches!(*self, PropagationStatusCP::NoConflictDetected)
    }

    pub fn conflict_detected(&self) -> bool {
        !self.no_conflict()
    }
}
