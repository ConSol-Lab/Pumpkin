pub enum PropagationStatusClausal {
    ConflictDetected { reason_code: u32 },
    NoConflictDetected,
}

impl PropagationStatusClausal {
    pub fn no_conflict(&self) -> bool {
        matches!(*self, PropagationStatusClausal::NoConflictDetected)
    }

    pub fn conflict_detected(&self) -> bool {
        !self.no_conflict()
    }
}
