use super::PropositionalConjunction;

pub enum PropagationStatusOneStepCP {
    ConflictDetected {
        failure_reason: PropositionalConjunction,
    },
    PropagationHappened,
    FixedPoint,
}

impl PropagationStatusOneStepCP {
    pub fn no_conflict(&self) -> bool {
        !matches!(
            *self,
            PropagationStatusOneStepCP::ConflictDetected { failure_reason: _ }
        )
    }

    /*pub fn conflict_detected(&self) -> bool {
        !self.no_conflict()
    }*/
}
