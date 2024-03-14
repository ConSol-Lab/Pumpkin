use super::ConflictInfo;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum PropagationStatusOneStepCP {
    ConflictDetected { conflict_info: ConflictInfo },
    PropagationHappened,
    FixedPoint,
}

impl PropagationStatusOneStepCP {
    pub fn no_conflict(&self) -> bool {
        matches!(
            *self,
            PropagationStatusOneStepCP::PropagationHappened
                | PropagationStatusOneStepCP::FixedPoint
        )
    }

    // pub fn conflict_detected(&self) -> bool {
    // !self.no_conflict()
    // }
}
