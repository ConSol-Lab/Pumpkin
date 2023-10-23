use super::PropositionalConjunction;

pub enum PropagationStatusOneStepCP {
    ConflictDetected {
        propositional_conjunction: PropositionalConjunction,
    },
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

    /*pub fn conflict_detected(&self) -> bool {
        !self.no_conflict()
    }*/
}
