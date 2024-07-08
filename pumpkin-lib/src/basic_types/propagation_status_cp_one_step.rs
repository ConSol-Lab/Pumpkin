use super::conflict_info::StoredConflictInfo;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum PropagationStatusOneStepCP {
    ConflictDetected { conflict_info: StoredConflictInfo },
    PropagationHappened,
    FixedPoint,
}
