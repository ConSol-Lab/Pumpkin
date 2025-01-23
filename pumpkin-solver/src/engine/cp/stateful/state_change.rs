use super::stateful_assignments::StatefulInteger;

#[derive(Debug, Clone)]
pub(crate) struct StateChange {
    pub(crate) old_value: i64,
    pub(crate) reference: StatefulInteger,
}
