use super::TrailedInt;

#[derive(Debug, Clone)]
pub(crate) struct TrailedChange {
    pub(crate) old_value: i64,
    pub(crate) reference: TrailedInt,
}
