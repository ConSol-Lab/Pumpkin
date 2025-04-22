use super::TrailedInteger;

#[derive(Debug, Clone)]
pub(crate) struct TrailedChange {
    pub(crate) old_value: i64,
    pub(crate) reference: TrailedInteger,
}
