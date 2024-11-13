use super::NogoodId;

/// The watch list is specific to a domain id.
#[derive(Default, Clone, Debug)]
pub(crate) struct NogoodWatchList {
    /// Nogoods with a watched predicate [x >= k]
    pub(crate) lower_bound: Vec<NogoodWatcher>,
    /// Nogoods with a watched predicate [x <= k]
    pub(crate) upper_bound: Vec<NogoodWatcher>,
    /// Nogoods with a watched predicate [x != k]
    pub(crate) hole: Vec<NogoodWatcher>,
    /// Nogoods with a watched predicate [x == k]
    pub(crate) equals: Vec<NogoodWatcher>,
}

/// The watcher is with respect to a specific domain id and predicate type.
#[derive(Default, Clone, Copy, Debug)]
pub(crate) struct NogoodWatcher {
    /// This field represents the right-hand side of the predicate present in the nogood.
    ///
    /// It is used as an indicator to whether the nogood should be inspected.
    pub(crate) right_hand_side: i32,
    pub(crate) nogood_id: NogoodId,
}
