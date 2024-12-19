use super::NogoodId;

/// The watch list is specific to a domain id.
#[derive(Default, Clone, Debug)]
pub(crate) struct NogoodWatchList {
    pub(crate) watchers: Vec<NogoodId>,
}

impl NogoodWatchList {
    pub(crate) fn drain_watchers(&mut self) -> impl Iterator<Item = NogoodId> + use<'_> {
        self.watchers.drain(..)
    }

    pub(crate) fn add_watcher(&mut self, nogood_id: NogoodId) {
        self.watchers.push(nogood_id)
    }
}
