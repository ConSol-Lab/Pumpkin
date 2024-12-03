use super::NogoodId;
use crate::basic_types::HashSet;

/// The watch list is specific to a domain id.
#[derive(Default, Clone, Debug)]
pub(crate) struct NogoodWatchList {
    /// Nogoods with a watched predicate [x >= k]
    lower_bound: Vec<NogoodWatcher>,
    /// Nogoods with a watched predicate [x <= k]
    upper_bound: Vec<NogoodWatcher>,
    /// Nogoods with a watched predicate [x != k]
    hole: Vec<NogoodWatcher>,
    /// Nogoods with a watched predicate [x == k]
    equals: Vec<NogoodWatcher>,
}

impl NogoodWatchList {
    fn find_and_remove_watcher(watch_list: &mut Vec<NogoodWatcher>, id: NogoodId, value: i32) {
        let position = watch_list
            .iter()
            .position(|w| w.right_hand_side == value && w.nogood_id == id)
            .expect("NogoodWatcher must be present.");
        let _ = watch_list.swap_remove(position);
    }

    pub(crate) fn ratio_unique_elements(&self) -> f64 {
        let total =
            self.lower_bound.len() + self.upper_bound.len() + self.hole.len() + self.equals.len();
        let dedup = |watchers: &Vec<NogoodWatcher>| -> usize {
            watchers
                .iter()
                .map(|watcher| watcher.right_hand_side)
                .collect::<HashSet<_>>()
                .len()
        };

        let unique = dedup(&self.lower_bound)
            + dedup(&self.upper_bound)
            + dedup(&self.hole)
            + dedup(&self.equals);
        unique as f64 / total as f64
    }
}

impl NogoodWatchList {
    pub(crate) fn iter_lower_bound_watchers(&self) -> impl Iterator<Item = &NogoodWatcher> {
        self.lower_bound.iter()
    }

    pub(crate) fn remove_lower_bound_watcher(&mut self, nogood_id: NogoodId, value: i32) {
        NogoodWatchList::find_and_remove_watcher(&mut self.lower_bound, nogood_id, value);
    }

    pub(crate) fn truncate_lower_bound_watchers(&mut self, new_len: usize) {
        self.lower_bound.truncate(new_len)
    }

    pub(crate) fn set_lower_bound_watcher_to_other_watcher(
        &mut self,
        index: usize,
        other_index: usize,
    ) {
        self.lower_bound[index] = self.lower_bound[other_index]
    }

    pub(crate) fn get_lower_bound_watcher_at_index(&self, index: usize) -> &NogoodWatcher {
        &self.lower_bound[index]
    }

    pub(crate) fn add_lower_bound_watcher(&mut self, nogood_id: NogoodId, right_hand_side: i32) {
        self.lower_bound.push(NogoodWatcher {
            right_hand_side,
            nogood_id,
        })
    }

    pub(crate) fn num_lower_bound_watchers(&self) -> usize {
        self.lower_bound.len()
    }
}

impl NogoodWatchList {
    pub(crate) fn iter_upper_bound_watchers(&self) -> impl Iterator<Item = &NogoodWatcher> {
        self.upper_bound.iter()
    }

    pub(crate) fn remove_upper_bound_watcher(&mut self, nogood_id: NogoodId, value: i32) {
        NogoodWatchList::find_and_remove_watcher(&mut self.upper_bound, nogood_id, value);
    }

    pub(crate) fn truncate_upper_bound_watchers(&mut self, new_len: usize) {
        self.upper_bound.truncate(new_len)
    }

    pub(crate) fn set_upper_bound_watcher_to_other_watcher(
        &mut self,
        index: usize,
        other_index: usize,
    ) {
        self.upper_bound[index] = self.upper_bound[other_index]
    }

    pub(crate) fn get_upper_bound_watcher_at_index(&self, index: usize) -> &NogoodWatcher {
        &self.upper_bound[index]
    }

    pub(crate) fn add_upper_bound_watcher(&mut self, nogood_id: NogoodId, right_hand_side: i32) {
        self.upper_bound.push(NogoodWatcher {
            right_hand_side,
            nogood_id,
        })
    }

    pub(crate) fn num_upper_bound_watchers(&self) -> usize {
        self.upper_bound.len()
    }
}

impl NogoodWatchList {
    pub(crate) fn iter_inequality_watchers(&self) -> impl Iterator<Item = &NogoodWatcher> {
        self.hole.iter()
    }

    pub(crate) fn remove_inequality_watcher(&mut self, nogood_id: NogoodId, value: i32) {
        NogoodWatchList::find_and_remove_watcher(&mut self.hole, nogood_id, value);
    }

    pub(crate) fn truncate_inequality_watchers(&mut self, new_len: usize) {
        self.hole.truncate(new_len)
    }

    pub(crate) fn set_inequality_watcher_to_other_watcher(
        &mut self,
        index: usize,
        other_index: usize,
    ) {
        self.hole[index] = self.hole[other_index]
    }

    pub(crate) fn get_inequality_watcher_at_index(&self, index: usize) -> &NogoodWatcher {
        &self.hole[index]
    }

    pub(crate) fn add_inequality_watcher(&mut self, nogood_id: NogoodId, right_hand_side: i32) {
        self.hole.push(NogoodWatcher {
            right_hand_side,
            nogood_id,
        })
    }

    pub(crate) fn num_inequality_watchers(&self) -> usize {
        self.hole.len()
    }

    pub(crate) fn set_right_hand_side_of_inequality_watcher_at_index(
        &mut self,
        index: usize,
        new_rhs: i32,
    ) {
        self.hole[index].right_hand_side = new_rhs;
    }
}

impl NogoodWatchList {
    pub(crate) fn iter_equality_watchers(&self) -> impl Iterator<Item = &NogoodWatcher> {
        self.equals.iter()
    }

    pub(crate) fn remove_equality_watcher(&mut self, nogood_id: NogoodId, value: i32) {
        NogoodWatchList::find_and_remove_watcher(&mut self.equals, nogood_id, value);
    }

    pub(crate) fn truncate_equality_watchers(&mut self, new_len: usize) {
        self.equals.truncate(new_len)
    }

    pub(crate) fn set_equality_watcher_to_other_watcher(
        &mut self,
        index: usize,
        other_index: usize,
    ) {
        self.equals[index] = self.equals[other_index]
    }

    pub(crate) fn get_equality_watcher_at_index(&self, index: usize) -> &NogoodWatcher {
        &self.equals[index]
    }

    pub(crate) fn add_equality_watcher(&mut self, nogood_id: NogoodId, right_hand_side: i32) {
        self.equals.push(NogoodWatcher {
            right_hand_side,
            nogood_id,
        })
    }

    pub(crate) fn num_equality_watchers(&self) -> usize {
        self.equals.len()
    }
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
