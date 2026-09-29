use std::fmt::Debug;

use bit_set::BitSet;
use enumset::EnumSet;

use super::PredicateIdAssignments;
use super::PredicateValue;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::StorageKey;
use crate::engine::TrailedInteger;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

/// The [`PredicateId`] stored in [`TrackedValueNode::ids`] for [`PredicateType`]s which are not
/// tracked for a value.
const PLACEHOLDER_PREDICATE_ID: PredicateId = PredicateId { id: u32::MAX };

/// A generic structure for keeping track of the polarity of [`Predicate`]s.
///
/// This structure keeps track of all different [`PredicateType`]s.
#[derive(Debug, Clone)]
pub(crate) struct PredicateTracker {
    /// The [`DomainId`] which the tracker is tracking the polarity for.
    domain_id: DomainId,
    /// A [`TrailedInteger`] which points to the largest lowest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x >= 6]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 1.
    min_assigned: TrailedInteger,
    /// A [`TrailedInteger`] which points to the smallest largest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x <= 8]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 3.
    max_assigned: TrailedInteger,
    /// A [`TrailedInteger`] which points to the largest lowest value which is assigned but not
    /// equal to the value.
    ///
    /// For example, if we have the values `x in [1, 6, 7, 9]` and we know that `[x >= 6]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 1.
    min_assigned_strict: TrailedInteger,
    /// A [`TrailedInteger`] which points to the smallest largest value which is assigned but not
    /// equal to the value.
    ///
    /// For example, if we have the values `x in [1, 5, 8, 9]` and we know that `[x <= 8]` holds,
    /// then [`PredicateTracker::min_assigned`] will point to index 3.
    max_assigned_strict: TrailedInteger,
    /// The values which are currently being tracked by this [`PredicateTracker`], each stored as
    /// a node of a doubly linked list which is ordered by value.
    ///
    /// The indices of the nodes remain consistent since they are, for example, stored in
    /// [`TrackedValueNode::smaller`] and [`TrackedValueNode::greater`]. Membership queries are
    /// answered by traversing the linked list (see [`PredicateTracker::track`]) or by looking up
    /// the [`PredicateId`] of a [`Predicate`] in the [`PredicateIdGenerator`] and checking whether
    /// it is tracked (see [`PredicateTracker::on_update`]).
    ///
    /// Note that the nodes are not stored in order of their values.
    nodes: Vec<TrackedValueNode>,
    /// The [`PredicateType`]s tracked by this [`PredicateTracker`].
    tracked: EnumSet<PredicateType>,
}

/// A value tracked by the [`PredicateTracker`], stored as a node of a doubly linked list which is
/// ordered by value.
///
/// A node contains all of the information which is required when traversing the list; this
/// ensures that each step of a traversal only accesses a single node (which takes 32 bytes).
#[derive(Clone, Copy, Debug)]
struct TrackedValueNode {
    /// The tracked value.
    value: i32,
    /// The index of the node with the largest value which is smaller than
    /// [`TrackedValueNode::value`], or [`u32::MAX`] if there is no such node.
    smaller: u32,
    /// The index of the node with the smallest value which is larger than
    /// [`TrackedValueNode::value`], or [`u32::MAX`] if there is no such node.
    greater: u32,
    /// The [`PredicateType`]s which are tracked for this value.
    flags: EnumSet<PredicateType>,
    /// The [`PredicateId`]s of the tracked predicates for this value, indexed by
    /// [`PredicateType`]; if a [`PredicateType`] is not tracked for this value, then its entry is
    /// [`PLACEHOLDER_PREDICATE_ID`].
    ids: [PredicateId; 4],
}

impl TrackedValueNode {
    /// Creates a new [`TrackedValueNode`] which does not track any [`PredicateType`]s yet.
    fn new(value: i32, smaller: u32, greater: u32) -> Self {
        Self {
            value,
            smaller,
            greater,
            flags: EnumSet::new(),
            ids: [PLACEHOLDER_PREDICATE_ID; 4],
        }
    }

    /// Store the provided [`PredicateType`] with its corresponding [`PredicateId`] in the
    /// [`TrackedValueNode`].
    fn track_predicate(&mut self, predicate_type: PredicateType, predicate_id: PredicateId) {
        self.flags |= predicate_type;
        self.ids[predicate_type as usize] = predicate_id;
    }

    /// Returns whether the provided [`PredicateType`] is tracked by this [`TrackedValueNode`].
    fn does_track_predicate_type(&self, predicate_type: PredicateType) -> bool {
        self.flags.contains(predicate_type)
    }

    /// Return the [`PredicateType`]s which are stored in this [`TrackedValueNode`].
    ///
    /// These are always returned in a pre-defined order, not the order in which they were
    /// inserted.
    fn get_predicate_types(&self) -> impl Iterator<Item = PredicateType> {
        self.flags.iter()
    }

    /// Returns the value which is stored in this [`TrackedValueNode`].
    fn get_value(&self) -> i32 {
        self.value
    }
}

impl PartialEq for TrackedValueNode {
    fn eq(&self, other: &Self) -> bool {
        self.get_value().eq(&other.get_value())
    }
}

impl Eq for TrackedValueNode {}

impl PartialOrd for TrackedValueNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TrackedValueNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_value().cmp(&other.get_value())
    }
}

impl PredicateTracker {
    pub(super) fn new() -> Self {
        Self {
            domain_id: DomainId::new(0),
            // We do not want to create the trailed integers until necessary
            min_assigned: TrailedInteger::create_from_index(0),
            max_assigned: TrailedInteger::create_from_index(0),
            min_assigned_strict: TrailedInteger::create_from_index(0),
            max_assigned_strict: TrailedInteger::create_from_index(0),
            nodes: Vec::default(),
            tracked: EnumSet::default(),
        }
    }

    pub(super) fn initialise(
        &mut self,
        domain_id: DomainId,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
        trailed_values: &mut TrailedValues,
    ) {
        if !self.nodes.is_empty() {
            // The structures has been initialised previously
            return;
        }

        self.min_assigned = trailed_values.grow(0);
        self.max_assigned = trailed_values.grow(1);
        self.min_assigned_strict = trailed_values.grow(0);
        self.max_assigned_strict = trailed_values.grow(1);

        // We set the tracking domain id
        self.domain_id = domain_id;

        // Then we place some sentinels for simplicity's sake which are always true; these do not
        // track any predicate types.
        //
        // It is _probably_ okay to note use the `-1` and `+1`
        //
        // For the first element (containing the lower-bound), there is no smaller element and the
        // greater element will currently point to the upper-bound element
        let _ = self.insert_node(initial_lower_bound - 1, u32::MAX, 1);
        // For the second element (containing the upper-bound), the smaller element will currently
        // point to the lower-bound element and there is no greater element
        let _ = self.insert_node(initial_upper_bound + 1, 0, u32::MAX);
    }

    /// Returns whether any [`PredicateType::Equal`] or [`PredicateType::NotEqual`] types are being
    /// tracked.
    pub(super) fn can_be_updated_by_disequality(&self) -> bool {
        self.tracked.contains(PredicateType::Equal)
            || self.tracked.contains(PredicateType::NotEqual)
    }

    /// Returns whether no more updates can take place due to the bounds not being able to be
    /// moved.
    pub(super) fn is_fixed(&self, trailed_values: &TrailedValues) -> bool {
        if self.tracked.is_empty() {
            // If it is empty, then it is trivially fixed
            return true;
        }

        // The idea is to use the `min_assigned` and `max_assigned` fields to infer whether any
        // updates can take place.
        //
        // Let's first look at an example for a variable `x`, imagine we have the following values
        // [0, 10, 5, 2, 3, 1] where `x \in [0, 10]` (i.e. the first two values are fixed);
        // we know that `min_assigned = 0` and `max_assigned = 1`; now we update the domain
        // of `x` to be `[4, 4]`. We know that `min_assigned = 4` (pointing to value 3), and
        // `max_assigned = 2` (pointing to value 5).
        //
        // If we now look at the successor of `min_assigned` (with index 2 and value 5) and the
        // predecessor of `max_assigned` (with index 5 and value 3), then we can see that
        // these are already assigned (according to `min_assigned` and `max_assigned`
        // respectively).
        //
        // Thus, we simply need to check whether either:
        // - The successor of `min_assigned` is equal to `max_assigned`
        // - The predecessor of `max_assigned` is equal to `min_assigned`
        let min_assigned_index = trailed_values.read(self.min_assigned) as usize;
        let min_unassigned_index = self.nodes[min_assigned_index].greater as usize;
        pumpkin_assert_simple!(self.nodes[min_assigned_index] < self.nodes[min_unassigned_index]);

        let max_assigned_index = trailed_values.read(self.max_assigned) as usize;
        let max_unassigned_index = self.nodes[max_assigned_index].smaller as usize;
        pumpkin_assert_simple!(self.nodes[max_assigned_index] > self.nodes[max_unassigned_index]);

        self.nodes[min_unassigned_index] >= self.nodes[max_assigned_index]
            || self.nodes[max_unassigned_index] <= self.nodes[min_assigned_index]
    }

    /// Inserts a node for the value with the provided neighbours into the internal structures,
    /// and returns its index.
    ///
    /// Note that this does not update the neighbours to point to the new node.
    fn insert_node(&mut self, value: i32, smaller: u32, greater: u32) -> usize {
        let index = self.nodes.len();
        self.nodes
            .push(TrackedValueNode::new(value, smaller, greater));

        index
    }

    /// Returns the node at the provided index.
    ///
    /// If the index is out of bounds, this method will panic.
    fn get_node_at_index(&self, index: usize) -> TrackedValueNode {
        self.nodes[index]
    }

    /// Returns all of the nodes currently present.
    fn get_all_nodes(&self) -> impl Iterator<Item = TrackedValueNode> {
        self.nodes.iter().copied()
    }

    /// Allows the [`PredicateTracker`] to indicate that a tracked [`Predicate`] has been satisfied.
    fn predicate_has_been_satisfied(
        &self,
        index: usize,
        predicate_type: PredicateType,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let predicate_id = self.nodes[index].ids[predicate_type as usize];
        if predicate_id == PLACEHOLDER_PREDICATE_ID {
            // If it is a placeholder then we ignore it
            return;
        }
        predicate_id_assignments.store_predicate(predicate_id, PredicateValue::AssignedTrue);
    }

    /// Allows the [`PredicateTracker`] to indicate that a tracked [`Predicate`] has been falsified.
    fn predicate_has_been_falsified(
        &self,
        index: usize,
        predicate_type: PredicateType,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let predicate_id = self.nodes[index].ids[predicate_type as usize];
        if predicate_id == PLACEHOLDER_PREDICATE_ID {
            return;
        }
        predicate_id_assignments.store_predicate(predicate_id, PredicateValue::AssignedFalse);
    }

    /// Tracks a [`Predicate`] with a provided `value` and [`PredicateId`].
    ///
    /// Returns true if it was not already tracked and false otherwise.
    pub(super) fn track(&mut self, predicate: Predicate, predicate_id: PredicateId) -> bool {
        pumpkin_assert_simple!(
            !self.nodes.is_empty(),
            "Initialise should have been called previously"
        );

        let predicate_type = predicate.get_predicate_type();
        self.tracked |= predicate_type;

        let value = predicate.get_right_hand_side();

        // Then we track the information for updating `smaller`; recall that we place a sentinel
        // node with the smallest possible value at index 0
        let index_largest_value_smaller_than;

        // And we track the information for updating `greater`; recall that we place a sentinel
        // node with the largest possible value at index 1
        let index_smallest_value_larger_than;

        // Then we go over each value (from largest to smallest) to determine whether the value is
        // already tracked, and otherwise where to place the element in the linked list.
        //
        // Note that the element at the 1st index has the largest value
        let mut index = 1;
        loop {
            let index_value = self.get_node_at_index(index);

            // If the value is already tracked, then we check whether this particular predicate
            // type has already been tracked
            if index_value.get_value() == value {
                if index_value.does_track_predicate_type(predicate_type) {
                    return false;
                }

                self.nodes[index].track_predicate(predicate_type, predicate_id);

                return true;
            }

            // As soon as we have found a value smaller than the to track value, we can stop
            if index_value.get_value() < value {
                index_largest_value_smaller_than = index as u32;

                index_smallest_value_larger_than = self.nodes[index].greater;
                break;
            }

            index = self.nodes[index].smaller as usize;
        }

        pumpkin_assert_eq_simple!(
            self.get_node_at_index(index_largest_value_smaller_than as usize),
            self.get_all_nodes()
                .filter(|&stored_value| stored_value.get_value() < value)
                .max()
                .unwrap(),
        );
        pumpkin_assert_eq_simple!(
            self.get_node_at_index(index_smallest_value_larger_than as usize),
            self.get_all_nodes()
                .filter(|&stored_value| stored_value.get_value() > value)
                .min()
                .unwrap()
        );

        let new_index = self.insert_node(
            value,
            index_largest_value_smaller_than,
            index_smallest_value_larger_than,
        );
        self.nodes[new_index].track_predicate(predicate_type, predicate_id);

        // Then we update the neighbours to point to the new node
        self.nodes[index_largest_value_smaller_than as usize].greater = new_index as u32;
        self.nodes[index_smallest_value_larger_than as usize].smaller = new_index as u32;

        true
    }

    /// Moves [`PredicateTracker::min_assigned_strict`] and [`PredicateTracker::min_assigned`]
    /// past all tracked values which are respectively `<` and `<=` the provided lower-bound
    /// `value`, and updates the tracked predicates of the passed values accordingly.
    ///
    /// The cursors are kept in local variables during the traversal and are only written to the
    /// [`TrailedValues`] once afterwards; this prevents adding an entry to the trail for every
    /// traversed value.
    fn update_lower_bound(
        &self,
        value: i32,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let mut min_assigned_strict = trailed_values.read(self.min_assigned_strict) as u32;
        let mut min_assigned = trailed_values.read(self.min_assigned) as u32;

        // First, we move `min_assigned_strict` by checking whether the greater predicate is
        // also satisfied
        let mut greater_strict = self.nodes[min_assigned_strict as usize].greater;
        while greater_strict != u32::MAX && value > self.nodes[greater_strict as usize].get_value()
        {
            // Now we go over all tracked predicate types and update them
            for predicate_type in self.nodes[greater_strict as usize].get_predicate_types() {
                match predicate_type {
                    PredicateType::UpperBound | PredicateType::Equal => {
                        self.predicate_has_been_falsified(
                            greater_strict as usize,
                            predicate_type,
                            predicate_id_assignments,
                        );
                    }
                    PredicateType::NotEqual => {
                        self.predicate_has_been_satisfied(
                            greater_strict as usize,
                            predicate_type,
                            predicate_id_assignments,
                        );
                    }
                    PredicateType::LowerBound => {
                        self.predicate_has_been_satisfied(
                            greater_strict as usize,
                            predicate_type,
                            predicate_id_assignments,
                        );
                    }
                }
            }
            // Note that we can move both instances since, if an update has a value `>` a
            // tracked value, then it is also necessarily `>=`
            min_assigned_strict = greater_strict;
            min_assigned = greater_strict;

            greater_strict = self.nodes[greater_strict as usize].greater;
        }

        // Now we move the `>=` index as well.
        let mut greater = self.nodes[min_assigned as usize].greater;
        while greater != u32::MAX && value >= self.nodes[greater as usize].get_value() {
            // In this case, we can only have a lower-bound update, because all of the other
            // predicate types require a strictly larger value
            if self.nodes[greater as usize].does_track_predicate_type(PredicateType::LowerBound) {
                self.predicate_has_been_satisfied(
                    greater as usize,
                    PredicateType::LowerBound,
                    predicate_id_assignments,
                );
            }
            min_assigned = greater;
            greater = self.nodes[greater as usize].greater;
        }

        trailed_values.assign(self.min_assigned_strict, min_assigned_strict as i64);
        trailed_values.assign(self.min_assigned, min_assigned as i64);
    }

    /// Moves [`PredicateTracker::max_assigned_strict`] and [`PredicateTracker::max_assigned`]
    /// past all tracked values which are respectively `>` and `>=` the provided upper-bound
    /// `value`, and updates the tracked predicates of the passed values accordingly.
    ///
    /// The cursors are kept in local variables during the traversal and are only written to the
    /// [`TrailedValues`] once afterwards; this prevents adding an entry to the trail for every
    /// traversed value.
    fn update_upper_bound(
        &self,
        value: i32,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let mut max_assigned_strict = trailed_values.read(self.max_assigned_strict) as u32;
        let mut max_assigned = trailed_values.read(self.max_assigned) as u32;

        // First, we move `max_assigned_strict` by checking whether the smaller predicate is
        // also satisfied
        let mut smaller_strict = self.nodes[max_assigned_strict as usize].smaller;
        while smaller_strict != u32::MAX && value < self.nodes[smaller_strict as usize].get_value()
        {
            // Now we go over all tracked predicate types and update them
            for predicate_type in self.nodes[smaller_strict as usize].get_predicate_types() {
                match predicate_type {
                    PredicateType::LowerBound | PredicateType::Equal => {
                        self.predicate_has_been_falsified(
                            smaller_strict as usize,
                            predicate_type,
                            predicate_id_assignments,
                        );
                    }
                    PredicateType::NotEqual => {
                        self.predicate_has_been_satisfied(
                            smaller_strict as usize,
                            predicate_type,
                            predicate_id_assignments,
                        );
                    }
                    PredicateType::UpperBound => {
                        self.predicate_has_been_satisfied(
                            smaller_strict as usize,
                            predicate_type,
                            predicate_id_assignments,
                        );
                    }
                }
            }
            // Note that we can move both instances since, if an update has a value `<` a
            // tracked value, then it is also necessarily `<=`
            max_assigned_strict = smaller_strict;
            max_assigned = smaller_strict;

            smaller_strict = self.nodes[smaller_strict as usize].smaller;
        }

        // Now we move the `<=` index as well.
        let mut smaller = self.nodes[max_assigned as usize].smaller;
        while smaller != u32::MAX && value <= self.nodes[smaller as usize].get_value() {
            // In this case, we can only have a upper-bound update, because all of the other
            // predicate types require a strictly smaller value
            if self.nodes[smaller as usize].does_track_predicate_type(PredicateType::UpperBound) {
                self.predicate_has_been_satisfied(
                    smaller as usize,
                    PredicateType::UpperBound,
                    predicate_id_assignments,
                );
            }
            max_assigned = smaller;
            smaller = self.nodes[smaller as usize].smaller;
        }

        trailed_values.assign(self.max_assigned_strict, max_assigned_strict as i64);
        trailed_values.assign(self.max_assigned, max_assigned as i64);
    }

    pub(super) fn on_update(
        &self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        predicate_id_generator: &PredicateIdGenerator,
        is_tracked: &BitSet,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        // If there are no tracked predicate types, then we don't need to perform any updates
        if self.tracked.is_empty() {
            return;
        }

        let value = predicate.get_right_hand_side();

        // Then we update our internal structures
        //
        // The updates which can occur depend on the predicate type
        if predicate.is_lower_bound_predicate() {
            // We have a lower-bound predicate, so we move our min indices
            self.update_lower_bound(value, trailed_values, predicate_id_assignments);
        } else if predicate.is_upper_bound_predicate() {
            // We have an upper-bound predicate, so we move our max indices
            self.update_upper_bound(value, trailed_values, predicate_id_assignments);
        } else if predicate.is_not_equal_predicate() {
            // If the right-hand side of the disequality predicate is smaller than the value
            // pointed to by `min_assigned_strict` then no updates can take place
            if value
                <= self.nodes[trailed_values.read(self.min_assigned_strict) as usize].get_value()
            {
                return;
            }

            // If the right-hand side of the disequality predicate is larger than the value
            // pointed to by `max_assigned_strict` then no updates can take place
            if value
                >= self.nodes[trailed_values.read(self.max_assigned_strict) as usize].get_value()
            {
                return;
            }

            // Now we check whether the disequality predicate and its negation (i.e., the equality
            // predicate) are tracked; if so, then we update them accordingly.
            if let Some(predicate_id) = predicate_id_generator.get_existing_id(predicate)
                && is_tracked.contains(predicate_id.index())
            {
                predicate_id_assignments
                    .store_predicate(predicate_id, PredicateValue::AssignedTrue);
            }
            if let Some(predicate_id) = predicate_id_generator.get_existing_id(!predicate)
                && is_tracked.contains(predicate_id.index())
            {
                predicate_id_assignments
                    .store_predicate(predicate_id, PredicateValue::AssignedFalse);
            }
        } else if predicate.is_equality_predicate() {
            // First update the lower-bound if necessary, and then the upper-bound
            self.update_lower_bound(value, trailed_values, predicate_id_assignments);
            self.update_upper_bound(value, trailed_values, predicate_id_assignments);

            // Now that we have moved the indices, we want to check whether it has become true
            //
            // We check whether min_assigned_strict and max_assigned_strict point to each other and
            // that the next value is equal to the value
            let greater =
                self.nodes[trailed_values.read(self.min_assigned_strict) as usize].greater;
            if greater == self.nodes[trailed_values.read(self.max_assigned_strict) as usize].smaller
                && self.nodes[greater as usize].get_value() == value
            {
                for predicate_type in self.nodes[greater as usize].get_predicate_types() {
                    match predicate_type {
                        PredicateType::NotEqual => {
                            self.predicate_has_been_falsified(
                                greater as usize,
                                predicate_type,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::Equal => {
                            self.predicate_has_been_satisfied(
                                greater as usize,
                                predicate_type,
                                predicate_id_assignments,
                            );
                        }
                        _ => {}
                    }
                }
            } else {
                pumpkin_assert_moderate!(self.nodes.iter().all(|tracked_value| {
                    tracked_value.get_value() != value
                        || (!tracked_value.does_track_predicate_type(PredicateType::NotEqual)
                            && !tracked_value.does_track_predicate_type(PredicateType::Equal))
                }));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bit_set::BitSet;

    use crate::basic_types::PredicateId;
    use crate::engine::Assignments;
    use crate::engine::TrailedValues;
    use crate::engine::notifications::predicate_notification::PredicateIdAssignments;
    use crate::engine::notifications::predicate_notification::predicate_tracker::PredicateTracker;
    use crate::engine::notifications::predicate_notification::predicate_tracker::TrackedValueNode;
    use crate::predicate;
    use crate::predicates::PredicateIdGenerator;
    use crate::predicates::PredicateType;

    #[test]
    fn test_update_lower_bound() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let is_tracked: BitSet = (0..id_generator.num_predicate_ids()).collect();

        tracker.on_update(
            predicate!(x >= 5),
            &mut trailed_values,
            &id_generator,
            &is_tracked,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x <= 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x == 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x != 5))));

        tracker.on_update(
            predicate!(x >= 6),
            &mut trailed_values,
            &id_generator,
            &is_tracked,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &id_generator
        ));
    }

    #[test]
    fn test_update_upper_bound() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let is_tracked: BitSet = (0..id_generator.num_predicate_ids()).collect();

        tracker.on_update(
            predicate!(x <= 5),
            &mut trailed_values,
            &id_generator,
            &is_tracked,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x >= 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x == 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x != 5))));

        tracker.on_update(
            predicate!(x <= 4),
            &mut trailed_values,
            &id_generator,
            &is_tracked,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &id_generator
        ));
    }

    #[test]
    fn test_update_not_equals() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let is_tracked: BitSet = (0..id_generator.num_predicate_ids()).collect();

        tracker.on_update(
            predicate!(x != 5),
            &mut trailed_values,
            &id_generator,
            &is_tracked,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x >= 5))));
        assert!(predicate_id_assignments.is_unknown(id_generator.get_id(predicate!(x <= 5))));
    }

    #[test]
    fn test_update_equals() {
        let mut assignments = Assignments::default();
        let mut id_generator = PredicateIdGenerator::default();
        let mut trailed_values = TrailedValues::default();
        let mut predicate_id_assignments = PredicateIdAssignments::default();

        let x = assignments.grow(0, 10);

        let mut tracker = PredicateTracker::new();

        tracker.initialise(x, 0, 10, &mut trailed_values);

        let predicate = predicate!(x >= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(!added);

        let predicate = predicate!(x <= 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 5);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x == 6);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let predicate = predicate!(x != 6);
        let added = tracker.track(predicate, id_generator.get_id(predicate));
        assert!(added);

        let is_tracked: BitSet = (0..id_generator.num_predicate_ids()).collect();

        tracker.on_update(
            predicate!(x == 6),
            &mut trailed_values,
            &id_generator,
            &is_tracked,
            &mut predicate_id_assignments,
        );
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x == 6)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x != 6)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x != 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x == 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_satisfied(
            id_generator.get_id(predicate!(x >= 5)),
            &assignments,
            &id_generator
        ));
        assert!(predicate_id_assignments.is_falsified(
            id_generator.get_id(predicate!(x <= 5)),
            &assignments,
            &id_generator
        ));
    }

    #[test]
    fn tracked_value_node_fits_in_32_bytes() {
        assert_eq!(size_of::<TrackedValueNode>(), 32);
    }

    #[test]
    fn pack_negative_value() {
        let x = -255;

        let mut value = TrackedValueNode::new(x, u32::MAX, u32::MAX);

        assert_eq!(value.get_value(), x);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            Vec::<PredicateType>::new()
        );

        value.track_predicate(
            PredicateType::Equal,
            PredicateId {
                id: PredicateType::Equal as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate(
            PredicateType::LowerBound,
            PredicateId {
                id: PredicateType::LowerBound as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::LowerBound, PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate(
            PredicateType::NotEqual,
            PredicateId {
                id: PredicateType::NotEqual as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::NotEqual,
                PredicateType::Equal,
            ]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate(
            PredicateType::UpperBound,
            PredicateId {
                id: PredicateType::UpperBound as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::NotEqual,
                PredicateType::Equal,
                PredicateType::UpperBound,
            ]
        );
        assert_eq!(value.get_value(), x);
        for predicate_type in value.get_predicate_types() {
            assert_eq!(
                value.ids[predicate_type as usize],
                PredicateId {
                    id: predicate_type as u32
                }
            );
        }
    }

    #[test]
    fn pack_positive_value() {
        let x = 255;

        let mut value = TrackedValueNode::new(x, u32::MAX, u32::MAX);

        assert_eq!(value.get_value(), x);
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            Vec::<PredicateType>::new()
        );

        value.track_predicate(
            PredicateType::Equal,
            PredicateId {
                id: PredicateType::Equal as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate(
            PredicateType::LowerBound,
            PredicateId {
                id: PredicateType::LowerBound as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![PredicateType::LowerBound, PredicateType::Equal]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate(
            PredicateType::NotEqual,
            PredicateId {
                id: PredicateType::NotEqual as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::NotEqual,
                PredicateType::Equal,
            ]
        );
        assert_eq!(value.get_value(), x);

        value.track_predicate(
            PredicateType::UpperBound,
            PredicateId {
                id: PredicateType::UpperBound as u32,
            },
        );
        assert_eq!(
            value.get_predicate_types().collect::<Vec<_>>(),
            vec![
                PredicateType::LowerBound,
                PredicateType::NotEqual,
                PredicateType::Equal,
                PredicateType::UpperBound,
            ]
        );
        assert_eq!(value.get_value(), x);
        for predicate_type in value.get_predicate_types() {
            assert_eq!(
                value.ids[predicate_type as usize],
                PredicateId {
                    id: predicate_type as u32
                }
            );
        }
    }
}
