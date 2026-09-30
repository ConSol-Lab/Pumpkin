use std::fmt::Debug;

use bit_set::BitSet;
use bitfield_struct::bitfield;
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
use crate::pumpkin_assert_eq_moderate;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

/// The [`PredicateId`] stored in [`TrackedValueNode::ids`] for [`PredicateType`]s which are not
/// tracked for a value.
const PLACEHOLDER_PREDICATE_ID: PredicateId = PredicateId { id: u32::MAX };

/// A generic structure for keeping track of the polarity of [`Predicate`]s.
///
/// This structure keeps track of all different [`PredicateType`]s.
#[derive(Debug, Clone)]
pub(crate) struct PredicateTracker {
    /// A [`TrailedInteger`] which contains the [`AssignedIndices`] (i.e., the indices of the nodes
    /// pointed to by `min_assigned` and `max_assigned`, from which `min_assigned_strict` and
    /// `max_assigned_strict` can be derived) packed into a single value.
    assigned_indices: TrailedInteger,
    /// The values which are currently being tracked by this [`PredicateTracker`], each stored as
    /// a node of a doubly linked list which is ordered by value.
    ///
    /// Note that the nodes are not stored in order of their values.
    nodes: Vec<TrackedValueNode>,
    /// The [`PredicateType`]s tracked by this [`PredicateTracker`].
    tracked: EnumSet<PredicateType>,
}

/// The indices of the nodes (see [`PredicateTracker::nodes`]) which indicate up to which values
/// the tracked predicates of a [`PredicateTracker`] have been assigned.
///
/// Besides `min_assigned` and `max_assigned`, the [`PredicateTracker`] makes use of the strict
/// indices `min_assigned_strict` and `max_assigned_strict` (see
/// [`PredicateTracker::min_assigned_strict`] and [`PredicateTracker::max_assigned_strict`]).
/// These are not stored explicitly, since `min_assigned_strict` is always either equal to
/// `min_assigned` or it is the node preceding it (i.e., `nodes[min_assigned].smaller`); the latter
/// is the case if and only if the lower-bound is equal to the value of `min_assigned`, which is
/// stored in `min_assigned_is_tight` (and analogously for the upper-bound).
///
/// These are packed into a single `u64` such that they can be stored in a single
/// [`TrailedInteger`]; this limits the number of nodes of a [`PredicateTracker`] to
/// [`MAX_NUMBER_OF_NODES`].
#[bitfield(u64)]
struct AssignedIndices {
    /// Points to the largest lowest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x >= 6]` holds,
    /// then `min_assigned` will point to index 1.
    #[bits(31)]
    min_assigned: u32,
    /// Whether the lower-bound is equal to the value of `min_assigned`, in which case
    /// `min_assigned_strict` (i.e., the largest lowest value which is assigned but not equal to
    /// the value) points to the node preceding `min_assigned`.
    ///
    /// For example, if we have the values `x in [1, 6, 7, 9]` and we know that `[x >= 6]` holds,
    /// then `min_assigned` will point to index 1, `min_assigned_is_tight` is true, and
    /// `min_assigned_strict` will point to index 0.
    min_assigned_is_tight: bool,
    /// Points to the smallest largest value which is assigned.
    ///
    /// For example, if we have the values `x in [1, 5, 7, 9]` and we know that `[x <= 8]` holds,
    /// then `max_assigned` will point to index 3.
    #[bits(31)]
    max_assigned: u32,
    /// Whether the upper-bound is equal to the value of `max_assigned`, in which case
    /// `max_assigned_strict` (i.e., the smallest largest value which is assigned but not equal to
    /// the value) points to the node succeeding `max_assigned`.
    ///
    /// For example, if we have the values `x in [1, 5, 8, 9]` and we know that `[x <= 8]` holds,
    /// then `max_assigned` will point to index 2, `max_assigned_is_tight` is true, and
    /// `max_assigned_strict` will point to index 3.
    max_assigned_is_tight: bool,
}

/// The maximum number of nodes (including the two sentinels) which a [`PredicateTracker`] can
/// contain, since the [`AssignedIndices`] store the indices of nodes using 31 bits.
const MAX_NUMBER_OF_NODES: usize = 1 << 31;

/// A value tracked by the [`PredicateTracker`], stored as a node of a doubly linked list which is
/// ordered by value.
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
            // We do not want to create the trailed integers until necessary
            assigned_indices: TrailedInteger::create_from_index(0),
            nodes: Vec::default(),
            tracked: EnumSet::default(),
        }
    }

    /// Returns the [`AssignedIndices`] which are currently stored in the [`TrailedValues`].
    fn read_assigned_indices(&self, trailed_values: &TrailedValues) -> AssignedIndices {
        AssignedIndices::from_bits(trailed_values.read(self.assigned_indices) as u64)
    }

    /// Stores the provided [`AssignedIndices`] in the [`TrailedValues`].
    fn write_assigned_indices(
        &self,
        trailed_values: &mut TrailedValues,
        assigned_indices: AssignedIndices,
    ) {
        trailed_values.assign(self.assigned_indices, assigned_indices.into_bits() as i64);
    }

    /// Returns the index of the node with the largest lowest value which is assigned but not equal
    /// to the value (see [`AssignedIndices`]).
    fn min_assigned_strict(&self, assigned_indices: AssignedIndices) -> u32 {
        if assigned_indices.min_assigned_is_tight() {
            self.nodes[assigned_indices.min_assigned() as usize].smaller
        } else {
            assigned_indices.min_assigned()
        }
    }

    /// Returns the index of the node with the smallest largest value which is assigned but not
    /// equal to the value (see [`AssignedIndices`]).
    fn max_assigned_strict(&self, assigned_indices: AssignedIndices) -> u32 {
        if assigned_indices.max_assigned_is_tight() {
            self.nodes[assigned_indices.max_assigned() as usize].greater
        } else {
            assigned_indices.max_assigned()
        }
    }

    pub(super) fn initialise(
        &mut self,
        initial_lower_bound: i32,
        initial_upper_bound: i32,
        trailed_values: &mut TrailedValues,
    ) {
        if !self.nodes.is_empty() {
            // The structures has been initialised previously
            return;
        }

        // Initially, the minimum indices point to the lower-bound sentinel (at index 0) and the
        // maximum indices point to the upper-bound sentinel (at index 1)
        //
        // Since the bounds are not equal to the sentinel values, the indices are not tight
        let initial_assigned_indices = AssignedIndices::new()
            .with_min_assigned(0)
            .with_min_assigned_is_tight(false)
            .with_max_assigned(1)
            .with_max_assigned_is_tight(false);
        self.assigned_indices = trailed_values.grow(initial_assigned_indices.into_bits() as i64);

        // Then we place some sentinels for simplicity's sake which are always true; these do not
        // track any predicate types.
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
        let assigned_indices = self.read_assigned_indices(trailed_values);

        let min_assigned_index = assigned_indices.min_assigned() as usize;
        let min_unassigned_index = self.nodes[min_assigned_index].greater as usize;
        pumpkin_assert_moderate!(self.nodes[min_assigned_index] < self.nodes[min_unassigned_index]);

        let max_assigned_index = assigned_indices.max_assigned() as usize;
        let max_unassigned_index = self.nodes[max_assigned_index].smaller as usize;
        pumpkin_assert_moderate!(self.nodes[max_assigned_index] > self.nodes[max_unassigned_index]);

        self.nodes[min_unassigned_index] >= self.nodes[max_assigned_index]
            || self.nodes[max_unassigned_index] <= self.nodes[min_assigned_index]
    }

    /// Inserts a node for the value with the provided neighbours into the internal structures,
    /// and returns its index.
    ///
    /// Note that this does not update the neighbours to point to the new node.
    fn insert_node(&mut self, value: i32, smaller: u32, greater: u32) -> usize {
        let index = self.nodes.len();
        pumpkin_assert_simple!(
            index < MAX_NUMBER_OF_NODES,
            "A predicate tracker supports at most {MAX_NUMBER_OF_NODES} nodes"
        );
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

        pumpkin_assert_eq_moderate!(
            self.get_node_at_index(index_largest_value_smaller_than as usize),
            self.get_all_nodes()
                .filter(|&stored_value| stored_value.get_value() < value)
                .max()
                .unwrap(),
        );
        pumpkin_assert_eq_moderate!(
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

    /// Moves `min_assigned` past all tracked values which are `<=` the provided lower-bound
    /// `value` (and stores whether it is tight, i.e., whether its value is equal to `value`), and
    /// updates the tracked predicates of the passed values accordingly (see [`AssignedIndices`]).
    fn update_lower_bound(
        &self,
        value: i32,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let assigned_indices = self.read_assigned_indices(trailed_values);
        let mut min_assigned = assigned_indices.min_assigned();
        let mut min_assigned_is_tight = assigned_indices.min_assigned_is_tight();

        // We traverse the nodes starting from the first node which has not been passed strictly
        // yet; if `min_assigned` is tight, then this is `min_assigned` itself, but only if
        // the bound has now moved strictly past it (otherwise it has already been processed).
        //
        // Nodes with a value strictly larger than the bound are passed strictly, while a node with
        // a value equal to the bound makes `min_assigned` tight (and there can be at most
        // one such node).
        let mut greater =
            if min_assigned_is_tight && value > self.nodes[min_assigned as usize].get_value() {
                min_assigned
            } else {
                self.nodes[min_assigned as usize].greater
            };
        while greater != u32::MAX && value >= self.nodes[greater as usize].get_value() {
            if value > self.nodes[greater as usize].get_value() {
                // Now we go over all tracked predicate types and update them
                for predicate_type in self.nodes[greater as usize].get_predicate_types() {
                    match predicate_type {
                        PredicateType::UpperBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                greater as usize,
                                predicate_type,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual | PredicateType::LowerBound => {
                            self.predicate_has_been_satisfied(
                                greater as usize,
                                predicate_type,
                                predicate_id_assignments,
                            );
                        }
                    }
                }
                min_assigned_is_tight = false;
            } else {
                // The value of the node is equal to the bound, so only the lower-bound predicate
                // can be updated, since all of the other predicate types require a strictly
                // larger value
                if self.nodes[greater as usize].does_track_predicate_type(PredicateType::LowerBound)
                {
                    self.predicate_has_been_satisfied(
                        greater as usize,
                        PredicateType::LowerBound,
                        predicate_id_assignments,
                    );
                }
                min_assigned_is_tight = true;
            }
            min_assigned = greater;
            greater = self.nodes[greater as usize].greater;
        }

        self.write_assigned_indices(
            trailed_values,
            assigned_indices
                .with_min_assigned(min_assigned)
                .with_min_assigned_is_tight(min_assigned_is_tight),
        );
    }

    /// Moves `max_assigned` past all tracked values which are `>=` the provided upper-bound
    /// `value` (and stores whether it is tight, i.e., whether its value is equal to `value`), and
    /// updates the tracked predicates of the passed values accordingly (see [`AssignedIndices`]).
    fn update_upper_bound(
        &self,
        value: i32,
        trailed_values: &mut TrailedValues,
        predicate_id_assignments: &mut PredicateIdAssignments,
    ) {
        let assigned_indices = self.read_assigned_indices(trailed_values);
        let mut max_assigned = assigned_indices.max_assigned();
        let mut max_assigned_is_tight = assigned_indices.max_assigned_is_tight();

        // We traverse the nodes starting from the first node which has not been passed strictly
        // yet; if `max_assigned` is tight, then this is `max_assigned` itself, but only if
        // the bound has now moved strictly past it (otherwise it has already been processed).
        //
        // Nodes with a value strictly smaller than the bound are passed strictly, while a node with
        // a value equal to the bound makes `max_assigned` tight (and there can be at most
        // one such node).
        let mut smaller =
            if max_assigned_is_tight && value < self.nodes[max_assigned as usize].get_value() {
                max_assigned
            } else {
                self.nodes[max_assigned as usize].smaller
            };
        while smaller != u32::MAX && value <= self.nodes[smaller as usize].get_value() {
            if value < self.nodes[smaller as usize].get_value() {
                // Now we go over all tracked predicate types and update them
                for predicate_type in self.nodes[smaller as usize].get_predicate_types() {
                    match predicate_type {
                        PredicateType::LowerBound | PredicateType::Equal => {
                            self.predicate_has_been_falsified(
                                smaller as usize,
                                predicate_type,
                                predicate_id_assignments,
                            );
                        }
                        PredicateType::NotEqual | PredicateType::UpperBound => {
                            self.predicate_has_been_satisfied(
                                smaller as usize,
                                predicate_type,
                                predicate_id_assignments,
                            );
                        }
                    }
                }
                max_assigned_is_tight = false;
            } else {
                // The value of the node is equal to the bound, so only the upper-bound predicate
                // can be updated, since all of the other predicate types require a strictly
                // smaller value
                if self.nodes[smaller as usize].does_track_predicate_type(PredicateType::UpperBound)
                {
                    self.predicate_has_been_satisfied(
                        smaller as usize,
                        PredicateType::UpperBound,
                        predicate_id_assignments,
                    );
                }
                max_assigned_is_tight = true;
            }
            max_assigned = smaller;
            smaller = self.nodes[smaller as usize].smaller;
        }

        self.write_assigned_indices(
            trailed_values,
            assigned_indices
                .with_max_assigned(max_assigned)
                .with_max_assigned_is_tight(max_assigned_is_tight),
        );
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
            let assigned_indices = self.read_assigned_indices(trailed_values);

            // If the right-hand side of the disequality predicate is smaller than the value
            // pointed to by `min_assigned_strict` then no updates can take place
            if value <= self.nodes[self.min_assigned_strict(assigned_indices) as usize].get_value()
            {
                return;
            }

            // If the right-hand side of the disequality predicate is larger than the value
            // pointed to by `max_assigned_strict` then no updates can take place
            if value >= self.nodes[self.max_assigned_strict(assigned_indices) as usize].get_value()
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
            let assigned_indices = self.read_assigned_indices(trailed_values);
            let greater = self.nodes[self.min_assigned_strict(assigned_indices) as usize].greater;
            if greater == self.nodes[self.max_assigned_strict(assigned_indices) as usize].smaller
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
    use crate::engine::notifications::predicate_notification::predicate_tracker::AssignedIndices;
    use crate::engine::notifications::predicate_notification::predicate_tracker::MAX_NUMBER_OF_NODES;
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

        tracker.initialise(0, 10, &mut trailed_values);

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

        tracker.initialise(0, 10, &mut trailed_values);

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

        tracker.initialise(0, 10, &mut trailed_values);

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

        tracker.initialise(0, 10, &mut trailed_values);

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
    fn assigned_indices_are_preserved_by_trailed_values() {
        let mut trailed_values = TrailedValues::default();

        // The largest index is stored in the highest bits, which exercises the sign bit of the
        // stored value
        let largest_index = (MAX_NUMBER_OF_NODES - 1) as u32;
        let assigned_indices = AssignedIndices::new()
            .with_min_assigned(1)
            .with_min_assigned_is_tight(true)
            .with_max_assigned(largest_index)
            .with_max_assigned_is_tight(true);
        let mut tracker = PredicateTracker::new();
        tracker.initialise(0, 10, &mut trailed_values);

        tracker.write_assigned_indices(&mut trailed_values, assigned_indices);
        let read = tracker.read_assigned_indices(&trailed_values);
        assert_eq!(read.min_assigned(), 1);
        assert!(read.min_assigned_is_tight());
        assert_eq!(read.max_assigned(), largest_index);
        assert!(read.max_assigned_is_tight());
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
