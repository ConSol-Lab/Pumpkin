use enumset::EnumSet;

use super::TransformableVariable;
use crate::containers::StorageKey;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::variables::AffineView;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::pumpkin_assert_simple;

/// A structure which represents the most basic [`IntegerVariable`]; it is simply the id which links
/// to a domain (hence the name).
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct DomainId {
    id: u32,
}

impl DomainId {
    pub fn new(id: u32) -> Self {
        pumpkin_assert_simple!(id >> 30 == 0, "The first two bits are used as flags");
        DomainId { id }
    }

    pub fn id(&self) -> u32 {
        self.id
    }
}

impl IntegerVariable for DomainId {
    type AffineView = AffineView<Self>;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        assignment.get_lower_bound(*self)
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        assignment.get_lower_bound_at_trail_position(*self, trail_position)
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        assignment.get_upper_bound(*self)
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        assignment.get_upper_bound_at_trail_position(*self, trail_position)
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        assignment.is_value_in_domain(*self, value)
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        assignment.is_value_in_domain_at_trail_position(*self, value, trail_position)
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        assignment.get_domain_iterator(*self)
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_all(*self, events);
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_all_backtrack(*self, events);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        event.unwrap()
    }

    fn get_holes_on_current_decision_level(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        assignments.get_holes_on_current_decision_level(*self)
    }
}

impl TransformableVariable<AffineView<DomainId>> for DomainId {
    fn scaled(&self, scale: i32) -> AffineView<DomainId> {
        AffineView::new(*self, scale, 0)
    }

    fn offset(&self, offset: i32) -> AffineView<DomainId> {
        AffineView::new(*self, 1, offset)
    }
}

impl StorageKey for DomainId {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        DomainId { id: index as u32 }
    }
}

impl std::fmt::Display for DomainId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.id)
    }
}

impl std::fmt::Debug for DomainId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.id)
    }
}
