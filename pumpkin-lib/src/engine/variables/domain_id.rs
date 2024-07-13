use enumset::EnumSet;

use super::TransformableVariable;
use crate::basic_types::StorageKey;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::reason::ReasonRef;
use crate::engine::variables::AffineView;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::IntDomainEvent;
use crate::engine::Watchers;

#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct DomainId {
    pub id: u32,
}

impl DomainId {
    pub fn new(id: u32) -> Self {
        DomainId { id }
    }
}

impl IntegerVariable for DomainId {
    type AffineView = AffineView<Self>;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        assignment.get_lower_bound(*self)
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        assignment.get_upper_bound(*self)
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        assignment.is_value_in_domain(*self, value)
    }

    fn describe_domain(&self, assignment: &Assignments) -> Vec<IntegerPredicate> {
        assignment.get_domain_description(*self)
    }

    fn remove(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        assignment.remove_value_from_domain(*self, value, reason)
    }

    fn set_lower_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        assignment.tighten_lower_bound(*self, value, reason)
    }

    fn set_upper_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        assignment.tighten_upper_bound(*self, value, reason)
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>) {
        watchers.watch_all(*self, events);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent {
        event.unwrap()
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
