use pumpkin_lib::assignments::AssignmentsInteger;
use pumpkin_lib::assignments::EmptyDomain;
use pumpkin_lib::assignments::IntDomainEvent;
use pumpkin_lib::assignments::OpaqueDomainEvent;
use pumpkin_lib::assignments::ReasonRef;
use pumpkin_lib::assignments::Watchers;
use pumpkin_lib::containers::EnumSet;
use pumpkin_lib::predicates::PredicateConstructor;
use pumpkin_lib::variables::AffineView;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::IntegerVariable;
use pumpkin_lib::variables::Literal;
use pumpkin_lib::variables::TransformableVariable;
use pyo3::pyclass;
use pyo3::pymethods;

/// An integer variable.
#[pyclass]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Variable(pub(crate) AffineView<DomainId>);

#[pymethods]
impl Variable {
    fn offset(&self, offset: i32) -> Variable {
        Variable(self.0.offset(offset))
    }
}

impl PredicateConstructor for Variable {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> pumpkin_lib::predicates::Predicate {
        self.0.lower_bound_predicate(bound)
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> pumpkin_lib::predicates::Predicate {
        self.0.upper_bound_predicate(bound)
    }

    fn equality_predicate(&self, bound: Self::Value) -> pumpkin_lib::predicates::Predicate {
        self.0.equality_predicate(bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> pumpkin_lib::predicates::Predicate {
        self.0.disequality_predicate(bound)
    }
}

impl TransformableVariable<Variable> for Variable {
    fn scaled(&self, scale: i32) -> Variable {
        Variable(self.0.scaled(scale))
    }

    fn offset(&self, offset: i32) -> Variable {
        Variable(self.0.offset(offset))
    }
}

impl IntegerVariable for Variable {
    type AffineView = Variable;

    fn lower_bound(&self, assignment: &AssignmentsInteger) -> i32 {
        self.0.lower_bound(assignment)
    }

    fn upper_bound(&self, assignment: &AssignmentsInteger) -> i32 {
        self.0.upper_bound(assignment)
    }

    fn contains(&self, assignment: &AssignmentsInteger, value: i32) -> bool {
        self.0.contains(assignment, value)
    }

    fn describe_domain(
        &self,
        assignment: &AssignmentsInteger,
    ) -> Vec<pumpkin_lib::predicates::Predicate> {
        self.0.describe_domain(assignment)
    }

    fn remove(
        &self,
        assignment: &mut AssignmentsInteger,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        self.0.remove(assignment, value, reason)
    }

    fn set_lower_bound(
        &self,
        assignment: &mut AssignmentsInteger,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        self.0.set_lower_bound(assignment, value, reason)
    }

    fn set_upper_bound(
        &self,
        assignment: &mut AssignmentsInteger,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        self.0.set_upper_bound(assignment, value, reason)
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>) {
        self.0.watch_all(watchers, events)
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>) {
        self.0.watch_all_backtrack(watchers, events)
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent {
        self.0.unpack_event(event)
    }
}

/// A boolean variable.
#[pyclass]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Boolean(pub(crate) Literal);
