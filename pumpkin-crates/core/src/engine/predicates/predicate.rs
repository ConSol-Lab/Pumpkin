use enumset::EnumSet;
use enumset::EnumSetType;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use crate::engine::Assignments;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::variables::AffineView;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::TransformableVariable;
use crate::predicate;
use crate::propagation::DomainEvent;
use crate::propagation::EventDispatcher;
use crate::propagation::EventTarget;
use crate::propagation::LocalId;

/// Representation of a domain operation, also known as an atomic constraint. It is a triple
/// ([`DomainId`], [`PredicateType`], value).
///
/// To create a [`Predicate`], use [Predicate::new] or the more concise [predicate!] macro.
///
/// ## Order
/// Predicates have a well-defined order. They are first ordered by the domain, and then by
/// predicate type, and finally by the value. The order is chosen such that for a fixed domain `x`,
/// predicates are ordered as follows:
/// [>= 5], [>= 7], [!= 2], [!= 3], [== 5], [!= 7], [<= 6], [<= 10]
///
/// From the order, we get the lower-bound predicates first, ordered by non-decreasing bound, then
/// the (not-)equal predicates, ordered by non-decreasing bound, then the upper-bound predicates,
/// ordered by non-increasing bounds.
///
/// ## As a variable
/// A [`Predicate`] can be used as a Boolean variable which is true if and only if the predicate
/// is true.
///
/// When used as an [`IntegerVariable`], a predicate is a 0-1 variable which is 1 if the predicate
/// is true and 0 if the predicate is false.
///
/// As for a 0-1 domain, a propagator registered for events on a predicate is notified of the
/// lower-bound and assign events when the predicate becomes true, and of the upper-bound and assign
/// events when it becomes false; each registered event is triggered once. Similarly, each
/// registered event is triggered once on backtracking when the predicate becomes unassigned.
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct Predicate {
    /// The two most significant bits of the id stored in the [`Predicate`] contains the type of
    /// predicate.
    id: u32,
    value: i32,
}

const LOWER_BOUND_CODE: u8 = PredicateType::LowerBound as u8;
const UPPER_BOUND_CODE: u8 = PredicateType::UpperBound as u8;
const NOT_EQUAL_CODE: u8 = PredicateType::NotEqual as u8;
const EQUAL_CODE: u8 = PredicateType::Equal as u8;

impl Predicate {
    /// Creates a new [`Predicate`] (also known as atomic constraint) which represents a domain
    /// operation.
    pub fn new(id: DomainId, predicate_type: PredicateType, value: i32) -> Self {
        let code = predicate_type as u8;
        let id = id.id() | (code as u32) << 30;
        Self { id, value }
    }

    /// Returns `true` if `self` implies `other`.
    ///
    /// # Example
    /// ```
    /// # use pumpkin_core::variables::DomainId;
    /// # use pumpkin_core::predicate;
    /// let x = DomainId::new(0);
    ///
    /// assert!(predicate![x >= 5].implies(predicate![x >= 3]));
    /// assert!(predicate![x >= 5].implies(predicate![x != 1]));
    /// assert!(predicate![x == 5].implies(predicate![x <= 5]));
    /// ```
    pub fn implies(&self, other: Predicate) -> bool {
        if self.get_domain() != other.get_domain() {
            // Predicates only imply other predicates on the same domain.
            return false;
        }

        match self.get_predicate_type() {
            PredicateType::LowerBound => match other.get_predicate_type() {
                PredicateType::LowerBound => {
                    self.get_right_hand_side() >= other.get_right_hand_side()
                }
                PredicateType::NotEqual => self.get_right_hand_side() > other.get_right_hand_side(),
                PredicateType::UpperBound | PredicateType::Equal => false,
            },
            PredicateType::UpperBound => match other.get_predicate_type() {
                PredicateType::UpperBound => {
                    self.get_right_hand_side() <= other.get_right_hand_side()
                }
                PredicateType::NotEqual => self.get_right_hand_side() < other.get_right_hand_side(),
                PredicateType::LowerBound | PredicateType::Equal => false,
            },
            PredicateType::NotEqual => {
                other.get_predicate_type() == PredicateType::NotEqual
                    && self.get_right_hand_side() == other.get_right_hand_side()
            }
            PredicateType::Equal => match other.get_predicate_type() {
                PredicateType::LowerBound => {
                    self.get_right_hand_side() >= other.get_right_hand_side()
                }
                PredicateType::UpperBound => {
                    self.get_right_hand_side() <= other.get_right_hand_side()
                }
                PredicateType::NotEqual => {
                    self.get_right_hand_side() != other.get_right_hand_side()
                }
                PredicateType::Equal => self.get_right_hand_side() == other.get_right_hand_side(),
            },
        }
    }

    fn get_type_code(&self) -> u8 {
        (self.id >> 30) as u8
    }

    pub fn get_predicate_type(&self) -> PredicateType {
        (*self).into()
    }

    fn is_bound_predicate(&self) -> bool {
        self.is_upper_bound_predicate() || self.is_lower_bound_predicate()
    }
}

impl PartialOrd for Predicate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Predicate {
    /// See [`Predicate`] for details on the order.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.get_domain().cmp(&other.get_domain()) {
            std::cmp::Ordering::Equal => {
                if self.is_bound_predicate() || other.is_bound_predicate() {
                    match self.get_type_code().cmp(&other.get_type_code()) {
                        std::cmp::Ordering::Equal => {
                            self.get_right_hand_side().cmp(&other.get_right_hand_side())
                        }
                        ordering @ (std::cmp::Ordering::Less | std::cmp::Ordering::Greater) => {
                            ordering
                        }
                    }
                } else {
                    self.get_right_hand_side().cmp(&other.get_right_hand_side())
                }
            }

            ordering @ (std::cmp::Ordering::Less | std::cmp::Ordering::Greater) => ordering,
        }
    }
}

#[derive(Debug, Hash, EnumSetType)]
#[repr(u8)]
#[enumset(repr = "u8")]
pub enum PredicateType {
    // Should correspond with the codes defined previously; `EnumSetType` requires that literals
    // are used and not expressions
    LowerBound = 0,
    NotEqual = 1,
    Equal = 2,
    UpperBound = 3,
}

impl From<DomainEvent> for PredicateType {
    fn from(value: DomainEvent) -> Self {
        match value {
            DomainEvent::Assign => PredicateType::Equal,
            DomainEvent::LowerBound => PredicateType::LowerBound,
            DomainEvent::UpperBound => PredicateType::UpperBound,
            DomainEvent::Removal => PredicateType::NotEqual,
        }
    }
}

impl PredicateType {
    pub fn is_lower_bound(&self) -> bool {
        matches!(self, PredicateType::LowerBound)
    }

    pub fn is_upper_bound(&self) -> bool {
        matches!(self, PredicateType::UpperBound)
    }

    pub fn is_disequality(&self) -> bool {
        matches!(self, PredicateType::NotEqual)
    }

    pub(crate) fn into_predicate(
        self,
        domain: DomainId,
        assignments: &Assignments,
        removed_value: Option<i32>,
    ) -> Predicate {
        match self {
            PredicateType::LowerBound => {
                predicate!(domain >= assignments.get_lower_bound(domain))
            }
            PredicateType::UpperBound => predicate!(domain <= assignments.get_upper_bound(domain)),
            PredicateType::NotEqual => predicate!(
                domain
                    != removed_value
                        .expect("For a `NotEqual`, the removed value should be provided")
            ),
            PredicateType::Equal => predicate!(
                domain
                    == assignments.get_assigned_value(&domain).expect(
                        "Expected domain to be assigned when creating an `Equal` predicate"
                    )
            ),
        }
    }
}

impl From<Predicate> for PredicateType {
    fn from(value: Predicate) -> Self {
        match value.get_type_code() {
            LOWER_BOUND_CODE => Self::LowerBound,
            UPPER_BOUND_CODE => Self::UpperBound,
            EQUAL_CODE => Self::Equal,
            NOT_EQUAL_CODE => Self::NotEqual,
            code => panic!("Unknown type code {code}"),
        }
    }
}

impl PredicateType {
    pub const fn into_bits(self) -> u8 {
        self as _
    }

    pub const fn from_bits(value: u8) -> PredicateType {
        match value {
            LOWER_BOUND_CODE => PredicateType::LowerBound,
            UPPER_BOUND_CODE => PredicateType::UpperBound,
            EQUAL_CODE => PredicateType::Equal,
            NOT_EQUAL_CODE => PredicateType::NotEqual,
            _ => panic!("Unknown code"),
        }
    }
}

impl Predicate {
    pub(crate) fn is_mutually_exclusive_with(self, other: Predicate) -> bool {
        let domain_id = self.get_domain();
        let rhs = self.get_right_hand_side();

        let domain_id_other = other.get_domain();
        let rhs_other = other.get_right_hand_side();

        if domain_id != domain_id_other {
            // Domain Ids do not match
            return false;
        }

        match (self.get_predicate_type(), other.get_predicate_type()) {
            (PredicateType::LowerBound, PredicateType::LowerBound)
            | (PredicateType::LowerBound, PredicateType::NotEqual)
            | (PredicateType::UpperBound, PredicateType::UpperBound)
            | (PredicateType::UpperBound, PredicateType::NotEqual)
            | (PredicateType::NotEqual, PredicateType::LowerBound)
            | (PredicateType::NotEqual, PredicateType::UpperBound)
            | (PredicateType::NotEqual, PredicateType::NotEqual) => false,
            (PredicateType::LowerBound, PredicateType::UpperBound) => rhs > rhs_other,
            (PredicateType::UpperBound, PredicateType::LowerBound) => rhs_other > rhs,
            (PredicateType::LowerBound, PredicateType::Equal) => rhs > rhs_other,
            (PredicateType::Equal, PredicateType::LowerBound) => rhs_other > rhs,
            (PredicateType::UpperBound, PredicateType::Equal) => rhs < rhs_other,
            (PredicateType::Equal, PredicateType::UpperBound) => rhs_other < rhs,
            (PredicateType::NotEqual, PredicateType::Equal)
            | (PredicateType::Equal, PredicateType::NotEqual) => rhs == rhs_other,
            (PredicateType::Equal, PredicateType::Equal) => rhs != rhs_other,
        }
    }
    pub fn is_equality_predicate(&self) -> bool {
        self.get_type_code() == EQUAL_CODE
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        self.get_type_code() == LOWER_BOUND_CODE
    }

    pub fn is_upper_bound_predicate(&self) -> bool {
        self.get_type_code() == UPPER_BOUND_CODE
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        self.get_type_code() == NOT_EQUAL_CODE
    }

    /// Returns the [`DomainId`] of the [`Predicate`]
    pub fn get_domain(&self) -> DomainId {
        DomainId::new(0b00111111_11111111_11111111_11111111 & self.id)
    }

    pub fn get_right_hand_side(&self) -> i32 {
        self.value
    }

    pub fn trivially_true() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        let domain_id = DomainId::new(0);
        predicate!(domain_id == 1)
    }

    pub fn trivially_false() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        let domain_id = DomainId::new(0);
        predicate!(domain_id != 1)
    }
}

impl std::ops::Not for Predicate {
    type Output = Predicate;

    fn not(self) -> Self::Output {
        let domain_id = self.get_domain();
        let value = self.get_right_hand_side();

        match self.get_predicate_type() {
            PredicateType::LowerBound => predicate!(domain_id <= value - 1),
            PredicateType::UpperBound => predicate!(domain_id >= value + 1),
            PredicateType::NotEqual => predicate!(domain_id == value),
            PredicateType::Equal => predicate!(domain_id != value),
        }
    }
}

impl std::fmt::Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Predicate::trivially_true() {
            write!(f, "[True]")
        } else if *self == Predicate::trivially_false() {
            write!(f, "[False]")
        } else {
            let domain_id = self.get_domain();
            let rhs = self.get_right_hand_side();

            match self.get_predicate_type() {
                PredicateType::LowerBound => write!(f, "[{domain_id} >= {rhs}]"),
                PredicateType::UpperBound => write!(f, "[{domain_id} <= {rhs}]"),
                PredicateType::NotEqual => write!(f, "[{domain_id} != {rhs}]"),
                PredicateType::Equal => write!(f, "[{domain_id} == {rhs}]"),
            }
        }
    }
}

impl std::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl AtomicConstraint for Predicate {
    type Identifier = DomainId;

    fn identifier(&self) -> Self::Identifier {
        self.get_domain()
    }

    fn comparison(&self) -> pumpkin_checking::Comparison {
        match self.get_predicate_type() {
            PredicateType::LowerBound => pumpkin_checking::Comparison::GreaterEqual,
            PredicateType::UpperBound => pumpkin_checking::Comparison::LessEqual,
            PredicateType::NotEqual => pumpkin_checking::Comparison::NotEqual,
            PredicateType::Equal => pumpkin_checking::Comparison::Equal,
        }
    }

    fn value(&self) -> i32 {
        self.get_right_hand_side()
    }

    fn negate(&self) -> Self {
        !*self
    }
}

/// Returns whether `value` is in the 0-1 domain of a predicate with the given truth value.
fn truth_value_contains(truth_value: Option<bool>, value: i32) -> bool {
    match value {
        0 => truth_value != Some(true),
        1 => truth_value != Some(false),
        _ => false,
    }
}

impl EventTarget for Predicate {
    fn register(
        &self,
        registration: &mut impl EventDispatcher,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    ) {
        registration.register_predicate_events(*self, events, local_id);
    }
}

impl CheckerVariable<Predicate> for Predicate {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        atomic.get_domain() == self.get_domain()
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        self.upper_bound_predicate(value)
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        self.lower_bound_predicate(value)
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        self.disequality_predicate(value)
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        self.equality_predicate(value)
    }

    fn induced_lower_bound(&self, domains: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(domains.is_true(self) as i32)
    }

    fn induced_upper_bound(&self, domains: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(!domains.is_true(&!*self) as i32)
    }

    fn induced_fixed_value(&self, domains: &VariableState<Predicate>) -> Option<i32> {
        if domains.is_true(self) {
            Some(1)
        } else if domains.is_true(&!*self) {
            Some(0)
        } else {
            None
        }
    }

    fn induced_domain_contains(&self, domains: &VariableState<Predicate>, value: i32) -> bool {
        let truth_value = self
            .induced_fixed_value(domains)
            .map(|fixed_value| fixed_value == 1);
        truth_value_contains(truth_value, value)
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        _domains: &'state VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        std::iter::empty()
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        domains: &'state VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        Some((0..=1).filter(|&value| self.induced_domain_contains(domains, value)))
    }
}

impl IntegerVariable for Predicate {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    ///
    /// Predicates which are true have a lower bound of 1.
    /// Predicates which are false have an upper bound of 0.
    ///
    /// Unassigned predicates have a lower bound of 0 and an upper-bound of 1.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        (assignment.evaluate_predicate(*self) == Some(true)) as i32
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        (assignment.evaluate_predicate_at_trail_position(*self, trail_position) == Some(true))
            as i32
    }

    /// Returns the upper bound represented as a 0-1 value.
    ///
    /// Predicates which are true have a lower bound of 1.
    /// Predicates which are false have an upper bound of 0.
    ///
    /// Unassigned predicates have a lower bound of 0 and an upper-bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        (assignment.evaluate_predicate(*self) != Some(false)) as i32
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        (assignment.evaluate_predicate_at_trail_position(*self, trail_position) != Some(false))
            as i32
    }

    /// Returns whether the input value, when interpreted as a bool, can be considered for the
    /// predicate.
    ///
    /// Predicates which are true only contain value 1.
    /// Predicates which are false only contain value 0.
    ///
    /// Unassigned predicates contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        truth_value_contains(assignment.evaluate_predicate(*self), value)
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        truth_value_contains(
            assignment.evaluate_predicate_at_trail_position(*self, trail_position),
            value,
        )
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        let truth_value = assignment.evaluate_predicate(*self);
        (0..=1).filter(move |&value| truth_value_contains(truth_value, value))
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        watchers.unwatch_predicate_events(*self)
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        event.unwrap()
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_predicate_events_backtrack(*self, events)
    }

    fn get_holes_at_current_checkpoint(
        &self,
        _assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }

    fn get_holes(&self, _assignments: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }
}

impl PredicateConstructor for Predicate {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            ..=0 => Predicate::trivially_true(),
            1 => *self,
            _ => Predicate::trivially_false(),
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            1.. => Predicate::trivially_true(),
            0 => !*self,
            _ => Predicate::trivially_false(),
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        match bound {
            1 => *self,
            0 => !*self,
            _ => Predicate::trivially_false(),
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        !self.equality_predicate(bound)
    }
}

impl TransformableVariable<AffineView<Predicate>> for Predicate {
    fn scaled(&self, scale: i32) -> AffineView<Predicate> {
        AffineView::new(*self, scale, 0)
    }

    fn offset(&self, offset: i32) -> AffineView<Predicate> {
        AffineView::new(*self, 1, offset)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::engine::PropagationStatusCP;
    use crate::engine::State;
    use crate::propagation::DomainEvents;
    use crate::propagation::Domains;
    use crate::propagation::EnqueueDecision;
    use crate::propagation::EventsToRegister;
    use crate::propagation::NotificationContext;
    use crate::propagation::PropagationContext;
    use crate::propagation::Propagator;
    use crate::propagation::PropagatorConstructor;
    use crate::propagation::PropagatorConstructorContext;
    use crate::propagation::PropagatorSpec;
    use crate::propagation::RuntimeCheckers;
    use crate::state::PropagatorHandle;

    #[test]
    fn are_mutually_exclusive() {
        let domain_id = DomainId::new(0);

        assert!(!predicate!(domain_id >= 5).is_mutually_exclusive_with(predicate!(domain_id >= 7)));
        assert!(!predicate!(domain_id >= 5).is_mutually_exclusive_with(predicate!(domain_id != 2)));
        assert!(!predicate!(domain_id <= 5).is_mutually_exclusive_with(predicate!(domain_id <= 8)));
        assert!(!predicate!(domain_id <= 5).is_mutually_exclusive_with(predicate!(domain_id != 8)));
        assert!(!predicate!(domain_id != 9).is_mutually_exclusive_with(predicate!(domain_id >= 8)));
        assert!(!predicate!(domain_id != 9).is_mutually_exclusive_with(predicate!(domain_id <= 8)));
        assert!(!predicate!(domain_id != 9).is_mutually_exclusive_with(predicate!(domain_id != 8)));

        assert!(predicate!(domain_id <= 7).is_mutually_exclusive_with(predicate!(domain_id >= 8)));
        assert!(predicate!(domain_id >= 8).is_mutually_exclusive_with(predicate!(domain_id <= 7)));

        assert!(predicate!(domain_id >= 8).is_mutually_exclusive_with(predicate!(domain_id == 7)));
        assert!(predicate!(domain_id == 7).is_mutually_exclusive_with(predicate!(domain_id >= 8)));

        assert!(predicate!(domain_id == 7).is_mutually_exclusive_with(predicate!(domain_id <= 6)));
        assert!(predicate!(domain_id <= 6).is_mutually_exclusive_with(predicate!(domain_id == 7)));

        assert!(predicate!(domain_id != 8).is_mutually_exclusive_with(predicate!(domain_id == 8)));
        assert!(predicate!(domain_id == 8).is_mutually_exclusive_with(predicate!(domain_id != 8)));

        assert!(predicate!(domain_id == 7).is_mutually_exclusive_with(predicate!(domain_id == 8)));
    }

    #[test]
    fn negating_trivially_true_predicate() {
        let trivially_true = Predicate::trivially_true();
        let trivially_false = Predicate::trivially_false();
        assert!(!trivially_true == trivially_false);
    }

    #[test]
    fn negating_trivially_false_predicate() {
        let trivially_true = Predicate::trivially_true();
        let trivially_false = Predicate::trivially_false();
        assert!(!trivially_false == trivially_true);
    }

    #[test]
    fn predicates_over_same_domain_are_ordered_by_increasing_lower_bound() {
        let x = DomainId::new(0);
        let p1 = predicate![x >= 4];
        let p2 = predicate![x >= 6];
        assert!(p1 < p2);
    }

    #[test]
    fn not_equal_predicates_are_bigger_than_lower_bounds() {
        let x = DomainId::new(0);
        let p1 = predicate![x >= 4];
        let p2 = predicate![x != 6];
        let p3 = predicate![x != 2];

        assert!(p1 < p2);
        assert!(p1 < p3);
    }

    #[test]
    fn not_equal_predicates_are_ordered_by_rhs() {
        let x = DomainId::new(0);
        let p1 = predicate![x != 6];
        let p2 = predicate![x != 2];

        assert!(p1 > p2);
    }

    #[test]
    fn equal_predicates_are_ordered_by_rhs() {
        let x = DomainId::new(0);
        let p1 = predicate![x == 6];
        let p2 = predicate![x == 2];

        assert!(p1 > p2);
    }

    #[test]
    fn equal_predicates_bigger_than_lower_bounds() {
        let x = DomainId::new(0);
        let p1 = predicate![x == 6];
        let p2 = predicate![x >= 2];

        assert!(p1 > p2);
    }

    #[test]
    fn equal_predicates_smaller_than_upper_bounds() {
        let x = DomainId::new(0);
        let p1 = predicate![x == 6];
        let p2 = predicate![x <= 2];

        assert!(p1 < p2);
    }

    #[test]
    fn tighter_upper_bound_is_smaller() {
        let x = DomainId::new(0);
        let p1 = predicate![x <= 6];
        let p2 = predicate![x <= 2];

        assert!(p1 > p2);
    }

    #[test]
    fn implies_over_different_domains_is_false() {
        let x = DomainId::new(0);
        let y = DomainId::new(1);

        assert!(!predicate![x >= 5].implies(predicate![y >= 4]));
    }

    #[test]
    fn lower_bound_implies() {
        let x = DomainId::new(0);

        // Implies weaker bounds
        assert!(predicate![x >= 5].implies(predicate![x >= 5]));
        assert!(predicate![x >= 5].implies(predicate![x >= 4]));

        // Implies not-equals below bound
        assert!(predicate![x >= 5].implies(predicate![x != 4]));
        assert!(predicate![x >= 5].implies(predicate![x != 3]));

        // Does not imply stronger bounds
        assert!(!predicate![x >= 5].implies(predicate![x >= 6]));

        // Does not imply not-equals at or above bound
        assert!(!predicate![x >= 5].implies(predicate![x != 6]));
        assert!(!predicate![x >= 5].implies(predicate![x != 5]));

        // Does not imply equals
        assert!(!predicate![x >= 5].implies(predicate![x == 6]));
        assert!(!predicate![x >= 5].implies(predicate![x == 5]));
        assert!(!predicate![x >= 5].implies(predicate![x == 4]));
    }

    #[test]
    fn upper_bound_implies() {
        let x = DomainId::new(0);

        // Implies weaker bounds
        assert!(predicate![x <= 5].implies(predicate![x <= 5]));
        assert!(predicate![x <= 5].implies(predicate![x <= 6]));

        // Implies not-equals above bound
        assert!(predicate![x <= 5].implies(predicate![x != 6]));
        assert!(predicate![x <= 5].implies(predicate![x != 7]));

        // Does not imply stronger bounds
        assert!(!predicate![x <= 5].implies(predicate![x <= 4]));

        // Does not imply not-equals at or below bound
        assert!(!predicate![x <= 5].implies(predicate![x != 4]));
        assert!(!predicate![x <= 5].implies(predicate![x != 5]));

        // Does not imply equals
        assert!(!predicate![x <= 5].implies(predicate![x == 6]));
        assert!(!predicate![x <= 5].implies(predicate![x == 5]));
        assert!(!predicate![x <= 5].implies(predicate![x == 4]));
    }

    #[test]
    fn equals_implies() {
        let x = DomainId::new(0);

        // Implies lower bounds at or below
        assert!(predicate![x == 5].implies(predicate![x >= 5]));
        assert!(predicate![x == 5].implies(predicate![x >= 4]));

        // Implies upper bounds at or above
        assert!(predicate![x == 5].implies(predicate![x <= 5]));
        assert!(predicate![x == 5].implies(predicate![x <= 6]));

        // Implies not-equals
        assert!(predicate![x == 5].implies(predicate![x != 4]));
        assert!(predicate![x == 5].implies(predicate![x != 6]));

        // Does not imply not-equals at bound
        assert!(!predicate![x == 5].implies(predicate![x != 5]));

        // Does not lower bounds above value
        assert!(!predicate![x == 5].implies(predicate![x >= 6]));

        // Does not upper bounds below value
        assert!(!predicate![x == 5].implies(predicate![x <= 4]));
    }

    #[test]
    fn not_equals_implies_nothing() {
        let x = DomainId::new(0);

        assert!(!predicate![x != 5].implies(predicate![x <= 4]));
        assert!(!predicate![x != 5].implies(predicate![x <= 5]));
        assert!(!predicate![x != 5].implies(predicate![x <= 6]));

        assert!(!predicate![x != 5].implies(predicate![x >= 4]));
        assert!(!predicate![x != 5].implies(predicate![x >= 5]));
        assert!(!predicate![x != 5].implies(predicate![x >= 6]));

        assert!(!predicate![x != 5].implies(predicate![x == 4]));
        assert!(!predicate![x != 5].implies(predicate![x == 5]));
        assert!(!predicate![x != 5].implies(predicate![x == 6]));

        assert!(!predicate![x != 5].implies(predicate![x != 4]));
        assert!(!predicate![x != 5].implies(predicate![x != 6]));

        assert!(predicate![x != 5].implies(predicate![x != 5]));
    }

    #[test]
    fn bounds_follow_the_truth_value_of_the_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        assert_eq!(predicate.lower_bound(&state.assignments), 0);
        assert_eq!(predicate.upper_bound(&state.assignments), 1);
        assert_eq!(
            predicate
                .iterate_domain(&state.assignments)
                .collect::<Vec<_>>(),
            vec![0, 1]
        );

        let _ = state.post(predicate![x >= 7]).expect("non-empty domain");

        assert_eq!(predicate.lower_bound(&state.assignments), 1);
        assert_eq!(predicate.upper_bound(&state.assignments), 1);
        assert!(!predicate.contains(&state.assignments, 0));
        assert!((!predicate).contains(&state.assignments, 0));
        assert_eq!((!predicate).upper_bound(&state.assignments), 0);
    }

    #[test]
    fn bounds_at_trail_position_use_the_old_truth_value() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x != 5];

        let trail_position = state.assignments.num_trail_entries() - 1;
        state.new_checkpoint();
        let _ = state.post(predicate![x <= 4]).expect("non-empty domain");

        assert_eq!(predicate.lower_bound(&state.assignments), 1);
        assert_eq!(
            predicate.lower_bound_at_trail_position(&state.assignments, trail_position),
            0
        );
        assert!(predicate.contains_at_trail_position(&state.assignments, 0, trail_position));
    }

    #[test]
    fn negation_is_canonical() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);

        for predicate in [
            predicate![x >= 3],
            predicate![x <= 3],
            predicate![x == 3],
            predicate![x != 3],
            Predicate::trivially_true(),
        ] {
            assert_eq!(!!predicate, predicate);
        }
    }

    #[test]
    fn predicates_over_the_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let p = predicate![x >= 3];

        assert_eq!(predicate![p >= 0], Predicate::trivially_true());
        assert_eq!(predicate![p >= 1], p);
        assert_eq!(predicate![p >= 2], Predicate::trivially_false());
        assert_eq!(predicate![p <= 1], Predicate::trivially_true());
        assert_eq!(predicate![p <= 0], !p);
        assert_eq!(predicate![p <= -1], Predicate::trivially_false());
        assert_eq!(predicate![p == 1], p);
        assert_eq!(predicate![p == 0], !p);
        assert_eq!(predicate![p == 2], Predicate::trivially_false());
        assert_eq!(predicate![p != 1], !p);
        assert_eq!(predicate![p != 0], p);
        assert_eq!(predicate![p != 2], Predicate::trivially_true());
    }

    /// Records the events and backtrack events it receives for its predicate.
    #[derive(Clone, Debug)]
    struct EventRecorder {
        predicate: Predicate,
        registered_events: DomainEvents,
        events: Vec<DomainEvent>,
        backtrack_events: Vec<DomainEvent>,
    }

    impl PropagatorConstructor for EventRecorder {
        type PropagatorImpl = Self;

        fn create(self, mut context: PropagatorConstructorContext) -> PropagatorSpec<Self> {
            context.register_backtrack(self.predicate, self.registered_events, LocalId::from(0));

            PropagatorSpec {
                registration: EventsToRegister::builder()
                    .add(&self.predicate, self.registered_events, LocalId::from(0))
                    .build(),
                checkers: RuntimeCheckers::empty(),
                propagator: self,
            }
        }
    }

    impl Propagator for EventRecorder {
        fn name(&self) -> &str {
            "EventRecorder"
        }

        fn propagate_from_scratch(&self, _context: PropagationContext) -> PropagationStatusCP {
            Ok(())
        }

        fn notify(
            &mut self,
            _context: NotificationContext,
            _local_id: LocalId,
            event: OpaqueDomainEvent,
        ) -> EnqueueDecision {
            self.events.push(self.predicate.unpack_event(event));
            EnqueueDecision::Skip
        }

        fn notify_backtrack(
            &mut self,
            _context: Domains,
            _local_id: LocalId,
            event: OpaqueDomainEvent,
        ) {
            self.backtrack_events
                .push(self.predicate.unpack_event(event));
        }
    }

    /// Posts each predicate of `to_post` at its own checkpoint, backtracks to the root, and returns
    /// the events and backtrack events received for `predicate`, which is registered for the given
    /// events.
    fn record_events(
        predicate: Predicate,
        registered_events: DomainEvents,
        state: &mut State,
        to_post: &[Predicate],
    ) -> (Vec<DomainEvent>, Vec<DomainEvent>) {
        let handle: PropagatorHandle<EventRecorder> = state.add_propagator(EventRecorder {
            predicate,
            registered_events,
            events: vec![],
            backtrack_events: vec![],
        });

        state.propagate_to_fixed_point().expect("no conflict");
        for &posted in to_post {
            state.new_checkpoint();
            let _ = state.post(posted).expect("non-empty domain");
            state.propagate_to_fixed_point().expect("no conflict");
        }
        let _ = state.restore_to(0);

        let recorder = state.get_propagator(handle).expect("propagator exists");
        (recorder.events.clone(), recorder.backtrack_events.clone())
    }

    #[test]
    fn single_events_when_predicate_becomes_true_and_is_undone() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[
                predicate![x >= 3],
                predicate![x >= 6],
                predicate![x <= 8],
                predicate![x >= 7],
            ],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::LowerBound], vec![DomainEvent::LowerBound])
        );
    }

    #[test]
    fn single_events_when_predicate_becomes_false_and_is_undone() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x <= 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[predicate![x >= 7], predicate![x >= 8]],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::UpperBound], vec![DomainEvent::UpperBound])
        );
    }

    #[test]
    fn single_events_for_equality_predicate() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x == 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[predicate![x != 4], predicate![x >= 5], predicate![x <= 5]],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::LowerBound], vec![DomainEvent::LowerBound])
        );
    }

    #[test]
    fn no_events_when_predicate_stays_unassigned() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::BOUNDS,
            &mut state,
            &[predicate![x >= 3], predicate![x <= 8]],
        );

        assert_eq!(events, (vec![], vec![]));
    }

    #[test]
    fn assign_events_are_triggered_alongside_bound_events() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::ANY_INT,
            &mut state,
            &[predicate![x >= 3], predicate![x >= 6]],
        );

        assert_eq!(
            events,
            (
                vec![DomainEvent::LowerBound, DomainEvent::Assign],
                vec![DomainEvent::LowerBound, DomainEvent::Assign]
            )
        );
    }

    #[test]
    fn only_assign_events_are_triggered_when_only_assign_is_registered() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let predicate = predicate![x >= 5];

        let events = record_events(
            predicate,
            DomainEvents::ASSIGN,
            &mut state,
            &[predicate![x <= 4]],
        );

        assert_eq!(
            events,
            (vec![DomainEvent::Assign], vec![DomainEvent::Assign])
        );
    }

    #[test]
    fn no_events_for_predicate_which_is_fixed_at_registration() {
        // `[x != 2]` is true at the root, and its negation `[x == 2]` is false at the root.
        for negate in [false, true] {
            let mut state = State::default();
            let x = state.new_sparse_variable(vec![1, 3], None);
            let predicate = predicate![x != 2];
            let predicate = if negate { !predicate } else { predicate };

            let events = record_events(
                predicate,
                DomainEvents::ANY_INT,
                &mut state,
                &[predicate![x >= 3]],
            );

            assert_eq!(events, (vec![], vec![]), "negated: {negate}");
        }
    }

    #[test]
    fn no_events_for_predicate_which_is_fixed_at_the_root_when_registered_during_search() {
        // `[x != 2]` is true at the root, and its negation `[x == 2]` is false at the root.
        for negate in [false, true] {
            let mut state = State::default();
            let x = state.new_sparse_variable(vec![1, 3], None);
            let predicate = predicate![x != 2];
            let predicate = if negate { !predicate } else { predicate };
            state.new_checkpoint();

            let events = record_events(
                predicate,
                DomainEvents::ANY_INT,
                &mut state,
                &[predicate![x >= 3]],
            );

            assert_eq!(events, (vec![], vec![]), "negated: {negate}");
        }
    }
}
