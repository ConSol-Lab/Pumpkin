use enumset::EnumSet;
use enumset::EnumSetType;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use crate::engine::Assignments;
use crate::engine::notifications::Watchers;
use crate::engine::variables::DomainId;
use crate::predicate;
use crate::predicates::PredicateConstructor;
use crate::propagation::DomainEvent;
use crate::propagation::OpaqueDomainEvent;
use crate::variables::AffineView;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

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

impl CheckerVariable<Predicate> for Predicate {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        atomic.get_domain() == self.get_domain()
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        if value == 0 {
            self.negate()
        } else if value >= 1 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        if value == 1 {
            self.negate()
        } else if value <= 0 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        if value == 1 {
            *self
        } else if value == 0 {
            self.negate()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        if value == 1 {
            self.negate()
        } else if value == 0 {
            *self
        } else {
            Predicate::trivially_true()
        }
    }

    fn induced_lower_bound(&self, variable_state: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(if variable_state.is_true(self) {
            1
        } else {
            Default::default()
        })
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<Predicate>) -> IntExt {
        IntExt::Int(if variable_state.is_true(&self.negate()) {
            0
        } else {
            1
        })
    }

    fn induced_fixed_value(&self, variable_state: &VariableState<Predicate>) -> Option<i32> {
        if variable_state.is_true(self) {
            Some(1)
        } else if variable_state.is_true(&self.negate()) {
            Some(0)
        } else {
            None
        }
    }

    fn induced_domain_contains(
        &self,
        variable_state: &VariableState<Predicate>,
        value: i32,
    ) -> bool {
        if value == 1 && !variable_state.is_true(&self.negate()) {
            true
        } else {
            value == 0 && !variable_state.is_true(self)
        }
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        _variable_state: &'state VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        std::iter::empty()
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        Some((0..=1).filter(|&value| {
            if value == 0 {
                !variable_state.is_true(self)
            } else if value == 1 {
                !variable_state.is_true(&!*self)
            } else {
                unreachable!()
            }
        }))
    }
}

impl IntegerVariable for Predicate {
    type AffineView = AffineView<Self>;

    /// Returns the lower bound represented as a 0-1 value.
    /// Literals that evaluate to true have a lower bound of 1.
    /// Literal that evaluate to false have a lower bound of 0.
    /// Unassigned literals have a lower bound of 0.
    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        if assignment.is_predicate_satisfied(*self) {
            1
        } else {
            0
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if assignment.is_predicate_satisfied_at_trail_position(*self, trail_position) {
            1
        } else {
            0
        }
    }

    /// Returns the upper bound represented as a 0-1 value.
    /// Literals that evaluate to true have an upper bound of 1.
    /// Literal that evaluate to false have a upper bound of 0.
    /// Unassigned literals have a upper bound of 1.
    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        if assignment.is_predicate_satisfied(self.negate()) {
            0
        } else {
            1
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if assignment.is_predicate_satisfied_at_trail_position(self.negate(), trail_position) {
            0
        } else {
            1
        }
    }

    /// Returns whether the input value, when interpreted as a bool,
    /// can be considered for the literal.
    /// Literals that evaluate to true only contain value 1.
    /// Literals that evaluate to false only contain value 0.
    /// Unassigned literals contain both values 0 and 1.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        if value == 0 {
            !assignment.is_predicate_satisfied(*self)
        } else if value == 1 {
            !assignment.is_predicate_satisfied(self.negate())
        } else {
            false
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        if value == 0 {
            !assignment.is_predicate_satisfied_at_trail_position(*self, trail_position)
        } else if value == 1 {
            !assignment.is_predicate_satisfied_at_trail_position(self.negate(), trail_position)
        } else {
            false
        }
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        (0..=1).filter(|&value| {
            if value == 0 {
                !assignment.is_predicate_satisfied(*self)
            } else if value == 1 {
                !assignment.is_predicate_satisfied(!*self)
            } else {
                unreachable!()
            }
        })
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_predicate(*self, events)
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        watchers.unwatch_predicate(*self);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        event.unwrap()
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        watchers.watch_predicate_backtrack(*self, events)
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
        if bound == 1 {
            *self
        } else if bound < 1 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 0 {
            self.negate()
        } else if bound > 0 {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 0 {
            self.negate()
        } else if bound == 1 {
            *self
        } else {
            Predicate::trivially_true()
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == 0 {
            *self
        } else if bound == 1 {
            self.negate()
        } else {
            Predicate::trivially_true()
        }
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
    use super::Predicate;
    use crate::predicate;
    use crate::variables::DomainId;

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
}
