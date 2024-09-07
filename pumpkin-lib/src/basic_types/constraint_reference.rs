use std::fmt::Debug;
use std::fmt::Formatter;

use bitfield::Bit;
use bitfield::BitMut;
use bitfield::BitRange;

use crate::basic_types::ClauseReference;
#[cfg(doc)]
use crate::basic_types::ConflictInfo;
use crate::engine::reason::ReasonRef;
use crate::pumpkin_assert_moderate;

// TODO: Point 2 and 3 can be improved, there is no need to allocate an entire bit for CP
// propagators; instead a maximum propagator value could be used, and the solver can simply
// determine whether the code is smaller than that value this was done in the code before
// introducing binary clauses

/// A [`ConstraintReference`] may refer to:
/// - a Virtual Binary Clause
/// - an Allocated Clause
/// - a Propagator
///
/// Note that the user can only distinguish between a [`ConstraintReference`] to a clause and a
/// propagator (but not whether a virtual or allocated clause).
///
/// The idea is to pack all this information into 32 bits which is done in the following way:
/// 1. Virtual Binary Clause: the 31st bit is one (31st bit -> most significant bit); the remaining
///    31 bits encode a [`Literal`] that is part of the binary clause while the other literal of the
///    binary clause is to be recovered from the data structure that stores this constraint
///    reference.
///
///    For example, if [`ConstraintReference`] `r` is used as the reason for propagating variable
///    `x`, then the binary clause is `(x v r)`.
/// 2. Propagator: the 31st bit is zero, and the 30th bit is one; the remaining 30 bits encode the
///    reason reference.
/// 3. Allocated Clause: both the 31st and 30th bit are zero; the remaining 30 bits encode the
///    clause ID.
///
///    This system is used for simplicity of implementation, however, the drawback of the
///    simplification is that we can store less clauses since we can only go up to 2^30.
///
///    There is potential of almost doubling this number with proper care
///
/// Note: having both the 30th and 31st bit set to one cannot take occur: this combination could be
/// used in the future for some other indicator but in that case then the binary
/// clause will only have 30 bits to work with, whereas currently it has 31 bits
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstraintReference {
    code: u32,
}

impl Debug for ConstraintReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ConstraintReference")?;
        if self.is_cp_reason() {
            write!(f, "::Reason({:?})", self.get_reason_ref())
        } else if self.is_virtual_binary_clause() {
            write!(
                f,
                "::VirtualBinaryClause({})",
                self.as_clause_reference()
                    .get_virtual_binary_clause_literal()
            )
        } else {
            debug_assert!(self.is_allocated_clause());
            write!(f, "::AllocatedClause({})", self.get_code())
        }
    }
}

/// Methods to create a constraint reference
impl ConstraintReference {
    // TODO: replace with a NonZeroU32 and Option
    pub const NULL: ConstraintReference = ConstraintReference { code: 0 };

    pub fn create_standard_clause_reference(clause_id: u32) -> ConstraintReference {
        pumpkin_assert_moderate!(ConstraintReference::is_valid_allocated_clause_id(clause_id));
        ConstraintReference { code: clause_id }
    }

    pub fn create_reason_reference(reason_ref: ReasonRef) -> ConstraintReference {
        let reason_index = reason_ref.0;

        let mut code = reason_index;
        code.set_bit(30, true); // the 31st bit is zero, and the 30th bit is one
        ConstraintReference { code }
    }
}

/// Methods to retrieve information stored in the constraint reference
impl ConstraintReference {
    pub fn is_null(&self) -> bool {
        self.code == 0
    }

    pub fn is_clause(&self) -> bool {
        self.is_virtual_binary_clause() || self.is_allocated_clause()
    }

    fn is_virtual_binary_clause(&self) -> bool {
        self.code.bit(31)
    }

    fn is_allocated_clause(&self) -> bool {
        ConstraintReference::two_most_significant_bits(self.code) == 0
    }

    pub fn is_cp_reason(&self) -> bool {
        ConstraintReference::two_most_significant_bits(self.code) == 1
    }

    pub fn get_reason_ref(&self) -> ReasonRef {
        pumpkin_assert_moderate!(self.is_cp_reason());
        let mut id = self.code;
        // clear the 30th bit, the 31st bit is assumed to already be cleared
        id.set_bit(30, false);
        ReasonRef(id)
    }

    // for internal purposes, not to be called usually
    pub fn get_code(&self) -> u32 {
        self.code
    }

    /// Get the underlying clause reference. If this is not a clause reference, but a propagator,
    /// this method will panic.
    pub fn as_clause_reference(self) -> ClauseReference {
        pumpkin_assert_moderate!(self.is_clause());
        ClauseReference::create_allocated_clause_reference(self.code)
    }
}

impl ConstraintReference {
    fn two_most_significant_bits(number: u32) -> u32 {
        <u32 as BitRange<u32>>::bit_range(&number, 31, 30)
    }

    fn is_valid_allocated_clause_id(clause_id: u32) -> bool {
        ConstraintReference::two_most_significant_bits(clause_id) == 0
    }
}

impl From<ClauseReference> for ConstraintReference {
    fn from(clause_reference: ClauseReference) -> Self {
        ConstraintReference {
            code: clause_reference.get_code(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::variables::Literal;

    #[test]
    fn test_binary_clause_conversion() {
        for num in [10, 11] {
            let literal = Literal::u32_to_literal(num);
            let clause_reference = ClauseReference::create_virtual_binary_clause_reference(literal);
            let constraint_reference: ConstraintReference = clause_reference.into();
            assert!(constraint_reference.is_clause());
            assert!(!constraint_reference.is_cp_reason());
        }
    }

    #[test]
    fn test_allocated_clause_conversion() {
        let clause_id: u32 = 10;
        let clause_reference = ClauseReference::create_allocated_clause_reference(clause_id);
        let constraint_reference: ConstraintReference = clause_reference.into();
        assert!(constraint_reference.is_clause());
        assert!(!constraint_reference.is_cp_reason());
    }

    #[test]
    fn test_propagator_conversion() {
        let reason_ref = ReasonRef(10);
        let constraint_reference = ConstraintReference::create_reason_reference(reason_ref);
        assert!(!constraint_reference.is_clause());
        assert!(constraint_reference.is_cp_reason());
        assert_eq!(constraint_reference.get_reason_ref(), reason_ref);
    }

    #[test]
    fn test_two_most_significant_bits() {
        assert_eq!(ConstraintReference::two_most_significant_bits(5), 0);
        assert_eq!(ConstraintReference::two_most_significant_bits(0), 0);
        assert_ne!(ConstraintReference::two_most_significant_bits(1 << 31), 0);
        assert_ne!(ConstraintReference::two_most_significant_bits(3 << 30), 0);
        assert_eq!(
            ConstraintReference::two_most_significant_bits(!(3 << 30)),
            0
        );
    }
}
