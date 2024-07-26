use std::fmt::Debug;
use std::fmt::Formatter;

use bitfield::Bit;
use bitfield::BitRange;

use crate::basic_types::ConstraintReference;
#[cfg(doc)]
use crate::engine::clause_allocators::clause_allocator_interface::ClauseAllocatorInterface;
use crate::engine::variables::Literal;
use crate::pumpkin_assert_moderate;

/// Opaque clause reference that is used by clause allocators (`ClauseAllocatorInterface`).
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ClauseReference {
    /// A packed representation of either a virtual binary clause of a reference to an allocated
    ///  clause.
    ///
    /// 1. Binary clause: The 31st bit is one (31st bit -> most significant bit). The remaining 31
    ///    bits encode a literal that is part of the binary clause. The other literal of the binary
    ///    clause is to be recovered from the data structure that stores this constraint reference.
    ///    For example, if ref 'r' is used as the reason for propagating variable x, then the
    ///    binary clause is (x v r).
    /// 2. Allocated clause: Both the 31st and 30th bit are zero, the remaining 30 bits encode the
    ///    clause id.
    ///
    /// N.B. having both 31st and 30th bit set or having the 31st not set with the 30th bit set
    ///  cannot take place, this combination could be used in the future for some other indicator.
    ///  But in that case then the binary clause will only have 30 bits to work with, whereas
    ///  currently it has 31 bits.
    code: u32,
}

impl Debug for ClauseReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ClauseReference")?;
        if self.is_virtual_binary_clause() {
            write!(
                f,
                "::VirtualBinaryClause({})",
                self.get_virtual_binary_clause_literal()
            )
        } else {
            debug_assert!(self.is_allocated_clause());
            write!(f, "::AllocatedClause({})", self.get_code())
        }
    }
}

/// Internal methods used by the clause allocator, including constructor methods.
impl ClauseReference {
    pub(crate) fn create_allocated_clause_reference(id: u32) -> Self {
        pumpkin_assert_moderate!(ClauseReference::is_valid_allocated_clause_id(id));
        ClauseReference { code: id }
    }

    #[cfg(test)]
    /// Creates the reference to indicate that propagation was due to the input literal as part of
    ///  a binary clause.
    pub(crate) fn create_virtual_binary_clause_reference(literal: Literal) -> ClauseReference {
        use bitfield::BitMut;

        pumpkin_assert_moderate!(!literal.to_u32().bit(31));
        let mut code = literal.to_u32();
        code.set_bit(31, true);
        ClauseReference { code }
    }

    pub(crate) fn get_code(&self) -> u32 {
        self.code
    }
}

/// Safe to use methods for identifying the what kind of reference this is.
impl ClauseReference {
    pub fn is_virtual_binary_clause(&self) -> bool {
        self.code.bit(31)
    }

    pub fn is_allocated_clause(&self) -> bool {
        ClauseReference::are_two_most_significant_bits_zero(self.code)
    }

    pub fn get_virtual_binary_clause_literal(&self) -> Literal {
        pumpkin_assert_moderate!(self.is_virtual_binary_clause());
        let literal_code = <u32 as BitRange<u32>>::bit_range(&self.code, 30, 0);
        Literal::u32_to_literal(literal_code)
    }

    fn are_two_most_significant_bits_zero(number: u32) -> bool {
        <u32 as BitRange<u32>>::bit_range(&number, 31, 30) == 0
    }

    fn is_valid_allocated_clause_id(clause_id: u32) -> bool {
        ClauseReference::are_two_most_significant_bits_zero(clause_id)
    }
}

/// A `ConstraintReference` can be a `ClauseReference` with the same internal structure.
impl From<ConstraintReference> for ClauseReference {
    fn from(constraint_reference: ConstraintReference) -> Self {
        pumpkin_assert_moderate!(constraint_reference.is_clause());
        ClauseReference {
            code: constraint_reference.get_code(),
        }
    }
}

#[cfg(test)]
mod tests {
    use bitfield::BitMut;
    use bitfield::BitRange;

    use crate::basic_types::ClauseReference;
    use crate::engine::variables::Literal;

    #[test]
    fn test_binary_clause_creation() {
        for num in [10, 11] {
            let literal = Literal::u32_to_literal(num);
            let clause_reference = ClauseReference::create_virtual_binary_clause_reference(literal);
            assert!(clause_reference.is_virtual_binary_clause());
            assert!(!clause_reference.is_allocated_clause());
            assert_eq!(
                clause_reference.get_virtual_binary_clause_literal(),
                literal
            );
        }
    }

    #[test]
    fn test_allocated_clause_creation() {
        let clause_id: u32 = 10;
        assert!(ClauseReference::is_valid_allocated_clause_id(clause_id));
        let clause_reference = ClauseReference::create_allocated_clause_reference(clause_id);
        assert!(!clause_reference.is_virtual_binary_clause());
        assert!(clause_reference.is_allocated_clause());
        assert_eq!(clause_reference.get_code(), clause_id);
    }

    #[test]
    fn test_bitfield() {
        let mut m: u32 = 0;
        m.set_bit(31, true);
        assert_eq!(m, 1 << 31);
        m.set_bit(0, true);
        m.set_bit(1, true);
        m.set_bit(2, true);
        m.set_bit(3, true);
        assert_eq!(<u32 as BitRange<u32>>::bit_range(&m, 2, 0), 7);
    }
}
