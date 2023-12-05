use bitfield::{Bit, BitMut, BitRange};

use crate::pumpkin_assert_moderate;

use super::{ConstraintReference, Literal};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ClauseReference {
    //the clause reference may refer to a virtual binary clause or an allocated clause
    //the idea is to pack all this information into 32 bits
    //this is done in the following way
    //1. binary clause: the 31st bit is one (31st bit -> most significant bit)
    //      the remaining 31 bits encode a literal that is part of the binary clause
    //      the other literal of the binary clause is to be recovered from the data structure that stores this constraint reference
    //      e.g., if ref 'r' is used as the reason for propagating variable x, then the binary clause is (x v r)
    //2. Allocated clause: both the 31st and 30th bit are zero
    //      the remaining 30 bits encode the clause id
    //note: having both 31st and 30th bit set or having the 31st not set with the 30th bit set cannot take place, this combination could be used in the future for some other indicator
    //          but in that case then the binary clause will only have 30 bits to work with, whereas currently it has 31 bits
    code: u32,
}

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

    pub fn create_allocated_clause_reference(id: u32) -> Self {
        pumpkin_assert_moderate!(ClauseReference::is_valid_allocated_clause_id(id));
        ClauseReference { code: id }
    }

    //for internal purposes, not to be called usually
    pub fn get_code(&self) -> u32 {
        self.code
    }

    //creates the reference to indicate that propagation was due to the input literal as part of a binary clause
    pub fn create_virtual_binary_clause_reference(literal: Literal) -> ClauseReference {
        pumpkin_assert_moderate!(!literal.to_u32().bit(31));
        let mut code = literal.to_u32();
        code.set_bit(31, true);
        ClauseReference { code }
    }

    fn are_two_most_significant_bits_zero(number: u32) -> bool {
        <u32 as BitRange<u32>>::bit_range(&number, 31, 30) == 0
    }

    fn is_valid_allocated_clause_id(clause_id: u32) -> bool {
        ClauseReference::are_two_most_significant_bits_zero(clause_id)
    }
}
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
    use bitfield::{BitMut, BitRange};

    use crate::basic_types::{ClauseReference, Literal};

    #[test]
    fn test_binary_clause_creation() {
        for num in [10, 11] {
            let literal = Literal::u32_to_literal(num);
            let clause_reference = ClauseReference::create_virtual_binary_clause_reference(literal);
            assert!(clause_reference.is_virtual_binary_clause());
            assert!(!clause_reference.is_allocated_clause());
            assert!(clause_reference.get_virtual_binary_clause_literal() == literal);
        }
    }

    #[test]
    fn test_allocated_clause_creation() {
        let clause_id: u32 = 10;
        assert!(ClauseReference::is_valid_allocated_clause_id(clause_id));
        let clause_reference = ClauseReference::create_allocated_clause_reference(clause_id);
        assert!(!clause_reference.is_virtual_binary_clause());
        assert!(clause_reference.is_allocated_clause());
        assert!(clause_reference.get_code() == clause_id);
    }

    #[test]
    fn test_bitfield() {
        let mut m: u32 = 0;
        m.set_bit(31, true);
        assert!(m == (1 << 31));
        m.set_bit(0, true);
        m.set_bit(1, true);
        m.set_bit(2, true);
        m.set_bit(3, true);
        assert!(<u32 as BitRange<u32>>::bit_range(&m, 2, 0) == 7);
    }
}
