//reason code -> constraint reference?
//change reason code names in assignments for instance

use bitfield::{Bit, BitMut, BitRange};

use crate::{engine::PropagatorId, pumpkin_assert_moderate};

use super::ClauseReference;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstraintReference {
    //the constraint reference may refer to a virtual binary clause, an allocated clause, or a propagator
    //  note that the user can only distinguish between a clause and a propagator (bu not whether a virtual or allocated clause)
    //the idea is to pack all this information into 32 bits
    //this is done in the following way
    //1. binary clause: the 31st bit is one (31st bit -> most significant bit)
    //      the remaining 31 bits encode a literal that is part of the binary clause
    //      the other literal of the binary clause is to be recovered from the data structure that stores this constraint reference
    //      e.g., if ref 'r' is used as the reason for propagating variable x, then the binary clause is (x v r)
    //2. propagator: the 31st bit is zero, and the 30th bit is one
    //      the remaining 30 bit encode the propagator id
    //3. Allocated clause: both the 31st and 30th bit are zero
    //      the remaining 30 bits encode the clause id
    //      todo: this can be improved, there is no need to allocate an entire bit for CP propagators
    //          instead a maximum propagator value could be used, and the solver can simply determine whether the code is smaller than that value
    //          this was done in the code before introducing binary clauses - we switched to this system for simplicity of implementation
    //          the drawback of the simplification is that we can store less clauses since we can only go up to 2^30
    //          there is potential of almost doubling this number with proper care
    //note: having both 31st and 30th bit set to one cannot take place, this combination could be used in the future for some other indicator
    //          but in that case then the binary clause will only have 30 bits to work with, whereas currently it has 31 bits
    code: u32,
}

//methods to create a constraint reference
impl ConstraintReference {
    pub fn create_null_reference() -> ConstraintReference {
        ConstraintReference { code: 0 }
    }

    pub fn create_standard_clause_reference(clause_id: u32) -> ConstraintReference {
        pumpkin_assert_moderate!(ConstraintReference::is_valid_allocated_clause_id(clause_id));
        ConstraintReference { code: clause_id }
    }

    pub fn create_propagator_reference(propagator_id: PropagatorId) -> ConstraintReference {
        let propagator_id = propagator_id.0;

        pumpkin_assert_moderate!(ConstraintReference::are_two_most_significant_bits_zero(
            propagator_id
        ));
        let mut code = propagator_id;
        code.set_bit(30, true); //the 31st bit is zero, and the 30th bit is one
        ConstraintReference { code }
    }
}

//methods to retrieve information stored in the constraint reference
impl ConstraintReference {
    pub fn is_null(&self) -> bool {
        self.code == 0
    }

    pub fn is_clause(&self) -> bool {
        self.is_virtual_binary_clause() | self.is_allocated_clause()
    }

    fn is_virtual_binary_clause(&self) -> bool {
        self.code.bit(31)
    }

    fn is_allocated_clause(&self) -> bool {
        ConstraintReference::are_two_most_significant_bits_zero(self.code)
    }

    pub fn is_propagator(&self) -> bool {
        <u32 as BitRange<u32>>::bit_range(&self.code, 31, 30) == 1
    }

    pub fn get_propagator_id(&self) -> PropagatorId {
        pumpkin_assert_moderate!(self.is_propagator());
        let mut id = self.code;
        id.set_bit(30, false); //clear the 30th bit, the 31st bit is assumed to already be cleared
        PropagatorId(id)
    }

    //for internal purposes, not to be called usually
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
    fn are_two_most_significant_bits_zero(number: u32) -> bool {
        <u32 as BitRange<u32>>::bit_range(&number, 31, 30) == 0
    }

    fn is_valid_allocated_clause_id(clause_id: u32) -> bool {
        ConstraintReference::are_two_most_significant_bits_zero(clause_id)
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
    use crate::{
        basic_types::{ClauseReference, Literal},
        engine::PropagatorId,
    };

    use super::ConstraintReference;

    #[test]
    fn test_binary_clause_conversion() {
        for num in [10, 11] {
            let literal = Literal::u32_to_literal(num);
            let clause_reference = ClauseReference::create_virtual_binary_clause_reference(literal);
            let constraint_reference: ConstraintReference = clause_reference.into();
            assert!(constraint_reference.is_clause());
            assert!(!constraint_reference.is_propagator());
        }
    }

    #[test]
    fn test_allocated_clause_conversion() {
        let clause_id: u32 = 10;
        let clause_reference = ClauseReference::create_allocated_clause_reference(clause_id);
        let constraint_reference: ConstraintReference = clause_reference.into();
        assert!(constraint_reference.is_clause());
        assert!(!constraint_reference.is_propagator());
    }

    #[test]
    fn test_propagator_conversion() {
        let propagator_id = PropagatorId(10);
        let constraint_reference = ConstraintReference::create_propagator_reference(propagator_id);
        assert!(!constraint_reference.is_clause());
        assert!(constraint_reference.is_propagator());
        assert!(constraint_reference.get_propagator_id() == propagator_id);
    }

    #[test]
    fn test_two_most_significant_bits() {
        assert!(ConstraintReference::are_two_most_significant_bits_zero(5));
        assert!(ConstraintReference::are_two_most_significant_bits_zero(0));
        assert!(!ConstraintReference::are_two_most_significant_bits_zero(
            1 << 31
        ));
        assert!(!ConstraintReference::are_two_most_significant_bits_zero(
            3 << 30
        ));
        assert!(ConstraintReference::are_two_most_significant_bits_zero(
            !(3 << 30)
        ));
    }
}
