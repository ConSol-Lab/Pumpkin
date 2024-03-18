//! Do not use the code in this file, it is not sound, i.e. it violates Rust's safety rules
// Memory layout for a clause:
// LBD -> 29 bits
// is_learned -> 1 bit (1 << 29)
// is_deleted -> 1 bit (1 << 30)
// is_protected -> 1 bit (1 << 31)
// ----32 bits----
// num_literals -> 4 bytes
// [array of literals]
// activity -> 4 bytes, only if learned

use bitfield::Bit;
use bitfield::BitMut;
use bitfield::BitRange;
use bitfield::BitRangeMut;

use super::ClauseInterface;
use crate::engine::variables::Literal;
use crate::pumpkin_assert_moderate;

#[repr(C)] // important to keep the c layout since the code below relies on this layout
#[derive(Debug, Copy, Clone)]
pub struct ClauseInlined {
    lbd_and_flags: u32,
    num_literals: u32,
    literals: [Literal; 0],
    // activity is implicit and is not given as part of the fields, the activity is stored after
    // the literals
}

impl ClauseInlined {
    fn bit_protected_against_deletion() -> usize {
        31
    }

    fn bit_deleted() -> usize {
        30
    }

    fn bit_learned() -> usize {
        29
    }

    fn bit_lbd_most_significant() -> usize {
        28
    }
}

impl ClauseInterface for ClauseInlined {
    fn len(&self) -> u32 {
        self.num_literals
    }

    fn is_learned(&self) -> bool {
        self.lbd_and_flags.bit(ClauseInlined::bit_learned())
    }

    fn is_protected_against_deletion(&self) -> bool {
        self.lbd_and_flags
            .bit(ClauseInlined::bit_protected_against_deletion())
    }

    fn is_deleted(&self) -> bool {
        self.lbd_and_flags.bit(ClauseInlined::bit_deleted())
    }

    fn get_literal_slice(&self) -> &[Literal] {
        // SAFETY: violated
        unsafe {
            std::slice::from_raw_parts(self.literals.get_unchecked(0), self.num_literals as usize)
        }
    }

    fn lbd(&self) -> u32 {
        self.lbd_and_flags
            .bit_range(ClauseInlined::bit_lbd_most_significant(), 0)
    }

    fn get_activity(&self) -> f32 {
        pumpkin_assert_moderate!(self.is_learned());
        // SAFETY: violated
        unsafe {
            // for learned clauses, the activity is stored right after the literals
            let ptr_literal: *const Literal =
                self.literals.get_unchecked(self.num_literals as usize);
            let ptr_f32 = ptr_literal.cast::<f32>();
            *ptr_f32
        }
    }

    fn mark_deleted(&mut self) {
        pumpkin_assert_moderate!(!self.is_deleted());
        self.lbd_and_flags
            .set_bit(ClauseInlined::bit_deleted(), true);
    }

    fn mark_protection_against_deletion(&mut self) {
        self.lbd_and_flags
            .set_bit(ClauseInlined::bit_protected_against_deletion(), true);
    }

    fn clear_protection_against_deletion(&mut self) {
        self.lbd_and_flags
            .set_bit(ClauseInlined::bit_protected_against_deletion(), false);
    }

    fn update_lbd(&mut self, lbd: u32) {
        pumpkin_assert_moderate!(
            <u32 as BitRange<u32>>::bit_range(
                &lbd,
                31,
                ClauseInlined::bit_lbd_most_significant() + 1
            ) == 0
        );
        self.lbd_and_flags
            .set_bit_range(ClauseInlined::bit_lbd_most_significant(), 0, lbd);
    }

    fn increase_activity(&mut self, increment: f32) {
        *self.get_activity_internal() += increment;
    }

    fn divide_activity(&mut self, division_factor: f32) {
        *self.get_activity_internal() /= division_factor;
    }
}

impl ClauseInlined {
    fn mark_learned(&mut self) {
        self.lbd_and_flags
            .set_bit(ClauseInlined::bit_learned(), true);
    }

    fn clear_flags(&mut self) {
        self.lbd_and_flags
            .set_bit_range(31, ClauseInlined::bit_lbd_most_significant() + 1, 0);
    }

    fn set_activity(&mut self, new_value: f32) {
        pumpkin_assert_moderate!(self.is_learned());
        // SAFETY: violated
        unsafe {
            // for learned clauses, the activity is stored right after the literals
            let ptr_literal: *mut Literal =
                self.literals.get_unchecked_mut(self.num_literals as usize);
            let ptr_f32 = ptr_literal.cast::<f32>();
            *ptr_f32 = new_value;
        }
    }

    fn get_activity_internal(&mut self) -> &mut f32 {
        pumpkin_assert_moderate!(self.is_learned());
        // SAFETY: violated
        unsafe {
            // for learned clauses, the activity is stored right after the literals
            let ptr_literal: *mut Literal =
                self.literals.get_unchecked_mut(self.num_literals as usize);
            let ptr_f32 = ptr_literal.cast::<f32>();
            &mut *ptr_f32
        }
    }

    // this method assumes that enough memory is available at the location
    //  see 'num_bytes_required_for_clause'
    #[allow(clippy::mut_from_ref)]
    pub fn create_clause_at_memory_location(
        loc: *mut u32,
        literals: &[Literal],
        is_learned: bool,
    ) -> &mut ClauseInlined {
        // SAFETY: violated
        unsafe {
            let clause = loc.cast::<ClauseInlined>();
            (*clause).update_lbd(literals.len() as u32);
            (*clause).clear_flags();
            if is_learned {
                (*clause).mark_learned();
            }
            (*clause).num_literals = literals.len() as u32;
            for e in literals.iter().enumerate() {
                (*clause)[e.0 as u32] = *e.1;
            }
            if is_learned {
                (*clause).set_activity(0.0);
            }
            &mut (*clause)
        }
    }

    /// Since we manually manage the memory for the clause
    /// it is important to know exactly the size of the clause
    /// note that size_of will not report the correct value, so we have this method
    pub fn num_u32s_required_for_clause(num_literals: u32, is_learned: bool) -> u32 {
        // 1 for lbd and flags
        // 1 for storing the number of literals
        // `num_literals` for the literals
        // `is_learned` for the activity of the learned clauses
        1 + 1 + num_literals + is_learned as u32
    }
}

impl std::ops::Index<u32> for ClauseInlined {
    type Output = Literal;
    fn index(&self, index: u32) -> &Literal {
        pumpkin_assert_moderate!(index < self.num_literals);
        // SAFETY: violated
        unsafe { self.literals.get_unchecked(index as usize) }
    }
}

impl std::ops::IndexMut<u32> for ClauseInlined {
    fn index_mut(&mut self, index: u32) -> &mut Literal {
        pumpkin_assert_moderate!(index < self.num_literals);
        // SAFETY: violated
        unsafe { self.literals.get_unchecked_mut(index as usize) }
    }
}

#[cfg(test)]
mod tests {

    use crate::engine::clause_allocators::ClauseInlined;
    use crate::engine::clause_allocators::ClauseInterface;
    use crate::engine::variables::Literal;

    #[ignore]
    #[test]
    fn test_clause_creation() {
        let lits = [
            Literal::u32_to_literal(5),
            Literal::u32_to_literal(10),
            Literal::u32_to_literal(15),
        ];

        let memory: &mut [u32; 1000] = &mut [0; 1000];

        let clause = ClauseInlined::create_clause_at_memory_location(&mut memory[0], &lits, false);

        #[allow(trivial_casts)]
        let p1 = (clause as *const ClauseInlined).cast::<u32>();
        let p2: *const u32 = &memory[0];

        assert_eq!(p1, p2);

        assert_eq!(lits[0], clause[0]);
        assert_eq!(lits[1], clause[1]);
        assert_eq!(lits[2], clause[2]);

        memory[2] = 0;
        memory[3] = 0;

        assert_eq!(clause[0], Literal::u32_to_literal(0));
        assert_eq!(clause[1], Literal::u32_to_literal(0));

        let new_lit_code = 100;
        let new_lit = Literal::u32_to_literal(new_lit_code);
        clause[0] = new_lit;

        assert_eq!(clause[0].to_u32(), new_lit_code);
        assert_eq!(clause[0], new_lit);
        assert_eq!(memory[2], new_lit_code);
    }

    #[ignore]
    #[test]
    fn test_size_and_align() {
        // these should be static asserts
        assert_eq!(std::mem::size_of::<ClauseInlined>(), 8);
        assert_eq!(std::mem::align_of::<ClauseInlined>(), 4);
        assert_eq!(std::mem::size_of::<Literal>(), 4);
    }

    #[ignore]
    #[test]
    fn test_fresh_clause() {
        let lits = [
            Literal::u32_to_literal(5),
            Literal::u32_to_literal(10),
            Literal::u32_to_literal(15),
        ];

        let memory: &mut [u32; 1000] = &mut [0; 1000];

        let clause = ClauseInlined::create_clause_at_memory_location(&mut memory[0], &lits, true);

        assert!(clause.is_learned());
        assert!(!clause.is_deleted());
        assert!(!clause.is_protected_against_deletion());
        assert_eq!(clause.lbd(), lits.len() as u32);
        assert_eq!(clause.get_activity(), 0.0);
        assert_eq!(clause.len(), 3);
    }

    #[ignore]
    #[test]
    fn test_lbd() {
        let lits = [
            Literal::u32_to_literal(5),
            Literal::u32_to_literal(10),
            Literal::u32_to_literal(15),
        ];

        let memory: &mut [u32; 1000] = &mut [0; 1000];

        let clause = ClauseInlined::create_clause_at_memory_location(&mut memory[0], &lits, true);

        assert_eq!(clause.lbd(), lits.len() as u32);
        clause.update_lbd(2);
        assert_eq!(clause.lbd(), 2);
        clause.update_lbd(10);
        assert_eq!(clause.lbd(), 10);
    }

    #[ignore]
    #[test]
    fn test_flags() {
        let lits = [
            Literal::u32_to_literal(5),
            Literal::u32_to_literal(10),
            Literal::u32_to_literal(15),
        ];

        let memory: &mut [u32; 1000] = &mut [0; 1000];

        let clause = ClauseInlined::create_clause_at_memory_location(&mut memory[0], &lits, true);

        assert!(clause.is_learned());

        assert!(!clause.is_protected_against_deletion());
        clause.mark_protection_against_deletion();
        assert!(clause.is_protected_against_deletion());
        clause.clear_protection_against_deletion();
        assert!(!clause.is_protected_against_deletion());

        assert!(!clause.is_deleted());
        clause.mark_deleted();
        assert!(clause.is_deleted());
    }

    #[ignore]
    #[test]
    fn test_activity() {
        let lits = [
            Literal::u32_to_literal(5),
            Literal::u32_to_literal(10),
            Literal::u32_to_literal(15),
        ];

        let memory: &mut [u32; 1000] = &mut [0; 1000];

        let clause = ClauseInlined::create_clause_at_memory_location(&mut memory[0], &lits, true);

        assert_eq!(clause.get_activity(), 0.0);
        clause.set_activity(10.0);
        assert_eq!(clause.get_activity(), 10.0);
        clause.set_activity(15.0);
        assert_eq!(clause.get_activity(), 15.0);
    }

    #[ignore]
    #[test]
    fn test_slice() {
        let lits = [
            Literal::u32_to_literal(5),
            Literal::u32_to_literal(10),
            Literal::u32_to_literal(15),
        ];

        let memory: &mut [u32; 1000] = &mut [0; 1000];

        let clause = ClauseInlined::create_clause_at_memory_location(&mut memory[0], &lits, true);

        let mut num = 0;
        for e in clause.get_literal_slice().iter().enumerate() {
            assert_eq!(lits[e.0], *e.1);
            num += 1;
        }
        assert_eq!(num, 3);
    }
}
