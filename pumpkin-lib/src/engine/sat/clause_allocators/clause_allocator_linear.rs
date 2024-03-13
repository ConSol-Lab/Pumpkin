//! Do not use the code in this file, it is not sound, i.e. it violates Rust's safety rules
use super::ClauseAllocatorInterface;
use super::ClauseInlined;
use super::ClauseInterface;
use crate::basic_types::ClauseReference;
use crate::basic_types::Literal;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_ne_simple;

// todo
//  + dynamic size allocation - currently the maximum amount is allocated (2^30 u32s)
//  + garbage collection

#[derive(Debug)]
pub struct ClauseAllocatorLinear {
    data: Vec<u32>, // allocating a fixed block for now
    next_location: u32,
    deleted_clause_space_usage: u32,
}

impl Default for ClauseAllocatorLinear {
    fn default() -> Self {
        ClauseAllocatorLinear {
            data: vec![0; 1 << 30],
            next_location: 1, // keeping zero for the null pointer
            deleted_clause_space_usage: 0,
        }
    }
}

impl ClauseAllocatorInterface<ClauseInlined> for ClauseAllocatorLinear {
    type Clause = ClauseInlined;

    fn create_clause(&mut self, literals: Vec<Literal>, is_learned: bool) -> ClauseReference {
        // todo - add assert to ensure that the clause is as we expect, e.g., no duplicate literals.
        // Normally preprocess_clause would get rid of this. Perhaps could move the responsibility
        // to the clause manager, and have an unchecked version for learned clauses
        pumpkin_assert_ne_simple!(literals.len(), 0);

        let clause_reference =
            ClauseReference::create_allocated_clause_reference(self.next_location);

        let _ = ClauseInlined::create_clause_at_memory_location(
            &mut self.data[self.next_location as usize],
            literals.as_slice(),
            is_learned,
        );

        self.next_location +=
            ClauseInlined::num_u32s_required_for_clause(literals.len() as u32, is_learned);

        clause_reference
    }

    fn get_mutable_clause(&mut self, clause_reference: ClauseReference) -> &mut ClauseInlined {
        // SAFETY: should be ok as long as the ClauseReference was created by self, and wasn't
        // deleted...
        unsafe { &mut *self.get_pointer_mut(clause_reference) }
    }

    fn get_clause(&self, clause_reference: ClauseReference) -> &ClauseInlined {
        // SAFETY: should be ok as long as the ClauseReference was created by self, and wasn't
        // deleted...
        unsafe { &*self.get_pointer(clause_reference) }
    }

    fn delete_clause(&mut self, clause_reference: ClauseReference) {
        // for now we do not really delete clauses, we only take note of clause deletion
        let ptr_clause = self.get_pointer_mut(clause_reference);
        // SAFETY: should be ok as long as the ClauseReference was created by self, and wasn't
        // deleted...
        let num_literals = unsafe { (*ptr_clause).len() };
        // SAFETY: should be ok as long as the ClauseReference was created by self, and wasn't
        // deleted...
        let is_learned = unsafe { (*ptr_clause).is_learned() };
        // SAFETY: should be ok as long as the ClauseReference was created by self, and wasn't
        // deleted...
        unsafe {
            (*ptr_clause).mark_deleted();
        }

        self.deleted_clause_space_usage +=
            ClauseInlined::num_u32s_required_for_clause(num_literals, is_learned);
    }
}

impl ClauseAllocatorLinear {
    fn get_pointer(&self, clause_reference: ClauseReference) -> *const ClauseInlined {
        pumpkin_assert_moderate!(clause_reference.is_allocated_clause());
        let ptr_u32: *const u32 = &self.data[clause_reference.get_code() as usize];
        ptr_u32.cast::<ClauseInlined>()
    }

    fn get_pointer_mut(&mut self, clause_reference: ClauseReference) -> *mut ClauseInlined {
        pumpkin_assert_moderate!(clause_reference.is_allocated_clause());
        let ptr_u32: *mut u32 = &mut self.data[clause_reference.get_code() as usize];
        ptr_u32.cast::<ClauseInlined>()
    }
}

impl std::ops::Index<ClauseReference> for ClauseAllocatorLinear {
    type Output = ClauseInlined;
    fn index(&self, clause_reference: ClauseReference) -> &ClauseInlined {
        self.get_clause(clause_reference)
    }
}

impl std::ops::IndexMut<ClauseReference> for ClauseAllocatorLinear {
    fn index_mut(&mut self, clause_reference: ClauseReference) -> &mut ClauseInlined {
        self.get_mutable_clause(clause_reference)
    }
}
