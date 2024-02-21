use super::ClauseAllocatorInterface;
use super::ClauseBasic;
use crate::basic_types::ClauseReference;
use crate::basic_types::Literal;
use crate::engine::clause_allocators::ClauseInterface;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Default, Debug)]
pub struct ClauseAllocatorBasic {
    allocated_clauses: Vec<ClauseBasic>,
    deleted_clause_references: Vec<ClauseReference>,
}

impl ClauseAllocatorInterface<ClauseBasic> for ClauseAllocatorBasic {
    type Clause = ClauseBasic;

    fn create_clause(&mut self, literals: Vec<Literal>, is_learned: bool) -> ClauseReference {
        // todo - add assert to ensure that the clause is as we expect, e.g., no duplicate literals.
        // Normally preprocess_clause would get rid of this. Perhaps could move the responsibility
        // to the clause manager, and have an unchecked version for learned clauses
        pumpkin_assert_simple!(literals.len() >= 2);

        if self.deleted_clause_references.is_empty() {
            // create a new clause reference, unseen before
            let clause_reference = ClauseReference::create_allocated_clause_reference(
                self.allocated_clauses.len() as u32 + 1,
            ); // we keep clause reference id zero as the null value, never to be allocated at that
               // position

            self.allocated_clauses
                .push(ClauseBasic::new(literals, is_learned));

            clause_reference
        } else {
            // reuse a clause reference from the deleted clause pool
            let clause_reference = self.deleted_clause_references.pop().unwrap();
            self.allocated_clauses[clause_reference.get_code() as usize - 1] =
                ClauseBasic::new(literals, is_learned);

            clause_reference
        }
    }

    fn get_mutable_clause(&mut self, clause_reference: ClauseReference) -> &mut ClauseBasic {
        &mut self.allocated_clauses[clause_reference.get_code() as usize - 1]
        //-1 since clause ids go from one, and not zero
    }

    fn get_clause(&self, clause_reference: ClauseReference) -> &ClauseBasic {
        &self.allocated_clauses[clause_reference.get_code() as usize - 1]
        //-1 since clause ids go from one, and not zero
    }

    fn delete_clause(&mut self, clause_reference: ClauseReference) {
        pumpkin_assert_moderate!(
            clause_reference.get_code() - 1 < self.allocated_clauses.len() as u32
        );
        // note that in the current implementation 'deleting' a clause simply labels its clause
        // reference as available  so next time a new clause is created, it can freely take
        // the value of a previous deleted clause  this may change if we change the clause
        // allocation mechanism as usual in SAT solvers
        pumpkin_assert_moderate!(
            !self.get_clause(clause_reference).is_deleted(),
            "Cannot delete an already deleted clause."
        );
        pumpkin_assert_advanced!(!self.deleted_clause_references.contains(&clause_reference), "Somehow the id of the deleted clause is already present in the internal data structures, meaning we are deleting the clause twice, unexpected.");

        self.get_mutable_clause(clause_reference).mark_deleted();
        self.deleted_clause_references.push(clause_reference);
    }
}

impl std::ops::Index<ClauseReference> for ClauseAllocatorBasic {
    type Output = ClauseBasic;
    fn index(&self, clause_reference: ClauseReference) -> &ClauseBasic {
        self.get_clause(clause_reference)
    }
}

impl std::ops::IndexMut<ClauseReference> for ClauseAllocatorBasic {
    fn index_mut(&mut self, clause_reference: ClauseReference) -> &mut ClauseBasic {
        self.get_mutable_clause(clause_reference)
    }
}

impl std::fmt::Display for ClauseAllocatorBasic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let clauses_string = &self
            .allocated_clauses
            .iter()
            .fold(String::new(), |acc, clause| format!("{acc}{clause}\n"));

        let num_clauses = self.allocated_clauses.len();
        write!(f, "Num clauses: {num_clauses}\n{clauses_string}")
    }
}
