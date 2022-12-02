use crate::basic_types::Clause;
use crate::basic_types::ClauseReference;
use crate::basic_types::Literal;
use crate::pumpkin_asserts::*;

pub struct ClauseAllocator {
    allocated_clauses: Vec<Clause>,
    max_clause_id: u32,
    deleted_clause_ids: Vec<ClauseReference>,
}

impl ClauseAllocator {
    pub fn new() -> ClauseAllocator {
        ClauseAllocator {
            allocated_clauses: vec![],
            max_clause_id: u32::MAX - 1,
            deleted_clause_ids: vec![],
        }
    }

    pub fn create_clause(&mut self, literals: Vec<Literal>, is_learned: bool) -> ClauseReference {
        //todo - add assert to ensure that the clause is as we expect, e.g., no duplicate literals. Normally preprocess_clause would get rid of this. Perhaps could move the responsibility to the clause manager, and have an unchecked version for learned clauses
        pumpkin_assert_ne_simple!(literals.len(), 0);

        if self.deleted_clause_ids.is_empty() {
            //create a new clause reference, unseen before
            let clause_reference = ClauseReference {
                id: self.allocated_clauses.len() as u32 + 1, //we keep clause reference id zero as the null value, never to be allocated at that position
            };

            self.allocated_clauses
                .push(Clause::new(literals, is_learned));

            pumpkin_assert_simple!(
                clause_reference.id <= self.max_clause_id,
                "Clause allocation reached its limit! Most likely cause by an error?"
            );
            clause_reference
        } else {
            //reuse a clause reference from the deleted clause pool
            let clause_reference = self.deleted_clause_ids.pop().unwrap();
            self.allocated_clauses[clause_reference.id as usize] =
                Clause::new(literals, is_learned);

            pumpkin_assert_simple!(
                clause_reference.id <= self.max_clause_id,
                "Clause allocation reached its limit when using a deleted clause id! Most likely cause by an error?"
            );
            clause_reference
        }
    }

    pub fn get_mutable_clause(&mut self, clause_reference: ClauseReference) -> &mut Clause {
        &mut self.allocated_clauses[clause_reference.id as usize - 1]
    }

    pub fn get_clause(&self, clause_reference: ClauseReference) -> &Clause {
        pumpkin_assert_ne_moderate!(clause_reference.id, 0);
        &self.allocated_clauses[clause_reference.id as usize - 1]
    }

    pub fn delete_clause(&mut self, clause_reference: ClauseReference) {
        //note that in the current implementation 'deleting' a clause simply labels its clause reference as available
        //  so next time a new clause is created, it can freely take the value of a previous deleted clause
        //  this may change if we change the clause allocation mechanism as usual in SAT solvers
        pumpkin_assert_moderate!(
            !self.get_clause(clause_reference).is_deleted(),
            "Cannot delete an already deleted clause."
        );

        pumpkin_assert_advanced!(!self.deleted_clause_ids.contains(&clause_reference), "Somehow the id of the deleted clause is already present in the internal data structures, meaning we are deleting the clause twice, unexpected.");
        self.get_mutable_clause(clause_reference).mark_deleted();
        self.deleted_clause_ids.push(clause_reference);
    }

    pub fn is_reason_code_linked_to_a_clause(&self, reason_code: u32) -> bool {
        reason_code <= self.max_clause_id
    }

    pub fn reduce_id_limit_by_one(&mut self) {
        pumpkin_assert_simple!(
            self.max_clause_id > 0
                && self.allocated_clauses.len() < (self.max_clause_id - 1) as usize,
            "Cannot reduce the limit beyond what is already allocated - perhaps an error?"
        );
        pumpkin_assert_simple!(!self.deleted_clause_ids.contains(&ClauseReference {
            id: self.max_clause_id}), "Reducing the max limit excludes a clause reference that is in the deleted pile that could be later used for a new clause. This is easy to fix, however we do not expect this assert to ever trigger and I am keeping this assert for the time being. In the future the allocation procedure will likely change."
        );

        self.max_clause_id -= 1;
    }
}

impl std::ops::Index<ClauseReference> for ClauseAllocator {
    type Output = Clause;
    fn index(&self, clause_reference: ClauseReference) -> &Clause {
        self.get_clause(clause_reference)
    }
}

impl std::ops::IndexMut<ClauseReference> for ClauseAllocator {
    fn index_mut(&mut self, clause_reference: ClauseReference) -> &mut Clause {
        self.get_mutable_clause(clause_reference)
    }
}

impl std::fmt::Display for ClauseAllocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let clauses_string = &self
            .allocated_clauses
            .iter()
            .fold(String::new(), |acc, clause| {
                acc + &clause.to_string() + "\n"
            });

        write!(
            f,
            "Num clauses: {}\n{}",
            self.allocated_clauses.len(),
            clauses_string
        )
    }
}
