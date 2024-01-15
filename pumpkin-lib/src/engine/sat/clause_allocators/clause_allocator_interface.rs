use super::ClauseInterface;
use crate::basic_types::ClauseReference;
use crate::basic_types::Literal;

//the trait requires the [] operator
//  and its corresponding Clause must implement the ClauseInterface
pub trait ClauseAllocatorInterface<Clause: ClauseInterface>:
    std::ops::Index<ClauseReference, Output = Clause>
    + std::ops::IndexMut<ClauseReference, Output = Clause>
{
    type Clause;

    fn create_clause(&mut self, literals: Vec<Literal>, is_learned: bool) -> ClauseReference;
    fn get_mutable_clause(&mut self, clause_reference: ClauseReference) -> &mut Clause;
    fn get_clause(&self, clause_reference: ClauseReference) -> &Clause;
    fn delete_clause(&mut self, clause_reference: ClauseReference);
}
