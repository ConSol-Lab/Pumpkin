use crate::basic_types::Literal;

#[allow(clippy::len_without_is_empty)] // does not make sense to have a is_empty() function since clauses are never empty
pub trait ClauseInterface:
    std::ops::Index<u32, Output = Literal> + std::ops::IndexMut<u32, Output = Literal>
{
    fn len(&self) -> u32;
    fn is_learned(&self) -> bool;
    fn is_protected_against_deletion(&self) -> bool;
    fn is_deleted(&self) -> bool;

    fn get_literal_slice(&self) -> &[Literal];
    fn lbd(&self) -> u32;
    fn get_activity(&self) -> f32;

    // note that this does _not_ delete the clause, it simply marks it as if it was deleted
    //  to delete a clause, use the ClauseManager
    //  could restrict access of this method in the future
    fn mark_deleted(&mut self);
    fn mark_protection_against_deletion(&mut self);
    fn clear_protection_against_deletion(&mut self);
    fn update_lbd(&mut self, new_lbd: u32);
    fn increase_activity(&mut self, increment: f32);
    fn divide_activity(&mut self, division_factor: f32);
}
