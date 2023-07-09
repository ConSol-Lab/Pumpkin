use crate::{
    basic_types::Literal, pumpkin_assert_advanced, pumpkin_assert_moderate, pumpkin_assert_simple,
};

use super::ClauseInterface;

#[allow(clippy::len_without_is_empty)] // The clause will always have at least two literals.
pub struct ClauseBasic {
    literals: Vec<Literal>,
    is_learned: bool,
    is_deleted: bool,
    is_protected_aganst_deletion: bool,
    lbd: u32,
    activity: f32,
}

impl ClauseBasic {
    pub fn new(literals: Vec<Literal>, is_learned: bool) -> ClauseBasic {
        pumpkin_assert_simple!(literals.len() >= 2);

        let num_literals = literals.len() as u32;
        ClauseBasic {
            literals,
            is_learned,
            is_deleted: false,
            is_protected_aganst_deletion: false,
            lbd: num_literals, //pessimistic lbd
            activity: 0.0,
        }
    }
}

impl ClauseInterface for ClauseBasic {
    fn len(&self) -> u32 {
        self.literals.len() as u32
    }

    fn is_learned(&self) -> bool {
        self.is_learned
    }

    fn is_protected_against_deletion(&self) -> bool {
        self.is_protected_aganst_deletion
    }

    fn is_deleted(&self) -> bool {
        self.is_deleted
    }

    fn get_literal_slice(&self) -> &[Literal] {
        &self.literals
    }

    fn get_lbd(&self) -> u32 {
        self.lbd
    }

    fn get_activity(&self) -> f32 {
        pumpkin_assert_advanced!(!self.activity.is_nan() && !self.activity.is_infinite());
        self.activity
    }

    //note that this does _not_ delete the clause, it simply marks it as if it was deleted
    //  to delete a clause, use the ClauseManager
    //  could restrict access of this method in the future
    fn mark_deleted(&mut self) {
        pumpkin_assert_moderate!(!self.is_deleted);
        self.is_deleted = true;
    }

    fn mark_protection_against_deletion(&mut self) {
        self.is_protected_aganst_deletion = true;
    }

    fn clear_protection_against_deletion(&mut self) {
        pumpkin_assert_moderate!(self.is_protected_aganst_deletion);
        self.is_protected_aganst_deletion = false;
    }

    fn update_lbd(&mut self, new_lbd: u32) {
        pumpkin_assert_moderate!(new_lbd < self.lbd);
        self.lbd = new_lbd;
    }

    fn increase_activity(&mut self, increment: f32) {
        self.activity += increment;
    }

    fn divide_activity(&mut self, division_factor: f32) {
        self.activity /= division_factor;
    }
}

impl std::ops::Index<u32> for ClauseBasic {
    type Output = Literal;
    fn index(&self, index: u32) -> &Literal {
        self.literals.index(index as usize)
    }
}

impl std::ops::IndexMut<u32> for ClauseBasic {
    fn index_mut(&mut self, index: u32) -> &mut Literal {
        self.literals.index_mut(index as usize)
    }
}

impl std::fmt::Display for ClauseBasic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let clause_string = &self
            .literals
            .iter()
            .fold(String::new(), |acc, lit| acc + &lit.to_string() + ",");

        write!(
            f,
            "({})[learned:{}, deleted:{}]",
            clause_string, self.is_learned, self.is_deleted
        )
    }
}
