use super::Literal;
use crate::pumpkin_asserts::*;

pub struct Clause {
    literals: Vec<Literal>,
    is_learned: bool,
    is_deleted: bool,
    is_protected_aganst_deletion: bool,
    lbd: u32,
    activity: f32,
}

impl Clause {
    pub fn new(literals: Vec<Literal>, is_learned: bool) -> Clause {
        pumpkin_assert_simple!(literals.len() >= 2);

        let num_literals = literals.len() as u32;
        Clause {
            literals,
            is_learned,
            is_deleted: false,
            is_protected_aganst_deletion: false,
            lbd: num_literals, //pessimistic lbd
            activity: 0.0,
        }
    }

    pub fn len(&self) -> u32 {
        self.literals.len() as u32
    }

    pub fn is_learned(&self) -> bool {
        self.is_learned
    }

    pub fn is_protected_aganst_deletion(&self) -> bool {
        self.is_protected_aganst_deletion
    }

    pub fn is_deleted(&self) -> bool {
        self.is_deleted
    }

    pub fn get_literal_slice(&self) -> &[Literal] {
        &self.literals
    }

    pub fn get_lbd(&self) -> u32 {
        self.lbd
    }

    pub fn get_activity(&self) -> f32 {
        pumpkin_assert_advanced!(self.activity == 0.0 || self.activity.is_normal());
        self.activity
    }

    //note that this does _not_ delete the clause, it simply marks it as if it was deleted
    //  to delete a clause, use the ClauseManager
    //  could restrict access of this method in the future
    pub fn mark_deleted(&mut self) {
        pumpkin_assert_moderate!(!self.is_deleted);
        self.is_deleted = true;
    }

    pub fn mark_protection_against_deletion(&mut self) {
        self.is_protected_aganst_deletion = true;
    }

    pub fn clear_protection_against_deletion(&mut self) {
        pumpkin_assert_moderate!(self.is_protected_aganst_deletion);
        self.is_protected_aganst_deletion = false;
    }

    pub fn update_lbd(&mut self, new_lbd: u32) {
        pumpkin_assert_moderate!(new_lbd < self.lbd);
        self.lbd = new_lbd;
    }

    pub fn increase_activity(&mut self, increment: f32) {
        self.activity += increment;
    }

    pub fn divide_activity(&mut self, division_factor: f32) {
        self.activity /= division_factor;
    }
}

impl std::ops::Index<u32> for Clause {
    type Output = Literal;
    fn index(&self, index: u32) -> &Literal {
        self.literals.index(index as usize)
    }
}

impl std::ops::IndexMut<u32> for Clause {
    fn index_mut(&mut self, index: u32) -> &mut Literal {
        self.literals.index_mut(index as usize)
    }
}

impl std::fmt::Display for Clause {
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
