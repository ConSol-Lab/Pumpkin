use crate::{
    engine::{AssignmentsInteger, AssignmentsPropositional},
    pumpkin_asserts::{pumpkin_assert_moderate, pumpkin_assert_simple},
};

use super::{IntegerVariable, Literal, PropositionalVariable};

pub struct Solution {
    truth_values: Vec<bool>,
    integer_values: Vec<i32>,
}

impl Solution {
    pub fn new(
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) -> Solution {
        let mut truth_values =
            vec![true; assignments_propositional.num_propositional_variables() as usize];

        let mut integer_values = vec![0; assignments_integer.num_integer_variables() as usize];

        Solution::update_propositional_values(&mut truth_values, assignments_propositional);

        Solution::update_integer_values(&mut integer_values, assignments_integer);

        Solution {
            truth_values,
            integer_values,
        }
    }

    pub fn num_propositional_variables(&self) -> usize {
        self.truth_values.len()
    }

    pub fn num_integer_variables(&self) -> usize {
        self.integer_values.len()
    }

    pub fn update(
        &mut self,
        assignments_propositional: &AssignmentsPropositional,
        assignments_integer: &AssignmentsInteger,
    ) {
        pumpkin_assert_moderate!(
            self.truth_values.len()
                <= assignments_propositional.num_propositional_variables() as usize
        );

        pumpkin_assert_moderate!(
            self.integer_values.len() <= assignments_integer.num_integer_variables() as usize
        );

        //it could be that more variables have been added to the problem since last this time struct has seen a solution
        //  e.g., encoding the upper bound
        //  in that case it is important to resize internal data structures
        if self.truth_values.len()
            < assignments_propositional.num_propositional_variables() as usize
        {
            self.truth_values.resize(
                assignments_propositional.num_propositional_variables() as usize,
                true,
            );
        }

        if self.integer_values.len() < assignments_integer.num_integer_variables() as usize {
            self.integer_values
                .resize(assignments_integer.num_integer_variables() as usize, 0);
        }

        Solution::update_propositional_values(&mut self.truth_values, assignments_propositional);
        Solution::update_integer_values(&mut self.integer_values, assignments_integer);
    }

    pub fn get_literal_value(&self, literal: Literal) -> bool {
        if literal.is_positive() {
            self.truth_values[literal.get_propositional_variable()]
        } else {
            !self.truth_values[literal.get_propositional_variable()]
        }
    }

    fn update_propositional_values(
        truth_values: &mut [bool],
        assignments_propositional: &AssignmentsPropositional,
    ) {
        for propositional_variable in assignments_propositional.get_propositional_variables() {
            pumpkin_assert_simple!(
                assignments_propositional.is_variable_assigned(propositional_variable),
                "The solution struct expects that all propositional variables are assigned."
            );
            truth_values[propositional_variable.index() as usize] =
                assignments_propositional.is_variable_assigned_true(propositional_variable);
        }
    }

    fn update_integer_values(integer_values: &mut [i32], assignments_integer: &AssignmentsInteger) {
        for integer_variable in assignments_integer.get_integer_variables_variables() {
            pumpkin_assert_simple!(
                assignments_integer.is_integer_variable_assigned(integer_variable),
                "The solution struct expects that all integer variables are assigned."
            );
            integer_values[integer_variable.id as usize] =
                assignments_integer.get_assigned_value(integer_variable);
        }
    }
}

impl std::ops::Index<IntegerVariable> for Solution {
    type Output = i32;
    fn index(&self, integer_variable: IntegerVariable) -> &i32 {
        &self.integer_values[integer_variable]
    }
}

impl std::ops::Index<PropositionalVariable> for Solution {
    type Output = bool;
    fn index(&self, propositional_variable: PropositionalVariable) -> &bool {
        &self.truth_values[propositional_variable]
    }
}
