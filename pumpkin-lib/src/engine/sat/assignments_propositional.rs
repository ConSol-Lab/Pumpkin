use crate::basic_types::{
    ConstraintReference, Literal, PropositionalVariable, PropositionalVariableGeneratorIterator,
};
use crate::{pumpkin_assert_moderate, pumpkin_assert_simple};

pub struct AssignmentsPropositional {
    assignment_info: Vec<PropositionalAssignmentInfo>,
    current_decision_level: u32,
    pub trail: Vec<Literal>,
    pub trail_delimiter: Vec<u32>, //[i] is the position where the i-th decision level ends (exclusive) on the trail. Note that the current decision level does not have an entry in the trail delimiter!
    pub true_literal: Literal,
    pub false_literal: Literal,
}

impl Default for AssignmentsPropositional {
    fn default() -> Self {
        let dummy_literal = Literal::new(PropositionalVariable::new(0), true);
        AssignmentsPropositional {
            assignment_info: vec![],
            current_decision_level: 0,
            trail: vec![],
            trail_delimiter: vec![],
            true_literal: dummy_literal,
            false_literal: dummy_literal,
        }
    }
}

impl AssignmentsPropositional {
    pub fn increase_decision_level(&mut self) {
        self.current_decision_level += 1;
        self.trail_delimiter.push(self.trail.len() as u32);
    }

    pub fn get_decision_level(&self) -> u32 {
        self.current_decision_level
    }

    pub fn grow(&mut self) {
        self.assignment_info
            .push(PropositionalAssignmentInfo::Unassigned);
    }

    pub fn num_propositional_variables(&self) -> u32 {
        self.assignment_info.len() as u32
    }

    pub fn get_propositional_variables(&self) -> PropositionalVariableGeneratorIterator {
        //we start from 1 to ignore the special variable with index zero, which is always assigned at the root to true
        PropositionalVariableGeneratorIterator::new(1, self.num_propositional_variables())
    }

    pub fn pop_trail(&mut self) -> Literal {
        let last_literal = self.trail.pop().expect("Cannot pop empty trail?");
        self.undo_assignment(last_literal.get_propositional_variable());
        last_literal
    }

    pub fn is_variable_assigned_true(&self, variable: PropositionalVariable) -> bool {
        match self.assignment_info[variable] {
            PropositionalAssignmentInfo::Assigned {
                truth_value,
                decision_level: _,
                constraint_reference: _,
            } => truth_value,
            PropositionalAssignmentInfo::Unassigned => false,
        }
    }

    pub fn is_variable_assigned_false(&self, variable: PropositionalVariable) -> bool {
        match self.assignment_info[variable] {
            PropositionalAssignmentInfo::Assigned {
                truth_value,
                decision_level: _,
                constraint_reference: _,
            } => !truth_value,
            PropositionalAssignmentInfo::Unassigned => false,
        }
    }

    pub fn is_literal_assigned_true(&self, literal: Literal) -> bool {
        if literal.is_positive() {
            self.is_variable_assigned_true(literal.get_propositional_variable())
        } else {
            self.is_variable_assigned_false(literal.get_propositional_variable())
        }
    }

    pub fn is_literal_assigned_false(&self, literal: Literal) -> bool {
        self.is_literal_assigned(literal) && !self.is_literal_assigned_true(literal)
    }

    pub fn is_literal_assigned(&self, literal: Literal) -> bool {
        self.is_variable_assigned(literal.get_propositional_variable())
    }

    pub fn is_literal_unassigned(&self, literal: Literal) -> bool {
        self.is_variable_unassigned(literal.get_propositional_variable())
    }

    pub fn is_variable_unassigned(&self, variable: PropositionalVariable) -> bool {
        self.assignment_info[variable] == PropositionalAssignmentInfo::Unassigned
    }

    pub fn is_variable_assigned(&self, variable: PropositionalVariable) -> bool {
        self.assignment_info[variable] != PropositionalAssignmentInfo::Unassigned
    }

    pub fn is_literal_root_assignment(&self, literal: Literal) -> bool {
        if self.is_literal_unassigned(literal) {
            false
        } else {
            self.get_variable_assignment_level(literal.get_propositional_variable()) == 0
        }
    }

    pub fn is_variable_propagated(&self, variable: PropositionalVariable) -> bool {
        match self.assignment_info[variable] {
            PropositionalAssignmentInfo::Unassigned => false,
            PropositionalAssignmentInfo::Assigned {
                truth_value: _,
                decision_level: _,
                constraint_reference,
            } => !constraint_reference.is_null(),
        }
    }

    pub fn is_literal_propagated(&self, literal: Literal) -> bool {
        self.is_variable_propagated(literal.get_propositional_variable())
    }

    pub fn get_variable_assignment_level(&self, variable: PropositionalVariable) -> u32 {
        match self.assignment_info[variable] {
            PropositionalAssignmentInfo::Unassigned => {
                panic!("Unassigned variables do not have assignment levels");
            }
            PropositionalAssignmentInfo::Assigned {
                truth_value: _,
                decision_level,
                constraint_reference: _,
            } => decision_level,
        }
    }

    pub fn get_literal_assignment_level(&self, literal: Literal) -> u32 {
        self.get_variable_assignment_level(literal.get_propositional_variable())
    }

    pub fn get_variable_reason_constraint(
        &self,
        variable: PropositionalVariable,
    ) -> ConstraintReference {
        match self.assignment_info[variable] {
            PropositionalAssignmentInfo::Unassigned => {
                panic!("Unassigned variables do not have reason codes levels");
            }
            PropositionalAssignmentInfo::Assigned {
                truth_value: _,
                decision_level: _,
                constraint_reference,
            } => constraint_reference,
        }
    }

    pub fn get_literal_reason_constraint(&self, literal: Literal) -> ConstraintReference {
        self.get_variable_reason_constraint(literal.get_propositional_variable())
    }

    fn make_assignment(
        &mut self,
        true_literal: Literal,
        constraint_reference: ConstraintReference,
    ) {
        pumpkin_assert_simple!(self.is_literal_unassigned(true_literal));

        self.assignment_info[true_literal.get_propositional_variable()] =
            PropositionalAssignmentInfo::Assigned {
                truth_value: true_literal.is_positive(),
                decision_level: self.get_decision_level(),
                constraint_reference,
            };

        self.trail.push(true_literal);
    }

    pub fn undo_assignment(&mut self, variable: PropositionalVariable) {
        pumpkin_assert_moderate!(self.is_variable_assigned(variable));

        self.assignment_info[variable] = PropositionalAssignmentInfo::Unassigned;
    }

    pub fn enqueue_decision_literal(&mut self, decision_literal: Literal) {
        pumpkin_assert_simple!(!self.is_literal_assigned(decision_literal));

        self.make_assignment(
            decision_literal,
            ConstraintReference::create_null_reference(),
        );
    }

    pub fn enqueue_propagated_literal(
        &mut self,
        propagated_literal: Literal,
        constraint_reference: ConstraintReference,
    ) {
        pumpkin_assert_simple!(!self.is_literal_assigned(propagated_literal));
        pumpkin_assert_simple!(!constraint_reference.is_null());

        self.make_assignment(propagated_literal, constraint_reference);
    }

    pub fn synchronise(&mut self, new_decision_level: u32) {
        pumpkin_assert_simple!(new_decision_level < self.current_decision_level);
        pumpkin_assert_simple!(
            self.trail.len() == (self.trail_delimiter[new_decision_level as usize] as usize),
            "It is expected that the solver would pop the trail before calling this method."
        );

        self.current_decision_level = new_decision_level;
        self.trail_delimiter.truncate(new_decision_level as usize);
    }

    pub fn is_at_the_root_level(&self) -> bool {
        self.current_decision_level == 0
    }

    pub fn num_assigned_propositional_variables(&self) -> u32 {
        self.trail.len() as u32
    }
}

#[derive(PartialEq)]
enum PropositionalAssignmentInfo {
    Assigned {
        truth_value: bool,
        decision_level: u32,
        constraint_reference: ConstraintReference,
    },
    Unassigned,
}
