use crate::basic_types::KeyedVec;
use crate::basic_types::PropositionalVariable;

#[derive(Default, Debug)]
pub struct PropositionalValueSelector {
    truth_values: KeyedVec<PropositionalVariable, CandidateTruthAssignment>,
}

#[derive(Debug)]
struct CandidateTruthAssignment {
    value: bool,
    frozen: bool,
}

impl PropositionalValueSelector {
    pub fn grow(&mut self) {
        self.truth_values.push(CandidateTruthAssignment {
            value: false,
            frozen: false,
        });
    }

    pub fn select_value(&self, variable: PropositionalVariable) -> bool {
        self.truth_values[variable].value
    }

    pub fn update_if_not_frozen(&mut self, variable: PropositionalVariable, new_truth_value: bool) {
        //probably better to avoid the explicit 'if' statement
        if !self.truth_values[variable].frozen {
            self.truth_values[variable].value = new_truth_value;
        }
    }

    pub fn update_and_freeze(&mut self, variable: PropositionalVariable, new_truth_value: bool) {
        self.truth_values[variable].value = new_truth_value;
        self.truth_values[variable].frozen = true;
    }
}
