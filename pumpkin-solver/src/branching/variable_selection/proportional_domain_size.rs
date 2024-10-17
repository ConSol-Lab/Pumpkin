use super::VariableSelector;
use crate::branching::SelectionContext;
use crate::pumpkin_assert_extreme;
use crate::variables::DomainId;

#[derive(Debug)]
pub struct ProportionalDomainSize {
    variables_under_consideration: Vec<(DomainId, i32)>,
    variables: Vec<DomainId>,
}

impl ProportionalDomainSize {
    pub fn new(variables: &[DomainId]) -> Self {
        ProportionalDomainSize {
            variables_under_consideration: variables
                .iter()
                .map(|domain_id| (*domain_id, 1))
                .collect(),
            variables: variables.to_vec(),
        }
    }
}

impl VariableSelector<DomainId> for ProportionalDomainSize {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        // We remove all of the fixed variables and update the size of the domain for all others
        self.variables_under_consideration
            .retain_mut(|(domain_id, size)| {
                if context.is_integer_fixed(*domain_id) {
                    return false;
                }
                *size = context.get_size_of_domain(*domain_id);
                true
            });

        if self.variables_under_consideration.is_empty() {
            pumpkin_assert_extreme!(self
                .variables
                .iter()
                .all(|variable| context.is_integer_fixed(*variable)), "There was a variable which was not fixed while the proportional domain selector returned None");
            return None;
        }

        context
            .random()
            .weighted_choice_domain_id(&self.variables_under_consideration)
    }

    fn on_backtrack(&mut self) {
        // We need to add back the fixed variables, for now we just add back everything (while
        // ensuring that no extra memory allocations are done by simply clearing and inserting)
        self.variables_under_consideration.clear();
        for domain_id in self.variables.iter() {
            self.variables_under_consideration.push((*domain_id, 1))
        }
    }
}
