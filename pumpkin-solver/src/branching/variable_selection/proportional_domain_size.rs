use super::VariableSelector;
use crate::branching::brancher::BrancherEvents;
use crate::branching::SelectionContext;
use crate::pumpkin_assert_extreme;
use crate::variables::DomainId;

#[derive(Debug)]
pub struct ProportionalDomainSize {
    /// A list domain sizes, used as weights to select the next variable.
    domain_sizes: Vec<f64>,
    /// For every entry in `domain_sizes`, the index into `variables` for the corresponding
    /// variable.
    weights_idx_to_variables: Vec<usize>,

    variables: Vec<DomainId>,
}

impl ProportionalDomainSize {
    pub fn new(variables: &[DomainId]) -> Self {
        ProportionalDomainSize {
            domain_sizes: vec![1.0; variables.len()],
            weights_idx_to_variables: (0..variables.len()).collect(),
            variables: variables.to_vec(),
        }
    }
}

impl VariableSelector<DomainId> for ProportionalDomainSize {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        // Update domain sizes and disregard fixed domains. Note, we go through the vectors from
        // back to front as we will be deleting from them.
        for weight_idx in (0..self.weights_idx_to_variables.len()).rev() {
            let variable_idx = self.weights_idx_to_variables[weight_idx];
            let domain_id = self.variables[variable_idx];

            if context.is_integer_fixed(domain_id) {
                // Fixed domains can be disregarded until the next backtrack.
                let _ = self.weights_idx_to_variables.swap_remove(weight_idx);
                let _ = self.domain_sizes.swap_remove(weight_idx);
                continue;
            }

            self.domain_sizes[weight_idx] = context.get_size_of_domain(domain_id) as f64;
        }

        if self.domain_sizes.is_empty() {
            pumpkin_assert_extreme!(self
                .variables
                .iter()
                .all(|variable| context.is_integer_fixed(*variable)), "There was a variable which was not fixed while the proportional domain selector returned None");
            return None;
        }

        context
            .random()
            .get_weighted_choice(&self.domain_sizes)
            .map(|index| self.variables[self.weights_idx_to_variables[index]])
    }

    fn on_backtrack(&mut self) {
        // We need to add back the fixed variables, for now we just add back everything (while
        // ensuring that no extra memory allocations are done by simply clearing and inserting)
        self.domain_sizes.clear();
        self.weights_idx_to_variables.clear();

        for idx in 0..self.variables.len() {
            self.domain_sizes.push(1.0);
            self.weights_idx_to_variables.push(idx);
        }
    }

    fn get_relevant_brancher_events(&self) -> Vec<BrancherEvents> {
        vec![BrancherEvents::Backtrack]
    }
}
