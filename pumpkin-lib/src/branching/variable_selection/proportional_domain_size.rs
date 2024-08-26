use super::VariableSelector;
use crate::branching::SelectionContext;
use crate::variables::DomainId;

#[derive(Debug)]
pub struct ProportionalDomainSize {
    variables: Vec<DomainId>,
}

impl ProportionalDomainSize {
    pub fn new(variables: &[DomainId]) -> Self {
        ProportionalDomainSize {
            variables: variables.to_vec(),
        }
    }
}

impl VariableSelector<DomainId> for ProportionalDomainSize {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        let unfixed_variables = self
            .variables
            .iter()
            .filter(|variable| !context.is_integer_fixed(**variable))
            // TODO: Maybe we should use the exact size of the domain here rather than the
            // approximate size?
            .map(|variable| (*variable, context.get_size_of_domain(*variable)))
            .collect::<Vec<_>>();

        if unfixed_variables.is_empty() {
            return None;
        }

        context
            .random()
            .weighted_choice_domain_id(unfixed_variables)
    }
}
