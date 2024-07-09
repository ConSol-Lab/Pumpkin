use crate::basic_types::HashMap;
use crate::engine::variables::DomainId;
use crate::engine::variables::PropositionalVariable;

#[derive(Debug, Default)]
pub(crate) struct VariableNames {
    propositionals: HashMap<PropositionalVariable, String>,
    integers: HashMap<DomainId, String>,
}

impl VariableNames {
    /// Get the name associated with a propositional variable.
    pub(crate) fn get_propositional_name(
        &self,
        propositional: PropositionalVariable,
    ) -> Option<&str> {
        self.propositionals.get(&propositional).map(|s| s.as_str())
    }

    /// Get the name associated with a domain id.
    pub(crate) fn get_int_name(&self, domain_id: DomainId) -> Option<&str> {
        self.integers.get(&domain_id).map(|s| s.as_str())
    }

    /// Add a name to the propositional variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_propositional(&mut self, variable: PropositionalVariable, name: String) {
        let _ = self.propositionals.insert(variable, name);
    }

    /// Add a name to the integer variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_integer(&mut self, integer: DomainId, name: String) {
        let _ = self.integers.insert(integer, name);
    }
}
