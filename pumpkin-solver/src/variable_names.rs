use crate::basic_types::HashMap;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;

#[derive(Debug, Default)]
#[allow(dead_code)]
pub(crate) struct VariableNames {
    booleans: HashMap<Literal, String>,
    integers: HashMap<DomainId, String>,
}

impl VariableNames {
    #[allow(dead_code)]
    /// Get the name associated with a propositional variable.
    pub(crate) fn get_boolean_name(&self, propositional: Literal) -> Option<&str> {
        self.booleans.get(&propositional).map(|s| s.as_str())
    }

    #[allow(dead_code)]
    /// Get the name associated with a domain id.
    pub(crate) fn get_int_name(&self, domain_id: DomainId) -> Option<&str> {
        self.integers.get(&domain_id).map(|s| s.as_str())
    }

    #[allow(dead_code)]
    /// Add a name to the propositional variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_boolean(&mut self, variable: Literal, name: String) {
        let _ = self.booleans.insert(variable, name);
    }

    /// Add a name to the integer variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_integer(&mut self, integer: DomainId, name: String) {
        let _ = self.integers.insert(integer, name);
    }
}
