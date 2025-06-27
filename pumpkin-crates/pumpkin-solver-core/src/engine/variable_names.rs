use crate::basic_types::HashMap;
use crate::engine::variables::DomainId;

#[derive(Debug, Default)]

pub(crate) struct VariableNames {
    integers: HashMap<DomainId, String>,
}

impl VariableNames {
    /// Get the name associated with a domain id.
    pub(crate) fn get_int_name(&self, domain_id: DomainId) -> Option<&str> {
        self.integers.get(&domain_id).map(|s| s.as_str())
    }

    /// Add a name to the integer variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_integer(&mut self, integer: DomainId, name: String) {
        let _ = self.integers.insert(integer, name);
    }
}
