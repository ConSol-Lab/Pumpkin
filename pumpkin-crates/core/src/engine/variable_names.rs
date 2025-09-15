use crate::containers::HashMap;
use crate::engine::variables::DomainId;

#[derive(Debug, Default)]

pub struct VariableNames {
    integers: HashMap<DomainId, String>,
    domain_by_name: HashMap<String, DomainId>,
}

impl VariableNames {
    /// Get the name associated with a domain id.
    pub(crate) fn get_int_name(&self, domain_id: DomainId) -> Option<&str> {
        self.integers.get(&domain_id).map(|s| s.as_str())
    }

    /// Get the [`DomainId`] associated with the given name.
    pub(crate) fn get_domain_by_name(&self, name: &str) -> Option<DomainId> {
        self.domain_by_name.get(name).copied()
    }

    /// Add a name to the integer variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_integer(&mut self, integer: DomainId, name: String) {
        let _ = self.integers.insert(integer, name.clone());
        let _ = self.domain_by_name.insert(name, integer);
    }
}
