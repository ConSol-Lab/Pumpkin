use std::sync::Arc;

use crate::containers::HashMap;
use crate::engine::variables::DomainId;

#[derive(Debug, Default, Clone)]
pub(crate) struct VariableNames {
    integers: HashMap<DomainId, Arc<str>>,
}

impl VariableNames {
    /// Get the name associated with a domain id.
    pub(crate) fn get_int_name(&self, domain_id: DomainId) -> Option<&str> {
        self.integers.get(&domain_id).map(|s| s.as_ref())
    }

    /// Every named variable with its name.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (DomainId, &str)> + '_ {
        self.integers
            .iter()
            .map(|(domain, name)| (*domain, name.as_ref()))
    }

    /// Add a name to the integer variable. This will override existing the name if it
    /// exists.
    pub(crate) fn add_integer(&mut self, integer: DomainId, name: Arc<str>) {
        let _ = self.integers.insert(integer, name);
    }
}
