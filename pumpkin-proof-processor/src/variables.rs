use std::rc::Rc;

use pumpkin_core::containers::HashMap;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

/// The names the proof gives the variables of the state, in both directions.
#[derive(Clone, Debug, Default)]
pub(crate) struct Variables {
    variable_map: HashMap<Rc<str>, DomainId>,
    domain_to_name: HashMap<DomainId, Rc<str>>,
}

impl Variables {
    /// The names of every named variable of the state.
    pub(crate) fn from_state(state: &State) -> Variables {
        let mut variables = Variables::default();
        for (domain_id, name) in state.named_variables() {
            variables.add_variable(Rc::from(name), domain_id);
        }
        variables
    }

    pub(crate) fn add_variable(&mut self, name: Rc<str>, domain_id: DomainId) {
        let _ = self.variable_map.insert(Rc::clone(&name), domain_id);
        let _ = self.domain_to_name.insert(domain_id, name);
    }

    pub(crate) fn get_domain_by_name(&self, name: &str) -> Option<DomainId> {
        self.variable_map.get(name).copied()
    }

    pub(crate) fn get_name_for_domain(&self, domain_id: DomainId) -> Option<Rc<str>> {
        self.domain_to_name.get(&domain_id).cloned()
    }
}
