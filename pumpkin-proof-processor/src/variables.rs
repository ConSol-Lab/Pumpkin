use std::rc::Rc;

use pumpkin_core::containers::HashMap;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

#[derive(Clone, Debug, Default)]
pub(crate) struct Variables {
    constant_map: HashMap<i32, DomainId>,
    variable_map: HashMap<Rc<str>, DomainId>,
    domain_to_name: HashMap<DomainId, Rc<str>>,
}

impl Variables {
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

    pub(crate) fn resolve(
        &mut self,
        variable: fzn_rs::VariableExpr<i32>,
        state: &mut State,
    ) -> DomainId {
        match variable {
            fzn_rs::VariableExpr::Identifier(name) => {
                self.variable_map.get(&name).copied().unwrap()
            }
            fzn_rs::VariableExpr::Constant(value) => *self
                .constant_map
                .entry(value)
                .or_insert_with(|| state.new_interval_variable(value, value, None)),
        }
    }
}
