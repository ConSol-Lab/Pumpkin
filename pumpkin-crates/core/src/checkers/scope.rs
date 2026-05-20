use crate::containers::HashMap;
use crate::propagation::LocalId;
use crate::variables::DomainId;

/// The scope of a constraint is the collection of variables involved in the relation.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    domains: HashMap<LocalId, DomainId>,
}

impl Scope {
    /// Add a new domain to the scope with the given local id.
    ///
    /// Any previous occurrance of this local id will be overridden.
    pub fn add_domain(&mut self, local_id: LocalId, domain_id: DomainId) {
        let _ = self.domains.insert(local_id, domain_id);
    }

    /// The integer domains in the scope with the [`LocalId`]s they are registered.
    pub fn domains(&self) -> impl ExactSizeIterator<Item = (LocalId, DomainId)> {
        self.domains.iter().map(|(lid, did)| (*lid, *did))
    }
}

impl<VA, VB, VC> From<((LocalId, &VA), (LocalId, &VB), (LocalId, &VC))> for Scope
where
    VA: ScopeItem,
    VB: ScopeItem,
    VC: ScopeItem,
{
    fn from(
        ((la, va), (lb, vb), (lc, vc)): ((LocalId, &VA), (LocalId, &VB), (LocalId, &VC)),
    ) -> Self {
        let mut scope = Scope::default();

        va.add_to_scope(&mut scope, la);
        vb.add_to_scope(&mut scope, lb);
        vc.add_to_scope(&mut scope, lc);

        scope
    }
}

pub trait ScopeItem {
    /// Adds self to the given scope with the given [`LocalId`].
    fn add_to_scope(&self, scope: &mut Scope, local_id: LocalId);
}

impl ScopeItem for i32 {
    fn add_to_scope(&self, _: &mut Scope, _: LocalId) {
        // Do nothing
    }
}
