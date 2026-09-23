use pumpkin_checking::VariableState;

use crate::containers::HashMap;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagation::LocalId;
use crate::propagation::ReadDomains;
use crate::variables::DomainId;

/// The scope of a constraint is the collection of variables involved in the relation.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    domains: HashMap<LocalId, DomainId>,
}

impl FromIterator<(LocalId, DomainId)> for Scope {
    fn from_iter<T: IntoIterator<Item = (LocalId, DomainId)>>(iter: T) -> Self {
        Scope {
            domains: iter.into_iter().collect(),
        }
    }
}

impl Scope {
    /// The scope of the given variables, with the [`LocalId`] of each variable its position.
    pub fn from_variables<'a, Variable: ScopeItem + 'a>(
        variables: impl IntoIterator<Item = &'a Variable>,
    ) -> Scope {
        let mut scope = Scope::default();
        for (index, variable) in variables.into_iter().enumerate() {
            variable.add_to_scope(&mut scope, LocalId::from(index as u32));
        }
        scope
    }

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

    /// The current domains of the variables in the scope, which is the state a retention checker
    /// reads.
    pub fn snapshot(&self, domains: &impl ReadDomains) -> VariableState<Predicate> {
        let mut state = VariableState::default();

        for (_, domain) in self.domains() {
            let lower_bound = domains.lower_bound(&domain);
            let upper_bound = domains.upper_bound(&domain);
            let _ = state.apply(&predicate![domain >= lower_bound]);
            let _ = state.apply(&predicate![domain <= upper_bound]);

            for hole in domains.get_holes(&domain) {
                let _ = state.apply(&predicate![domain != hole]);
            }
        }

        state
    }
}

macro_rules! impl_scope_from_tuple {
    ($($lid_name:ident,$var_name:ident : $ty_name:ident),+) => {
        impl<$($ty_name),+> From<($((LocalId, &$ty_name)),+)> for Scope
        where
            $($ty_name: ScopeItem),+
        {
            fn from(
                ($(($lid_name, $var_name)),+): ($((LocalId, &$ty_name)),+),
            ) -> Self {
                let mut scope = Scope::default();

                $($var_name.add_to_scope(&mut scope, $lid_name);)+

                scope
            }
        }
    };
}

impl_scope_from_tuple!(la,va: VA, lb,vb: VB);
impl_scope_from_tuple!(la,va: VA, lb,vb: VB, lc,vc: VC);

pub trait ScopeItem {
    /// Adds self to the given scope with the given [`LocalId`].
    fn add_to_scope(&self, scope: &mut Scope, local_id: LocalId);
}

impl ScopeItem for i32 {
    fn add_to_scope(&self, _: &mut Scope, _: LocalId) {
        // Do nothing
    }
}
