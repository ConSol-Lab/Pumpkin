use std::rc::Rc;

pub use pumpkin_core_derive::ScopeItem;

use crate::predicates::Predicate;
use crate::variables::DomainId;

/// The scope of a constraint is the collection of variables involved in the relation.
///
/// A domain may occur more than once, for instance when two terms share a variable.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    domains: Vec<DomainId>,
}

impl Scope {
    pub fn add_domain(&mut self, domain_id: DomainId) {
        self.domains.push(domain_id);
    }

    pub fn domains(&self) -> impl ExactSizeIterator<Item = DomainId> + '_ {
        self.domains.iter().copied()
    }
}

/// Implemented by everything a constraint description is made of: a variable adds its domains to
/// the scope, and data without variables, such as a constant, adds nothing.
///
/// Usually derived, so that every field of a description is accounted for:
///
/// ```
/// use pumpkin_core::checkers::ScopeItem;
/// use pumpkin_core::variables::DomainId;
///
/// /// The description of the constraint `x + y <= bound`.
/// #[derive(Clone, Debug, ScopeItem)]
/// pub struct SumDescription {
///     pub x: DomainId,
///     pub y: DomainId,
///     pub bound: i32,
/// }
/// ```
///
/// The scope of a `SumDescription` holds the domains of `x` and `y`; the constant `bound` adds
/// nothing.
pub trait ScopeItem {
    fn add_to_scope(&self, scope: &mut Scope);
}

impl ScopeItem for i32 {
    fn add_to_scope(&self, _: &mut Scope) {
        // A constant has no domain.
    }
}

impl ScopeItem for Predicate {
    fn add_to_scope(&self, scope: &mut Scope) {
        scope.add_domain(self.get_domain());
    }
}

impl<Item: ScopeItem> ScopeItem for [Item] {
    fn add_to_scope(&self, scope: &mut Scope) {
        for item in self {
            item.add_to_scope(scope);
        }
    }
}

impl<Item: ScopeItem + ?Sized> ScopeItem for Box<Item> {
    fn add_to_scope(&self, scope: &mut Scope) {
        self.as_ref().add_to_scope(scope);
    }
}

impl<Item: ScopeItem + ?Sized> ScopeItem for Rc<Item> {
    fn add_to_scope(&self, scope: &mut Scope) {
        self.as_ref().add_to_scope(scope);
    }
}

impl<Item: ScopeItem> ScopeItem for Vec<Item> {
    fn add_to_scope(&self, scope: &mut Scope) {
        self.as_slice().add_to_scope(scope);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::StorageKey;

    #[derive(ScopeItem)]
    struct Task {
        start: DomainId,
        duration: i32,
    }

    #[derive(ScopeItem)]
    struct Description {
        variable: DomainId,
        terms: Box<[DomainId]>,
        tasks: Vec<Task>,
        bound: i32,
    }

    #[test]
    fn every_variable_of_every_field_is_in_the_scope() {
        let domain = |index| DomainId::create_from_index(index);
        let description = Description {
            variable: domain(1),
            terms: Box::from([domain(2), domain(3)]),
            tasks: vec![
                Task {
                    start: domain(4),
                    duration: 2,
                },
                Task {
                    start: domain(5),
                    duration: 3,
                },
            ],
            bound: 7,
        };

        let mut scope = Scope::default();
        description.add_to_scope(&mut scope);

        assert_eq!(
            scope.domains().collect::<Vec<_>>(),
            (1..=5).map(domain).collect::<Vec<_>>()
        );
    }
}
