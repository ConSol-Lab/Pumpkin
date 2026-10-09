use std::rc::Rc;

use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::ConstraintDescription;
use crate::variables::DomainId;

/// The scope is the set of variables used by the constraint.
///
/// A domain may occur more than once, for instance when two terms share a variable,
/// although we aim to avoid these situations in the solver by normalising constraints.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    domains: Vec<DomainId>,
}

impl Scope {
    pub fn extend(&mut self, domain_id: DomainId) {
        self.domains.push(domain_id);
    }

    pub fn domains(&self) -> impl ExactSizeIterator<Item = DomainId> + '_ {
        self.domains.iter().copied()
    }
}

/// Implemented by everything a [`ConstraintDescription`] is made of: a variable adds its domain to
/// the scope, and data without variables, such as a constant, adds nothing.
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
        scope.extend(self.get_domain());
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

/// Defines a struct and implements [`ScopeItem`] for it from all its fields, so that no field that
/// holds variables can be left out of the scope. Every field has to implement [`ScopeItem`].
/// The purpose of the macro is to make it harder to make mistakes,
/// e.g., forgetting to add a variable to the scope.
#[macro_export]
macro_rules! scoped_struct {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident $(<$($parameter:ident),+ $(,)?>)? {
            $($(#[$field_meta:meta])* $field_vis:vis $field:ident : $field_type:ty),* $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name $(<$($parameter),+>)? {
            $($(#[$field_meta])* $field_vis $field : $field_type),*
        }

        impl $(<$($parameter),+>)? $crate::checkers::ScopeItem for $name $(<$($parameter),+>)?
        where
            $($field_type: $crate::checkers::ScopeItem),*
        {
            fn add_to_scope(&self, scope: &mut $crate::checkers::Scope) {
                let _ = &scope;
                $($crate::checkers::ScopeItem::add_to_scope(&self.$field, scope);)*
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::StorageKey;

    crate::scoped_struct! {
        struct Task {
            start: DomainId,
            duration: i32,
        }
    }

    crate::scoped_struct! {
        struct Description {
            variable: DomainId,
            terms: Box<[DomainId]>,
            tasks: Vec<Task>,
            bound: i32,
        }
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
