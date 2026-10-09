use crate::AtomicConstraint;
use crate::IntExt;
#[cfg(doc)]
use crate::VariableState;

/// A read-only view of the domains of variables.
///
/// The [`VariableState`] built for a conflict check is a view, and so are the domains of a
/// solver, so code that reads domains, such as a check whether a constraint is satisfied, works on
/// both without copying them.
pub trait DomainView<Atomic: AtomicConstraint> {
    fn lower_bound(&self, identifier: &Atomic::Identifier) -> IntExt;

    fn upper_bound(&self, identifier: &Atomic::Identifier) -> IntExt;

    fn contains(&self, identifier: &Atomic::Identifier, value: i32) -> bool;

    /// The values within the bounds of the variable that are not in its domain.
    fn holes<'a>(&'a self, identifier: &Atomic::Identifier) -> Box<dyn Iterator<Item = i32> + 'a>;

    /// Whether the atomic constraint holds in every value of the domains.
    fn is_true(&self, atomic: &Atomic) -> bool;

    fn fixed_value(&self, identifier: &Atomic::Identifier) -> Option<i32> {
        let lower_bound = self.lower_bound(identifier).as_int()?;
        (IntExt::Int(lower_bound) == self.upper_bound(identifier)).then_some(lower_bound)
    }

    /// The values in the domain of the variable, or `None` if the domain is unbounded.
    fn iter_domain<'a>(
        &'a self,
        identifier: &Atomic::Identifier,
    ) -> Option<Box<dyn Iterator<Item = i32> + 'a>>
    where
        Atomic::Identifier: 'a,
    {
        let lower_bound = self.lower_bound(identifier).as_int()?;
        let upper_bound = self.upper_bound(identifier).as_int()?;
        let values = (lower_bound..=upper_bound)
            .filter(|&value| self.contains(identifier, value))
            .collect::<Vec<_>>();

        Some(Box::new(values.into_iter()))
    }
}
