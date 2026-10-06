use crate::AtomicConstraint;
use crate::IntExt;
#[cfg(doc)]
use crate::VariableState;
#[cfg(doc)]
use crate::checkers::RetentionChecker;

/// A read-only view of the domains of variables.
///
/// A [`RetentionChecker`] reads the domains through this view, so that it can check the domains of
/// the solver without copying them. The [`VariableState`] built for an inference check is also a
/// view.
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
