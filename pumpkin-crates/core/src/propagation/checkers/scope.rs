use crate::propagation::LocalId;
use crate::variables::DomainId;

/// The scope of a propagator consists of the variables the propagator reasons over.
#[derive(Clone, Debug)]
pub struct Scope(Vec<(LocalId, DomainId)>);

impl Scope {
    /// Iterate over all domains in the scope.
    ///
    /// For each [`DomainId`] this indicates the [`LocalId`] that the domain is registered as.
    /// It can happen that a propagator registers the same [`DomainId`] for different
    /// [`LocalId`]s.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (LocalId, DomainId)> {
        self.0.iter().copied()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Clone, Debug, Default)]
pub struct ScopeBuilder(Vec<(LocalId, DomainId)>);

impl ScopeBuilder {
    pub fn add(&mut self, local_id: LocalId, domain_id: DomainId) {
        self.0.push((local_id, domain_id));
    }

    pub fn build(self) -> Scope {
        Scope(self.0)
    }
}
