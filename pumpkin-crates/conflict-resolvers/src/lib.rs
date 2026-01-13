use crate::resolvers::AnalysisMode;
use crate::resolvers::ResolutionResolver;

pub mod minimisers;
pub mod resolvers;

#[cfg(doc)]
use pumpkin_core::conflict_resolving::ConflictResolver;
#[cfg(doc)]
use pumpkin_core::conflict_resolving::CoreExtractor;

/// Returns a default [`CoreExtractor`].
pub fn default_core_extractor() -> ResolutionResolver {
    ResolutionResolver::new(AnalysisMode::AllDecision)
}

/// Returns a default [`ConflictResolver`].
pub fn default_conflict_resolver() -> ResolutionResolver {
    ResolutionResolver::new(AnalysisMode::OneUIP)
}
