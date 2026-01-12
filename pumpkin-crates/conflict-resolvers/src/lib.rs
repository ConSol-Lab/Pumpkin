use crate::resolvers::AnalysisMode;
use crate::resolvers::ResolutionResolver;

pub mod minimisers;
pub mod resolvers;

/// The default [`ConflictResolver`].
pub type DefaultResolver = ResolutionResolver;

pub fn default_core_extractor() -> ResolutionResolver {
    ResolutionResolver::new(AnalysisMode::AllDecision)
}
