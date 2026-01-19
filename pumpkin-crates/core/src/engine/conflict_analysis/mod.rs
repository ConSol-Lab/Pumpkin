//! Contains algorithms for conflict analysis, core extraction, and clause minimisation.
//! The algorithms use resolution and implement the 1uip and all decision literal learning schemes

mod conflict_analysis_context;
mod minimiser;
mod resolvers;

pub use conflict_analysis_context::ConflictAnalysisContext;
pub use minimiser::NogoodMinimiser;
pub use resolvers::ConflictResolver;
pub use resolvers::CoreExtractor;
pub use resolvers::LearnedNogood;
