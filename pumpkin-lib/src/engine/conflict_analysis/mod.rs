//! Contains algorithms for conflict analysis, core extraction, and clause minimisation.
//! The algorithms use resolution and implement the 1uip and all decision literal learning schemes
mod conflict_analysis_context;
mod recursive_minimisation;
mod semantic_minimiser;

pub(crate) use conflict_analysis_context::ConflictAnalysisContext;
pub(crate) use recursive_minimisation::*;
pub(crate) use semantic_minimiser::*;
mod resolvers;
pub use resolvers::*;
