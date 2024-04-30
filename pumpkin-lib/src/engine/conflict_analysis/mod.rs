//! Contains algorithms for conflict analysis, core extraction, and clause minimisation.
//! The algorithms use resolution and implement the 1uip and all decision literal learning schemes

mod conflict_analysis_context;
mod resolution_conflict_analyser;

pub use conflict_analysis_context::ConflictAnalysisContext;
pub use resolution_conflict_analyser::ConflictAnalysisResult;
pub use resolution_conflict_analyser::ResolutionConflictAnalyser;
