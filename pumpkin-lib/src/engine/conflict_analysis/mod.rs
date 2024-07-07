//! Contains algorithms for conflict analysis, core extraction, and clause minimisation.
//! The algorithms use resolution and implement the 1uip and all decision literal learning schemes

mod conflict_analysis_context;
mod resolution_conflict_analyser;

mod advanced_nogood;
mod conflict_analysis_nogood_context;
mod resolution_nogood_conflict_analyser;

pub use conflict_analysis_context::ConflictAnalysisContext;
pub use conflict_analysis_nogood_context::ConflictAnalysisNogoodContext;
pub use resolution_conflict_analyser::*;
pub use resolution_nogood_conflict_analyser::LearnedNogood;
pub use resolution_nogood_conflict_analyser::ResolutionNogoodConflictAnalyser;
