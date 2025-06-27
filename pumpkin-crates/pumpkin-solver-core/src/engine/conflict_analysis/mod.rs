//! Contains algorithms for conflict analysis, core extraction, and clause minimisation.
//! The algorithms use resolution and implement the 1uip and all decision literal learning schemes

mod conflict_analysis_context;
mod learned_nogood;
mod minimisers;
mod resolvers;

pub(crate) use conflict_analysis_context::ConflictAnalysisContext;
pub(crate) use learned_nogood::*;
pub(crate) use minimisers::*;
pub(crate) use resolvers::*;
