use std::fmt::Display;

use clap::ValueEnum;

#[derive(Debug, Copy, Clone, Default, ValueEnum)]
pub enum DecisionDiagramIntersection {
    /// Construct a DD separately for each constraint
    #[default]
    None,
    /// Construct a single DD for all applicable constraints
    All,
}

impl Display for DecisionDiagramIntersection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecisionDiagramIntersection::None => write!(f, "none"),
            DecisionDiagramIntersection::All => write!(f, "all"),
        }
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct DecisionDiagramOptions {
    /// Maximum width of constructed decision diagrams
    pub max_width: usize,
    /// Defines how and if the preprocessor
    /// chooses the constraints to be intersected
    pub intersection_strategy: DecisionDiagramIntersection,
}

impl DecisionDiagramOptions {
    pub fn new(max_width: usize, intersection_strategy: DecisionDiagramIntersection) -> Self {
        Self {
            max_width,
            intersection_strategy,
        }
    }
}
