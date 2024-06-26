pub mod results;
mod solver;

pub use crate::engine::variables;
pub use solver::*;

pub mod options {
    pub use crate::basic_types::sequence_generators::SequenceGeneratorType;
    pub use crate::encoders::PseudoBooleanEncoding;
    pub use crate::engine::LearnedClauseSortingStrategy;
    pub use crate::engine::LearningOptions;
    pub use crate::engine::RestartOptions;
    pub use crate::engine::SatisfactionSolverOptions as SolverOptions;
}

pub mod termination {
    pub use crate::engine::termination::combinator::*;
    pub use crate::engine::termination::indefinite::*;
    pub use crate::engine::termination::os_signal::*;
    pub use crate::engine::termination::time_budget::*;
    pub use crate::engine::termination::TerminationCondition;
}

pub mod statistics {
    pub use crate::basic_types::statistic_logging::statistic_logger::*;
}

pub use crate::engine::proof;
