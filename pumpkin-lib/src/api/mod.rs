pub mod results;
mod solver;

pub use crate::basic_types::Function;
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
    pub use crate::optimisation::log_statistics;
    pub use crate::optimisation::log_statistics_with_objective;
}

pub use crate::engine::proof;

pub mod predicates {
    pub use crate::engine::predicates::integer_predicate::IntegerPredicate;
    pub use crate::engine::predicates::predicate::Predicate;
    pub use crate::engine::predicates::predicate_constructor::PredicateConstructor;
}
pub use crate::pumpkin_asserts::*;

pub mod hashed_structures {
    pub use crate::basic_types::HashMap;
    pub use crate::basic_types::HashSet;
}
