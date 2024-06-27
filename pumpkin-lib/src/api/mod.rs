pub mod results;
mod solver;

pub use solver::*;

pub use crate::basic_types::Function;
pub use crate::basic_types::Solution;
pub use crate::engine::variables;

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

pub mod predicates {
    pub use crate::engine::predicates::integer_predicate::IntegerPredicate;
    pub use crate::engine::predicates::predicate::Predicate;
    pub use crate::engine::predicates::predicate_constructor::PredicateConstructor;
}

pub mod hashed_structures {
    pub use crate::basic_types::HashMap;
    pub use crate::basic_types::HashSet;
}

pub use crate::basic_types::Stopwatch;

pub mod encodings {
    pub use crate::encoders::CardinalityNetworkEncoder;
    pub use crate::encoders::GeneralisedTotaliserEncoder;
    pub use crate::encoders::PseudoBooleanConstraintEncoder;
    pub use crate::encoders::SingleIntegerEncoder;
}

pub mod asserts {
    pub use crate::pumpkin_assert_advanced;
    pub use crate::pumpkin_assert_eq_simple;
    pub use crate::pumpkin_assert_extreme;
    pub use crate::pumpkin_assert_moderate;
    pub use crate::pumpkin_assert_ne_moderate;
    pub use crate::pumpkin_assert_ne_simple;
    pub use crate::pumpkin_assert_simple;
    pub use crate::pumpkin_asserts::PUMPKIN_ASSERT_ADVANCED;
    pub use crate::pumpkin_asserts::PUMPKIN_ASSERT_EXTREME;
    pub use crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION;
    pub use crate::pumpkin_asserts::PUMPKIN_ASSERT_MODERATE;
    pub use crate::pumpkin_asserts::PUMPKIN_ASSERT_SIMPLE;
}
