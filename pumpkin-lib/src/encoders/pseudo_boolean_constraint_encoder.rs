use std::fmt::Debug;
use std::fmt::Formatter;
/// The following facilitates easier reuse and consistency amongst pseudo-Boolean encoders
/// The idea is to separate the 'preprocessing' of the input and encoding algorithm
///     this way all encoders can benefit from the same preprocessing
///     and the encoding algorithm can then consider only canonical cases for its input
///
/// The trait 'PseudoBooleanConstraintEncoderInterface' provides the interface
///     encoders are expected to implement this trait
/// PseudoBooleanConstraintEncoder acts as a wrapper around the interface structs
use std::time::Instant;

use log::debug;
use thiserror::Error;

use super::CardinalityNetworkEncoder;
use super::GeneralisedTotaliserEncoder;
use super::SingleIntegerEncoder;
use crate::basic_types::Function;
use crate::basic_types::Literal;
use crate::basic_types::WeightedLiteral;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::DebugDyn;
use crate::pumpkin_assert_simple;

pub trait PseudoBooleanConstraintEncoderInterface {
    /// Add clauses that encode \sum w_i x_i <= k and returns the encoder object
    /// The encoder can later be used to strengthen the constraint (see 'strengthen_at_most_k')
    /// The method assumes the input is meaningful so the encoding cannot trivially fail, i.e.,
    ///     \sum w_i > k
    ///     0 < w_i <= k
    ///     x_i unassigned    
    ///     weighted_literals is not empty
    /// Recall that this trait is used in combination with 'PseudoBooleanConstraintEncoder<T>', which ensures the above conditions are met
    fn encode_at_most_k(
        weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<Self, EncodingError>
    where
        Self: Sized;

    /// Incrementally strengthen the encoding to encode \sum w_i x_i <= k
    /// Assumes the k is smaller than the previous k, and that encode_at_most_k has been called some time before
    fn strengthen_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError>;
}

#[derive(Clone, Copy, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum PseudoBooleanEncoding {
    GTE,
    CNE,
    SingleInteger,
}

impl std::fmt::Display for PseudoBooleanEncoding {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PseudoBooleanEncoding::GTE => write!(f, "gte"),
            PseudoBooleanEncoding::CNE => write!(f, "cne"),
            PseudoBooleanEncoding::SingleInteger => write!(f, "single_integer"),
        }
    }
}

/// main struct through which encoders are to be used
#[derive(Debug)]
pub struct PseudoBooleanConstraintEncoder {
    state: State,
    constant_term: u64,
    k_previous: u64,
    encoding_algorithm: PseudoBooleanEncoding,
}

enum State {
    New(Vec<WeightedLiteral>),
    Encoded(Box<dyn PseudoBooleanConstraintEncoderInterface>),
    Preprocessed(Vec<WeightedLiteral>),
    TriviallySatisfied,
    SingleIntegerNew(Vec<WeightedLiteral>),
    SingleInteger(SingleIntegerEncoder),
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            State::New(weighted_literals) => {
                f.debug_tuple("New").field(&weighted_literals).finish()
            }
            State::Encoded(_) => f
                .debug_tuple("Encoded")
                .field(&DebugDyn::from("PseudoBooleanConstraintEncoderInterface"))
                .finish(),
            State::Preprocessed(weighted_literals) => f
                .debug_tuple("Preprocessed")
                .field(&weighted_literals)
                .finish(),
            State::TriviallySatisfied => f.debug_tuple("TriviallySatisfied").finish(),
            State::SingleIntegerNew(weighted_literals) => f
                .debug_tuple("SingleIntegerNew")
                .field(&weighted_literals)
                .finish(),
            State::SingleInteger(_) => f
                .debug_tuple("SingleInteger")
                .field(&DebugDyn::from("PseudoBooleanConstraintEncoderInterface"))
                .finish(),
        }
    }
}

impl PseudoBooleanConstraintEncoder {
    pub fn new(
        weighted_literals: Vec<WeightedLiteral>,
        encoding_algorithm: PseudoBooleanEncoding,
    ) -> Self {
        pumpkin_assert_simple!(
            weighted_literals.iter().all(|term| term.weight > 0),
            "Zero-weight term detected, error?"
        );

        Self {
            state: State::New(weighted_literals),
            constant_term: 0,
            k_previous: 0,
            encoding_algorithm,
        }
    }

    pub fn from_single_integer_function(weighted_literals: Vec<WeightedLiteral>) -> Self {
        Self {
            state: State::SingleIntegerNew(weighted_literals),
            constant_term: 0,
            k_previous: 0,
            encoding_algorithm: PseudoBooleanEncoding::SingleInteger,
        }
    }

    pub fn from_weighted_literal_vector(
        weighted_literals: Vec<WeightedLiteral>,
        encoding_algorithm: PseudoBooleanEncoding,
    ) -> Self {
        PseudoBooleanConstraintEncoder::new(weighted_literals, encoding_algorithm)
    }

    pub fn from_literal_vector(
        literals: &[Literal],
        encoding_algorithm: PseudoBooleanEncoding,
    ) -> Self {
        PseudoBooleanConstraintEncoder::new(
            literals
                .iter()
                .map(|lit| WeightedLiteral {
                    literal: *lit,
                    weight: 1,
                    bound: None,
                })
                .collect(),
            encoding_algorithm,
        )
    }

    pub fn from_function(
        function: &Function,
        csp_solver: &mut ConstraintSatisfactionSolver,
        encoding_algorithm: PseudoBooleanEncoding,
    ) -> Self {
        let single_integer_case = function.get_weighted_literals().len() == 0
            && function.get_weighted_integers().len() == 1;
        let mut encoder = if single_integer_case {
            PseudoBooleanConstraintEncoder::from_single_integer_function(
                function.get_function_as_weighted_literals_vector(csp_solver),
            )
        } else {
            PseudoBooleanConstraintEncoder::new(
                function.get_function_as_weighted_literals_vector(csp_solver),
                encoding_algorithm,
            )
        };
        if !single_integer_case {
            encoder.constant_term = function.get_constant_term();
        }

        encoder
    }

    pub fn get_constant_term(&self) -> u64 {
        self.constant_term
    }

    pub fn constrain_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError> {
        pumpkin_assert_simple!(
            csp_solver
                .get_propositional_assignments()
                .is_at_the_root_level(),
            "Can only add encodings at the root level."
        );

        match self.state {
            State::New(ref mut weighted_literals) => {
                let literals = std::mem::take(weighted_literals);
                self.create_encoding(literals, k, csp_solver)?
            }
            State::Encoded(ref mut encoder) => {
                pumpkin_assert_simple!(self.k_previous > k, "The strenthened k value for the right hand side is not smaller than the previous k.");

                pumpkin_assert_simple!(
                        k >= self.constant_term,
                        "The k is below the trivial lower bound, probably an error? k={}, constant_term={}", k, self.constant_term
                    );

                encoder.strengthen_at_most_k(k - self.constant_term, csp_solver)?;
            }

            State::Preprocessed(ref mut literals) => {
                let sum_weight = literals.iter().map(|term| term.weight).sum::<u64>();
                let literals = std::mem::take(literals);

                if sum_weight > k - self.constant_term {
                    self.state = State::Encoded(Self::create_encoder(
                        literals,
                        k,
                        csp_solver,
                        self.encoding_algorithm,
                    )?);
                }
            }

            State::TriviallySatisfied => {}
            State::SingleInteger(ref mut encoder) => {
                pumpkin_assert_simple!(self.k_previous > k, "The strenthened k value for the right hand side is not smaller than the previous k.");

                pumpkin_assert_simple!(
                        k >= self.constant_term,
                        "The k is below the trivial lower bound, probably an error? k={}, constant_term={}", k, self.constant_term
                    );
                encoder.strengthen_at_most_k(k, csp_solver)?
            }
            State::SingleIntegerNew(ref mut weighted_literals) => {
                let literals = std::mem::take(weighted_literals);
                let encoder = SingleIntegerEncoder::encode_at_most_k(literals, k, csp_solver)?;
                self.state = State::SingleInteger(encoder);
                self.k_previous = k;
            }
        }

        Ok(())
    }

    fn create_encoding(
        &mut self,
        weighted_literals: Vec<WeightedLiteral>,
        initial_k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError> {
        let time_start = Instant::now();

        let preprocessed_weighted_literals =
            self.initialise_and_preprocess(weighted_literals, initial_k, csp_solver)?;

        let sum_weight = preprocessed_weighted_literals
            .iter()
            .map(|term| term.weight)
            .sum::<u64>();

        if preprocessed_weighted_literals.is_empty() {
            //All literals are assigned at the root level, it is thus trivially satisfied
            self.state = State::TriviallySatisfied;
        } else if sum_weight <= initial_k - self.constant_term {
            //The sum of the weights of the literals assigned at the root level is lower than the initial_k - constant_term
            //This means that this constraint is currently satisfied (but might be strengthened in the future)
            self.state = State::Preprocessed(preprocessed_weighted_literals);
        } else {
            //We need to constrain the preprocessed literals further to ensure that it encodes the initial value
            //We know that `constant_term` is already assigned, we thus need to constraint the remaining variables to `initial_k - constant_term`
            self.state = State::Encoded(Self::create_encoder(
                preprocessed_weighted_literals,
                initial_k - self.constant_term,
                csp_solver,
                self.encoding_algorithm,
            )?);
        }

        debug!(
            "Initial encoding took {} seconds.",
            time_start.elapsed().as_secs()
        );

        Ok(())
    }

    /// Initialises internal data structures
    /// Returns the preprocessed weighted literals    
    fn initialise_and_preprocess(
        &mut self,
        weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<Vec<WeightedLiteral>, EncodingError> {
        //preprocess the input before the initial encoding considering the following:
        //  1. Terms that are assigned at the root level are removed
        //      True literals decrease the right-hand side
        //      Falsified literals can be removed without modifying the right-hand side
        //  2. The constant term on the left-hand side effectively reduces the right-hand side
        //      in case the constant term is greater than the left-hand side, we have a trivial conflict
        //  3. Literals with weights exceeding the right-hand side are propagated to zero
        //  4. If setting every left-hand side literal to true still does not violate the constant, no encoding is needed

        //there are more rules we could consider adding to preprocessing in the future (todo):
        //  remove duplicate literals by merging into one literal
        //  remove literals of opposite polarity and change the constant term
        //  divide the weights by the GCD?

        //the preprocessing could be implemented more efficiency but probably is not the bottleneck

        self.k_previous = k;

        // Propagate literals x_i with too large coefficients
        //   i.e., w_i > k -> ~x_i
        // The propagation is done until a fixed point
        //  since propagating a literal x_i with a large coefficient
        //  may lead to the propagation of other literals

        // During this propagation, the constant term may exceed the bound,
        //  at which point the constraint is trivially unsatisfiable.

        let mut has_assigned = true;

        while has_assigned {
            has_assigned = false;

            for term in &weighted_literals {
                if term.weight > k - self.constant_term
                    && csp_solver
                        .get_propositional_assignments()
                        .is_literal_unassigned(term.literal)
                {
                    has_assigned = true;

                    let result = csp_solver.add_unit_clause(!term.literal);
                    if result.is_err() {
                        return Err(EncodingError::RootPropagationConflict);
                    }
                } else if csp_solver
                    .get_propositional_assignments()
                    .is_literal_assigned_true(term.literal)
                {
                    self.constant_term += term.weight;
                }
            }

            if self.constant_term > k {
                return Err(EncodingError::TriviallyUnsatisfiable);
            }
        }

        //collect terms that are not assigned at the root level
        let unassigned_weighted_literals: Vec<WeightedLiteral> = weighted_literals
            .iter()
            .filter(|term| {
                csp_solver
                    .get_propositional_assignments()
                    .is_literal_unassigned(term.literal)
            })
            .copied()
            .collect();

        Ok(unassigned_weighted_literals)
    }

    fn create_encoder(
        weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
        encoding_algorithm: PseudoBooleanEncoding,
    ) -> Result<Box<dyn PseudoBooleanConstraintEncoderInterface>, EncodingError> {
        match encoding_algorithm {
            PseudoBooleanEncoding::GTE => {
                let encoder = GeneralisedTotaliserEncoder::encode_at_most_k(
                    weighted_literals,
                    k,
                    csp_solver,
                )?;
                Ok(Box::new(encoder))
            }
            PseudoBooleanEncoding::CNE => {
                let encoder =
                    CardinalityNetworkEncoder::encode_at_most_k(weighted_literals, k, csp_solver)?;
                Ok(Box::new(encoder))
            }
            PseudoBooleanEncoding::SingleInteger => {
                unreachable!("The SingleInteger encoder is always created in a concrete manner")
            }
        }
    }
}

#[derive(Error, Debug, Copy, Clone)]
pub enum EncodingError {
    #[error("Constraint detected conflict at root level by propagation")]
    RootPropagationConflict,
    #[error("Strengthening caused conflict")]
    CannotStrengthen,
    #[error("Constraint is trivially unsatisfiable")]
    TriviallyUnsatisfiable,
}
