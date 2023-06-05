/// The following facilitates easier reuse and consistency amongst pseudo-Boolean encoders
/// The idea is to separate the 'preprocessing' of the input and encoding algorithm
///     this way all encoders can benefit from the same preprocessing
///     and the encoding algorithm can then consider only canonical cases for its input
///
/// The trait 'PseudoBooleanConstraintEncoderInterface' provides the interface
///     encoders are expected to implement this trait
/// PseudoBooleanConstraintEncoder acts as a wrapper around the interface structs
use std::time::Instant;
use thiserror::Error;

use log::debug;

use crate::{
    basic_types::{ClauseAdditionOutcome, Function, Literal, WeightedLiteral},
    engine::ConstraintSatisfactionSolver,
    pumpkin_assert_simple,
};

use super::{CardinalityNetworkEncoder, GeneralisedTotaliserEncoder};

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
}

/// main struct through which encoders are to be used
pub struct PseudoBooleanConstraintEncoder {
    original_weighted_literals: Vec<WeightedLiteral>,
    original_constant_term: u64,
    encoder: Option<Box<dyn PseudoBooleanConstraintEncoderInterface>>,
    has_initial_encoding: bool,
    constant_term: u64,
    k_previous: u64,
    encoding_algorithm: PseudoBooleanEncoding,
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
            original_weighted_literals: weighted_literals,
            original_constant_term: 0,
            encoder: None,
            has_initial_encoding: false,
            constant_term: 0,
            k_previous: 0,
            encoding_algorithm,
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
        let mut encoder = PseudoBooleanConstraintEncoder::new(
            function.get_function_as_weighted_literals_vector(csp_solver),
            encoding_algorithm,
        );
        encoder.constant_term = function.get_constant_term();
        encoder.original_constant_term = encoder.constant_term;
        encoder
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

        //if the encoding has already been encoded, then we strenthen the constraint incrementally
        if self.has_initial_encoding {
            pumpkin_assert_simple!(self.are_strenthening_conditions_met(k));

            self.encoder
                .as_mut()
                .unwrap()
                .strengthen_at_most_k(k - self.constant_term, csp_solver)
        } else {
            let time_start = Instant::now();

            let preprocessed_weighted_literals = self.initialise_and_preprocess(k, csp_solver)?;

            if !preprocessed_weighted_literals.is_empty() {
                self.encoder = Some(Self::create_encoder(
                    preprocessed_weighted_literals,
                    k,
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
    }

    fn are_strenthening_conditions_met(&self, k: u64) -> bool {
        if self.k_previous <= k {
            debug!("The strenthened k value for the right hand side is not smaller than the previous k.");
            return false;
        }

        if k < self.constant_term {
            debug!("The k is below the trivial lower bound, probably an error?");
            return false;
        }

        if self.encoder.is_none() {
            debug!("Strenthening to be applied but no encoding?");
            return false;
        }

        true
    }

    /// Initialises internal data structures
    /// Returns the preprocessed weighted literals    
    fn initialise_and_preprocess(
        &mut self,
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
        self.has_initial_encoding = true;

        //  compute the left hand side value caused by literals assigned to true at the root level
        //      note: during initialisation, the self.constant_term may be set to greater than zero so we need to add to it
        self.constant_term += self
            .original_weighted_literals
            .iter()
            .filter_map(|p| {
                if csp_solver
                    .get_propositional_assignments()
                    .is_literal_assigned_true(p.literal)
                {
                    Some(p.weight)
                } else {
                    None
                }
            })
            .sum::<u64>();

        //can terminate if the left-hand side already exceeds k
        if self.constant_term > k {
            return Err(EncodingError::TriviallyUnsatisfiable);
        }

        //propagate literals with too large coefficients
        //  i.e., w_i > k -> ~x_i
        for term in &self.original_weighted_literals {
            if term.weight > k - self.constant_term
                && csp_solver
                    .get_propositional_assignments()
                    .is_literal_unassigned(term.literal)
            {
                let status = csp_solver.add_unit_clause(!term.literal);

                //should handle by changing 'add_unit_clause' to return a result
                if let ClauseAdditionOutcome::Infeasible = status {
                    return Err(EncodingError::RootPropagationConflict);
                }
            }
        }

        //collect terms that are not assigned at the root level
        let unassigned_weighted_literals: Vec<WeightedLiteral> = self
            .original_weighted_literals
            .iter()
            .filter(|term| {
                csp_solver
                    .get_propositional_assignments()
                    .is_literal_unassigned(term.literal)
            })
            .copied()
            .collect();

        if unassigned_weighted_literals
            .iter()
            .map(|term| term.weight)
            .sum::<u64>()
            <= k - self.constant_term
        {
            //trivially satisfied, so return the empty vector of literals
            return Ok(vec![]);
        }
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
        }
    }
}

#[derive(Error, Debug)]
pub enum EncodingError {
    #[error("Constraint detected conflict at root level by propagation")]
    RootPropagationConflict,
    #[error("Strenthening caused conflict")]
    CannotStrenthen,
    #[error("Constraint is trivially unsatisfiable")]
    TriviallyUnsatisfiable,
}
