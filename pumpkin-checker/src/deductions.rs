use std::collections::BTreeMap;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;

use crate::inferences::Fact;
use crate::model::Nogood;

/// An inference that was ignored when checking a deduction.
#[derive(Clone, Debug)]
pub struct IgnoredInference {
    /// The ID of the ignored inference.
    pub constraint_id: ConstraintId,

    /// The premises that were not satisfied when the inference was evaluated.
    pub unsatisfied_premises: Vec<IntAtomic<String, i32>>,
}

/// A deduction is rejected by the checker.
#[derive(thiserror::Error, Debug)]
#[error("invalid deduction")]
pub enum InvalidDeduction {
    /// The constraint ID of the deduction is already used by an existing constraint.
    #[error("constraint id {0} already in use")]
    DuplicateConstraintId(ConstraintId),

    /// An inference in the deduction sequence does not exist in the proof stage.
    #[error("inference {0} does not exist")]
    UnknownInference(ConstraintId),

    /// The inferences in the proof stage do not derive an empty domain or an explicit
    /// conflict.
    #[error("no conflict was derived after applying all inferences")]
    NoConflict(Vec<IgnoredInference>),

    /// The premise contains mutually exclusive atomic constraints.
    #[error("the deduction contains inconsistent premises")]
    InconsistentPremises,
}

/// Verify that a deduction is valid given the inferences in the proof stage.
pub fn verify_deduction(
    _deduction: &drcp_format::Deduction<Rc<str>, i32>,
    _facts_in_proof_stage: &BTreeMap<ConstraintId, Fact>,
) -> Result<Nogood, InvalidDeduction> {
    todo!()
}
