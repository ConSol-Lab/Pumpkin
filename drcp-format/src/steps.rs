//! The module containing the individual proof steps.
use std::num::NonZero;
use std::num::NonZeroU64;

pub type StepId = NonZeroU64;

#[derive(Debug, PartialEq, Eq)]
pub struct Nogood<Literals, Hints> {
    pub id: StepId,
    pub literals: Literals,
    pub hints: Option<Hints>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Deletion {
    pub id: StepId,
}

impl Deletion {
    pub fn new(id: NonZeroU64) -> Self {
        Self { id }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Conclusion<Literal> {
    Unsatisfiable,
    Optimal(Literal),
}

/// One inference step in the proof.
#[derive(Debug, PartialEq, Eq)]
pub struct Inference<'label, Premises, Propagated> {
    /// The ID of this step.
    pub id: StepId,
    /// The hint to the constraint which implies the inference.
    pub hint_constraint_id: Option<NonZero<u32>>,
    /// The hint to the filtering algorithm which identifies the inference.
    pub hint_label: Option<&'label str>,
    /// The premises of the inference.
    pub premises: Premises,
    /// The conclusion of the inference.
    ///
    /// If absent, the inference implies false.
    pub propagated: Option<Propagated>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Step<'a, Literals, Literal, Hints> {
    Inference(Inference<'a, Literals, Literal>),
    Nogood(Nogood<Literals, Hints>),
    Delete(Deletion),
    Conclusion(Conclusion<Literal>),
}
