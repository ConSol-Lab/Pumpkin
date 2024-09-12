//! The module containing the individual proof steps.
use std::num::NonZero;
use std::num::NonZeroU64;

pub type NogoodId = NonZeroU64;

#[derive(Debug, PartialEq, Eq)]
pub struct Nogood<Literals> {
    pub id: NogoodId,
    pub literals: Literals,
}

impl<Literals> Nogood<Literals> {
    pub fn new(id: NogoodId, literals: Literals) -> Self {
        Nogood { id, literals }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Deletion {
    pub id: NogoodId,
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
    /// The hint to the constraint which implies the inference.
    pub hint_constraint_id: Option<NonZero<u32>>,
    /// The hint to the filtering algorithm which identifies the inference.
    pub hint_label: Option<&'label str>,
    /// The premises of the inference.
    pub premises: Premises,
    /// The conclusion of the inference.
    pub propagated: Propagated,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Step<'a, Literals, Literal> {
    Inference(Inference<'a, Literals, Literal>),
    Nogood(Nogood<Literals>),
    Delete(Deletion),
    Conclusion(Conclusion<Literal>),
}
