//! The module containing the individual proof steps.
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

#[derive(Debug, PartialEq, Eq)]
pub struct Inference<'label, Premises, Propagated> {
    pub label: &'label str,
    pub premises: Premises,
    pub propagated: Propagated,
}

impl<'label, Premises, Propagated> Inference<'label, Premises, Propagated> {
    pub fn new(label: &'label str, premises: Premises, propagated: Propagated) -> Self {
        Inference {
            label,
            premises,
            propagated,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Step<'a, Literals, Literal> {
    Inference(Inference<'a, Literals, Literal>),
    Nogood(Nogood<Literals>),
    Delete(Deletion),
    Conclusion(Conclusion<Literal>),
}
