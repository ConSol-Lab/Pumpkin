//! This crate handles the reading and writing of proofs produced by
//! [Pumpkin](https://github.com/consol-lab/pumpkin).
//!
//! ## Reading Proofs
//! Proofs can be read using a [`ProofReader`]. It reads the proof line-by-line, and can be thought
//! of as an Iterator over [`ProofStep`].
//!
//! ```
//! use std::num::NonZero;
//!
//! use drcp_format::Constraint;
//! use drcp_format::Inference;
//! use drcp_format::IntAtomic;
//! use drcp_format::IntComparison::*;
//! use drcp_format::ProofReader;
//! use drcp_format::Step;
//!
//! let source = r#"
//! a 1 [x1 >= 0]
//! a 2 [x2 >= 0]
//! i 1 1 0 2 c:1 l:inf_name
//! "#;
//!
//! let mut reader = ProofReader::new(source.as_bytes());
//!
//! let inference = reader.next_step::<i32>()
//!     .expect("no error reading")
//!     .expect("there is one proof step");
//!
//! let a1 = IntAtomic { name: "x1", comparison: GreaterEqual, value: 0 };
//! let a2 = IntAtomic { name: "x2", comparison: GreaterEqual, value: 0 };
//!
//! let expected_inference = Inference {
//!     id: NonZero::new(1).unwrap(),
//!     premises: &[a1] as &[IntAtomic<&str, i32>],
//!     consequent: Some(a2),
//!     generated_by: Some(Constraint::Axiom(NonZero::new(1).unwrap())),
//!     label: Some("inf_name"),
//! };
//! assert_eq!(Step::Inference(expected_inference), inference);
//! ```

use std::{fmt::Display, io::Read, num::NonZero};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntComparison {
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl Display for IntComparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntComparison::GreaterEqual => ">=",
            IntComparison::LessEqual => "<=",
            IntComparison::Equal => "==",
            IntComparison::NotEqual => "!=",
        };

        write!(f, "{s}")
    }
}

/// An integer atomic constraint of the form `[name op value]`, where `op` is an [`IntComparison`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntAtomic<Identifier, Int> {
    pub name: Identifier,
    pub comparison: IntComparison,
    pub value: Int,
}

impl<Identifier: Display, Int: Display> Display for IntAtomic<Identifier, Int> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IntAtomic {
            name,
            comparison,
            value,
        } = self;

        write!(f, "[{name} {comparison} {value}]")
    }
}

/// The ID of a constraint in the original model.
pub type AxiomId = NonZero<u32>;

/// The ID of a proof step.
pub type StepId = NonZero<u32>;

/// A reference to an object that can generate inference steps in the proof.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Constraint {
    /// An axiom is a constraint from the model.
    Axiom(AxiomId),
    /// A nogood is treated as a derived constraint.
    Nogood(StepId),
}

/// An inference step.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Inference<Premises, Consequent, Label> {
    /// The ID of the step.
    pub id: StepId,
    /// The premises of the proof.
    pub premises: Premises,
    /// The consequent of the proof. If this is [`None`], the `premises` explicitly describe a
    /// conflicting assignment.
    pub consequent: Option<Consequent>,
    /// The constraint that introduced this inference.
    pub generated_by: Option<Constraint>,
    /// The label that identifies the reasoning that introduced this inference.
    pub label: Option<Label>,
}

/// An individual proof step from the proof.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Step<Atomics, Atomic, Label> {
    Inference(Inference<Atomics, Atomic, Label>),
}

pub struct ProofReader<Source> {
    source: Source,
}

impl<Source> ProofReader<Source> {
    pub fn new(source: Source) -> Self {
        ProofReader { source }
    }
}

pub type ReadAtomic<'a, Int> = IntAtomic<&'a str, Int>;
pub type ReadStep<'a, Int> = Step<&'a [ReadAtomic<'a, Int>], ReadAtomic<'a, Int>, &'a str>;

#[derive(Debug, thiserror::Error)]
#[error("failed to read proof step")]
pub struct Error;

impl<Source: Read> ProofReader<Source> {
    pub fn next_step<Int>(&mut self) -> Result<Option<ReadStep<'_, Int>>, Error> {
        todo!()
    }
}
