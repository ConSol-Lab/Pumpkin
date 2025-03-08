//! Implements the writing of DRCP files.
//!
//! See [`ProofWriter`] for more information on how to write proofs.

mod literal_code_provider;

use std::io::BufWriter;
use std::io::Write;
use std::num::NonZero;
use std::num::NonZeroI32;
use std::num::NonZeroU64;

pub use literal_code_provider::*;

use crate::format::Format;
use crate::steps::Conclusion;
use crate::steps::Deletion;
use crate::steps::Inference;
use crate::steps::Nogood;
use crate::steps::StepId;

/// Abstraction for writing DRCP proofs.
///
/// # Example
/// ```
/// # use std::num::NonZeroI32;
/// # use drcp_format::Format;
/// # use drcp_format::writer::ProofWriter;
/// # use drcp_format::steps::StepId;
/// let mut proof: Vec<u8> = Vec::new();
/// let mut writer = ProofWriter::new(Format::Text, &mut proof, std::convert::identity);
///
/// let lit = |num: i32| NonZeroI32::new(num).unwrap();
/// writer
///     .log_inference(None, Some("linear_bound"), [lit(4), lit(5)], Some(lit(-2)))
///     .unwrap();
/// let nogood_id = writer
///     .log_nogood_clause([lit(1), lit(-3), lit(5)], None::<[StepId; 0]>)
///     .unwrap();
/// writer.log_deletion(nogood_id).unwrap();
/// writer.unsat().unwrap();
///
/// let expected = "
/// i 1 4 5 0 -2 l:linear_bound
/// n 2 1 -3 5
/// d 2
/// c UNSAT
/// ";
/// assert_eq!(std::str::from_utf8(&proof).unwrap(), expected.trim_start());
/// ```
#[derive(Debug)]
pub struct ProofWriter<W: Write, Literals> {
    /// The writer to the underlying sink.
    writer: BufWriter<W>,
    /// The format in which to log the proof.
    format: Format,
    /// A container for all the literals which are seen in the proof. Unseen literals need not be
    /// defined in the literal definition file.
    encountered_literals: Literals,
    /// The id for the next step which is logged.
    next_step_id: StepId,
}

impl<W: Write, Literals> ProofWriter<W, Literals> {
    /// Create a new proof writer which writes the proof to an underlying sink implementing
    /// [`Write`].
    pub fn new(format: Format, writer: W, encountered_literals: Literals) -> Self {
        Self {
            format,
            writer: BufWriter::new(writer),
            encountered_literals,
            next_step_id: NonZeroU64::new(1).unwrap(),
        }
    }
}

impl<W: Write, Literals> ProofWriter<W, Literals> {
    /// Get the encountered literals instance to be mutated.
    pub fn literals_mut(&mut self) -> &mut Literals {
        &mut self.encountered_literals
    }
}

impl<W, Literals> ProofWriter<W, Literals>
where
    W: Write,
    Literals: LiteralCodeProvider,
{
    /// Write a nogood step.
    ///
    /// This step can be referenced by the [`StepId`] that is returned. Such a reference may occur
    /// when the nogood is deleted, or used in the derivation of another nogood as a hint.
    ///
    /// The `propagation_hints` can be optionally given to indicate the order in which the given
    /// steps can be applied to derive the conflict under reverse propagation.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    ///
    /// # Note
    /// The nogood *must* be given in its clausal form, i.e. a disjunction of literals, not as a
    /// conjunction of literals.
    pub fn log_nogood_clause(
        &mut self,
        nogood: impl IntoIterator<Item = Literals::Literal>,
        propagation_hints: Option<impl IntoIterator<Item = StepId>>,
    ) -> std::io::Result<StepId> {
        let id = self.next_step_id();

        let nogood = Nogood {
            id,
            literals: nogood
                .into_iter()
                .map(|pred| self.encountered_literals.to_code(pred)),
            hints: propagation_hints,
        };

        nogood.write(self.format, &mut self.writer)?;

        Ok(id)
    }

    fn next_step_id(&mut self) -> NonZero<u64> {
        let id = self.next_step_id;
        self.next_step_id = self.next_step_id.checked_add(1).unwrap();
        id
    }

    /// Log that the nogood with the given ID can be deleted.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn log_deletion(&mut self, nogood_id: StepId) -> std::io::Result<()> {
        Deletion::new(nogood_id).write(self.format, &mut self.writer)
    }

    /// Log an inference step.
    ///
    /// Besides premises and a conclusion, an inference step can optionally include hints regarding
    /// the constraint that implied the inference, and the label of the filtering algorithm which
    /// identified the inference.
    ///
    /// If there is no conclusion, then the format prescribes that the conclusion is false, so that
    /// the premises form a nogood.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn log_inference(
        &mut self,
        hint_constraint_id: Option<NonZero<u32>>,
        hint_label: Option<&str>,
        premises: impl IntoIterator<Item = Literals::Literal>,
        propagated: Option<Literals::Literal>,
    ) -> std::io::Result<StepId> {
        let propagated = propagated.map(|p| self.encountered_literals.to_code(p));
        let id = self.next_step_id();

        let inference = Inference {
            id,
            hint_constraint_id,
            hint_label,
            premises: premises
                .into_iter()
                .map(|pred| self.encountered_literals.to_code(pred)),
            propagated,
        };

        inference.write(self.format, &mut self.writer)?;

        Ok(id)
    }

    /// Conclude with the unsatisfiable claim.
    ///
    /// Since the conclusion is the very last step in the proof, this method takes ownership of
    /// [`Self`], to ensure no more steps can be written after this one.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn unsat(self) -> std::io::Result<Literals> {
        self.conclude(Conclusion::Unsatisfiable)
    }

    /// Conclude the proof with the optimality claim, and log the bound of the objective variable.
    ///
    /// Since the conclusion is the very last step in the proof, this method takes ownership of
    /// [`Self`], to ensure no more steps can be written after this one.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn optimal(mut self, objective_bound: Literals::Literal) -> std::io::Result<Literals> {
        let code = self.encountered_literals.to_code(objective_bound);
        self.conclude(Conclusion::Optimal(code))
    }

    fn conclude(mut self, conclusion: Conclusion<NonZeroI32>) -> std::io::Result<Literals> {
        conclusion.write(self.format, &mut self.writer)?;
        Ok(self.encountered_literals)
    }
}

trait WritableProofStep: Sized {
    /// Write the proof step in the string form.
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()>;

    /// Write the proof step as binary.
    fn write_binary(self, sink: &mut impl Write) -> std::io::Result<()>;

    /// Write the step in the given format to the given sink.
    fn write(self, format: Format, sink: &mut impl Write) -> std::io::Result<()> {
        match format {
            Format::Text => self.write_string(sink),
            Format::Binary => self.write_binary(sink),
        }
    }
}

impl<Literals, Hints> WritableProofStep for Nogood<Literals, Hints>
where
    Literals: IntoIterator<Item = NonZeroI32>,
    Hints: IntoIterator<Item = StepId>,
{
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()> {
        write!(sink, "n {}", self.id)?;

        for literal in self.literals {
            write!(sink, " {literal}")?;
        }

        if let Some(hints) = self.hints {
            write!(sink, " 0")?;

            for hint in hints {
                write!(sink, " {hint}")?;
            }
        }

        writeln!(sink)?;

        Ok(())
    }

    fn write_binary(self, _sink: &mut impl Write) -> std::io::Result<()> {
        todo!()
    }
}

impl WritableProofStep for Conclusion<NonZeroI32> {
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()> {
        match self {
            Conclusion::Unsatisfiable => writeln!(sink, "c UNSAT"),
            Conclusion::Optimal(literal) => writeln!(sink, "c {literal}"),
        }
    }

    fn write_binary(self, _sink: &mut impl Write) -> std::io::Result<()> {
        todo!()
    }
}

impl<Premises> WritableProofStep for Inference<'_, Premises, NonZeroI32>
where
    Premises: IntoIterator<Item = NonZeroI32>,
{
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()> {
        write!(sink, "i {}", self.id)?;

        for literal in self.premises {
            write!(sink, " {literal}")?;
        }

        if let Some(propagated) = self.propagated {
            write!(sink, " 0 {}", propagated)?;
        }

        if let Some(constraint_id) = self.hint_constraint_id {
            write!(sink, " c:{constraint_id}")?;
        }

        if let Some(label) = self.hint_label {
            write!(sink, " l:{label}")?;
        }

        writeln!(sink)?;

        Ok(())
    }

    fn write_binary(self, _sink: &mut impl Write) -> std::io::Result<()> {
        todo!()
    }
}

impl WritableProofStep for Deletion {
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()> {
        writeln!(sink, "d {}", self.id)
    }

    fn write_binary(self, _sink: &mut impl Write) -> std::io::Result<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_ID: NonZeroU64 = NonZeroU64::new(1).unwrap();

    #[test]
    fn write_basic_inference() {
        test_step_serialization(
            Inference {
                id: TEST_ID,
                hint_constraint_id: None,
                hint_label: None,
                premises: [lit(2), lit(-3)],
                propagated: Some(lit(1)),
            },
            "i 1 2 -3 0 1\n",
        );
    }

    #[test]
    fn write_inference_with_label() {
        test_step_serialization(
            Inference {
                id: TEST_ID,
                hint_constraint_id: None,
                hint_label: Some("inf_label"),
                premises: [lit(2), lit(-3)],
                propagated: Some(lit(1)),
            },
            "i 1 2 -3 0 1 l:inf_label\n",
        );
    }

    #[test]
    fn write_inference_with_constraint_id() {
        test_step_serialization(
            Inference {
                id: TEST_ID,
                hint_constraint_id: Some(NonZero::new(1).unwrap()),
                hint_label: None,
                premises: [lit(2), lit(-3)],
                propagated: Some(lit(1)),
            },
            "i 1 2 -3 0 1 c:1\n",
        );
    }

    #[test]
    fn write_inference_with_constraint_id_and_label() {
        test_step_serialization(
            Inference {
                id: TEST_ID,
                hint_constraint_id: Some(NonZero::new(1).unwrap()),
                hint_label: Some("inf_label"),
                premises: [lit(2), lit(-3)],
                propagated: Some(lit(1)),
            },
            "i 1 2 -3 0 1 c:1 l:inf_label\n",
        );
    }

    #[test]
    fn write_inference_without_conclusion_with_hints() {
        test_step_serialization(
            Inference {
                id: TEST_ID,
                hint_constraint_id: Some(NonZero::new(1).unwrap()),
                hint_label: Some("inf_label"),
                premises: [lit(2), lit(-3)],
                propagated: None,
            },
            "i 1 2 -3 c:1 l:inf_label\n",
        );
    }

    #[test]
    fn write_inference_without_conclusion_without_hints() {
        test_step_serialization(
            Inference {
                id: TEST_ID,
                hint_constraint_id: None,
                hint_label: None,
                premises: [lit(2), lit(-3)],
                propagated: None,
            },
            "i 1 2 -3\n",
        );
    }

    fn lit(num: i32) -> NonZero<i32> {
        NonZero::new(num).unwrap()
    }

    fn test_step_serialization(step: impl WritableProofStep, expected: &str) {
        let mut buffer = Vec::new();
        step.write_string(&mut buffer).expect("no error writing");

        let actual = String::from_utf8(buffer).expect("valid utf8");
        assert_eq!(expected, actual);
    }
}
