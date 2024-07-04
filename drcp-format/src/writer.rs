use std::io::BufWriter;
use std::io::Write;
use std::num::NonZeroI32;
use std::num::NonZeroU64;

use crate::encountered_literals::LiteralCodeProvider;
use crate::format::Format;
use crate::steps::Conclusion;
use crate::steps::Deletion;
use crate::steps::Inference;
use crate::steps::Nogood;
use crate::steps::NogoodId;

/// Abstraction for writing DRCP proofs.
///
/// # Example
/// ```
/// # use std::num::NonZeroI32;
/// # use drcp_format::Format;
/// # use drcp_format::ProofWriter;
/// let mut proof: Vec<u8> = Vec::new();
/// let mut writer = ProofWriter::new(Format::Text, &mut proof, std::convert::identity);
///
/// let lit = |num: i32| NonZeroI32::new(num).unwrap();
/// writer
///     .log_inference("linear_bound", [lit(4), lit(5)], lit(-2))
///     .unwrap();
/// let nogood_id = writer.log_nogood_clause([lit(1), lit(-3), lit(5)]).unwrap();
/// writer.log_deletion(nogood_id).unwrap();
/// writer.unsat().unwrap();
///
/// let expected = "
/// i linear_bound 4 5 0 -2
/// n 1 1 -3 5
/// d 1
/// c UNSAT
/// ";
/// assert_eq!(proof, expected.trim_start().as_bytes());
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
    /// The id for the next nogood which is logged.
    next_nogood_id: NogoodId,
}

impl<W: Write, Literals> ProofWriter<W, Literals> {
    /// Create a new proof writer which writes the proof to an underlying sink implementing
    /// [`Write`].
    pub fn new(format: Format, writer: W, encountered_literals: Literals) -> Self {
        Self {
            format,
            writer: BufWriter::new(writer),
            encountered_literals,
            next_nogood_id: NonZeroU64::new(1).unwrap(),
        }
    }
}

impl<W, Literals> ProofWriter<W, Literals>
where
    W: Write,
    Literals: LiteralCodeProvider,
{
    /// Write a nogood step. A new nogood ID will be generated, which can be given to
    /// [`Self::log_deletion`] to indicate the nogood is deleted.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    ///
    /// # Note
    /// The nogood *must* be given in its clausal form, i.e. a disjunction of literals, not as a
    /// conjunction of literals.
    pub fn log_nogood_clause(
        &mut self,
        nogood: impl IntoIterator<Item = Literals::Literal>,
    ) -> std::io::Result<NogoodId> {
        let id = self.next_nogood_id;
        self.next_nogood_id = self.next_nogood_id.checked_add(1).unwrap();

        let nogood = Nogood::new(
            id,
            nogood
                .into_iter()
                .map(|pred| self.encountered_literals.to_code(pred)),
        );

        nogood.write(self.format, &mut self.writer)?;

        Ok(id)
    }

    /// Log that the nogood with the given ID can be deleted.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn log_deletion(&mut self, nogood_id: NogoodId) -> std::io::Result<()> {
        Deletion::new(nogood_id).write(self.format, &mut self.writer)
    }

    /// Log an inference step.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn log_inference(
        &mut self,
        label: &str,
        premises: impl IntoIterator<Item = Literals::Literal>,
        propagated: Literals::Literal,
    ) -> std::io::Result<()> {
        let propagated = self.encountered_literals.to_code(propagated);

        let inference = Inference::new(
            label,
            premises
                .into_iter()
                .map(|pred| self.encountered_literals.to_code(pred)),
            propagated,
        );

        inference.write(self.format, &mut self.writer)
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

impl<Literals> WritableProofStep for Nogood<Literals>
where
    Literals: IntoIterator<Item = NonZeroI32>,
{
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()> {
        write!(sink, "n {}", self.id)?;

        for literal in self.literals {
            write!(sink, " {literal}")?;
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

impl<'label, Premises> WritableProofStep for Inference<'label, Premises, NonZeroI32>
where
    Premises: IntoIterator<Item = NonZeroI32>,
{
    fn write_string(self, sink: &mut impl Write) -> std::io::Result<()> {
        write!(sink, "i {}", self.label)?;

        for literal in self.premises {
            write!(sink, " {literal}")?;
        }

        writeln!(sink, " 0 {}", self.propagated)?;

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
