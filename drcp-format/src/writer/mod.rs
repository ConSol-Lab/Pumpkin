//! Implements the writing of DRCP files.
//!
//! # Example
//! ```
//! use std::num::NonZero;
//!
//! use drcp_format::Conclusion;
//! use drcp_format::Deduction;
//! use drcp_format::Inference;
//! use drcp_format::IntAtomic;
//! use drcp_format::IntComparison::*;
//! use drcp_format::writer::ProofWriter;
//!
//! let mut buffer = Vec::new();
//! let mut writer = ProofWriter::new(&mut buffer);
//!
//! writer
//!     .log_inference(Inference {
//!         constraint_id: NonZero::new(2).unwrap(),
//!         premises: vec![
//!             IntAtomic::new("x1", GreaterEqual, 1),
//!             IntAtomic::new("x2", LessEqual, 3),
//!         ],
//!         consequent: Some(IntAtomic::new("x3", Equal, -3)),
//!         generated_by: Some(NonZero::new(1).unwrap()),
//!         label: Some("inf_name"),
//!     })
//!     .unwrap();
//!
//! writer
//!     .log_deduction(Deduction {
//!         constraint_id: NonZero::new(3).unwrap(),
//!         premises: vec![
//!             IntAtomic::new("x1", GreaterEqual, 1),
//!             IntAtomic::new("x2", GreaterEqual, 4),
//!         ],
//!         sequence: vec![NonZero::new(2).unwrap()],
//!     })
//!     .unwrap();
//!
//! writer.log_conclusion::<&str>(Conclusion::Unsat).unwrap();
//!
//! let proof_string = String::from_utf8(buffer).expect("valid utf8");
//! let expected = r#"
//! a 1 [x1 >= 1]
//! a 2 [x2 <= 3]
//! a 3 [x3 == -3]
//! i 2 1 2 0 3 c:1 l:inf_name
//! n 3 1 -2 0 2
//! c UNSAT
//! "#;
//!
//! assert_eq!(expected.trim(), proof_string.trim());
//! ```

pub mod blocks;

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;
use std::num::NonZero;

use crate::Conclusion;
use crate::Deduction;
use crate::Inference;
use crate::IntAtomic;
use crate::IntComparison;
use crate::IntValue;

/// Write a DRCP proof to some sink.
///
/// The codes of the atomic constraints over one variable, by their comparison and value.
type AtomicCodes<Int> = BTreeMap<(IntComparison, Int), NonZero<i32>>;

/// See the module documentation of [`crate::writer`] for examples on how to use the
/// [`ProofWriter`].
#[derive(Debug)]
pub struct ProofWriter<W, Int> {
    /// The writer to the underlying sink.
    writer: W,

    /// A container for all the literals which are seen in the proof, by the name of their
    /// variable and then by their comparison and value, so that an atomic constraint that is
    /// defined already is found from the borrowed name without allocating a copy of it. Unseen
    /// literals need not be defined in the literal definition file.
    defined_atomics: HashMap<Box<str>, AtomicCodes<Int>>,

    /// The next ID to use when a new atomic needs to be defined.
    next_atomic_id: i32,
}

impl<W: Write, Int> ProofWriter<W, Int> {
    /// Create a new proof writer which writes the proof to an underlying sink implementing
    /// [`Write`].
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            defined_atomics: HashMap::default(),
            next_atomic_id: 0,
        }
    }

    /// The underlying sink.
    pub fn get_mut(&mut self) -> &mut W {
        &mut self.writer
    }
}

impl<W: Write, Int: IntValue> ProofWriter<W, Int> {
    /// Write a deduction step.
    ///
    /// The `propagation_hints` can be optionally given to indicate which previously derived facts
    /// should be applied to derive the conflict.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn log_deduction<Identifier: AsRef<str> + Into<Box<str>>>(
        &mut self,
        deduction: Deduction<Identifier, Int>,
    ) -> std::io::Result<()> {
        let Deduction {
            constraint_id,
            premises,
            sequence,
        } = deduction;

        let premises = premises
            .into_iter()
            .map(|premise| self.get_atomic_code(premise))
            .collect::<Result<Vec<_>, _>>()?;

        write!(&mut self.writer, "n {constraint_id}")?;

        for code in premises {
            write!(&mut self.writer, " {code}")?;
        }

        write!(&mut self.writer, " 0")?;

        for constraint_id in sequence {
            write!(&mut self.writer, " {constraint_id}")?;
        }

        writeln!(&mut self.writer)?;

        Ok(())
    }

    /// Log an inference step.
    ///
    /// Besides premises and a consequent, an inference step can optionally include hints regarding
    /// the constraint that generated the inference, and the label of the filtering algorithm which
    /// identified the inference.
    ///
    /// If there is no consequent, then the format prescribes that the consequent is false, so that
    /// the premises form a nogood.
    ///
    /// This function wraps an IO operation, which is why it can fail with an IO error.
    pub fn log_inference<Identifier: AsRef<str> + Into<Box<str>>, Label: Display>(
        &mut self,
        inference: Inference<Identifier, Int, Label>,
    ) -> std::io::Result<()> {
        let Inference {
            constraint_id,
            premises,
            consequent,
            generated_by,
            label,
        } = inference;

        let premises = premises
            .into_iter()
            .map(|premise| self.get_atomic_code(premise))
            .collect::<Result<Vec<_>, _>>()?;

        let consequent = consequent
            .map(|premise| self.get_atomic_code(premise))
            .transpose()?;

        write!(&mut self.writer, "i {constraint_id}")?;

        for code in premises {
            write!(&mut self.writer, " {code}")?;
        }

        write!(&mut self.writer, " 0")?;

        if let Some(code) = consequent {
            write!(&mut self.writer, " {code}")?;
        }

        if let Some(generated_by) = generated_by {
            write!(&mut self.writer, " c:{generated_by}")?;
        }

        if let Some(label) = label {
            write!(&mut self.writer, " l:{label}")?;
        }

        writeln!(&mut self.writer)?;

        Ok(())
    }

    /// Write a deduction step whose premises are given by their codes, as
    /// [`ProofWriter::atomic_code`] returned them; the step is written as
    /// [`ProofWriter::log_deduction`] writes it.
    pub fn log_deduction_by_codes(
        &mut self,
        constraint_id: crate::ConstraintId,
        premises: &[NonZero<i32>],
        sequence: impl IntoIterator<Item = crate::ConstraintId>,
    ) -> std::io::Result<()> {
        write!(&mut self.writer, "n {constraint_id}")?;
        for code in premises {
            write!(&mut self.writer, " {code}")?;
        }
        write!(&mut self.writer, " 0")?;
        for constraint_id in sequence {
            write!(&mut self.writer, " {constraint_id}")?;
        }
        writeln!(&mut self.writer)
    }

    /// The code of the atomic constraint in the proof, defining it first when it is new; the
    /// code of its negation is the negated code.
    pub fn atomic_code<Identifier: AsRef<str> + Into<Box<str>>>(
        &mut self,
        atomic: IntAtomic<Identifier, Int>,
    ) -> std::io::Result<NonZero<i32>> {
        self.get_atomic_code(atomic)
    }

    /// Conclude the proof with a conclusion.
    pub fn log_conclusion<Identifier: AsRef<str> + Into<Box<str>>>(
        &mut self,
        conclusion: Conclusion<Identifier, Int>,
    ) -> std::io::Result<()> {
        match conclusion {
            Conclusion::Unsat => writeln!(&mut self.writer, "c UNSAT"),
            Conclusion::DualBound(atomic) => {
                let code = self.get_atomic_code(atomic)?;
                writeln!(&mut self.writer, "c {code}")
            }
        }
    }

    fn get_atomic_code<Identifier: AsRef<str> + Into<Box<str>>>(
        &mut self,
        atomic: IntAtomic<Identifier, Int>,
    ) -> std::io::Result<NonZero<i32>> {
        if let Some(&code) = self
            .defined_atomics
            .get(atomic.name.as_ref())
            .and_then(|codes| codes.get(&(atomic.comparison, atomic.value.clone())))
        {
            return Ok(code);
        }

        self.next_atomic_id += 1;
        let id = NonZero::new(self.next_atomic_id)
            .expect("next_atomic_id starts at 0, and is incremented, so it can never be 0");

        let key = IntAtomic {
            name: atomic.name.into(),
            comparison: atomic.comparison,
            value: atomic.value,
        };
        let negation = !key.clone();
        let codes = self.defined_atomics.entry(key.name.clone()).or_default();
        let _ = codes.insert((key.comparison, key.value.clone()), id);
        let _ = codes.insert((negation.comparison, negation.value), -id);

        writeln!(&mut self.writer, "a {id} {key}")?;

        Ok(id)
    }
}

#[cfg(test)]
mod tests {
    use IntComparison::*;

    use super::*;
    use crate::ConstraintId;
    use crate::IntComparison;
    use crate::Step;

    const TEST_ID: ConstraintId = NonZero::new(5).unwrap();

    #[test]
    fn write_basic_inference() {
        test_step_serialization(
            Step::Inference(Inference {
                constraint_id: TEST_ID,
                premises: vec![atomic("x1", GreaterEqual, 1), atomic("x2", LessEqual, 3)],
                consequent: Some(atomic("x3", Equal, -3)),
                generated_by: None,
                label: None,
            }),
            vec![
                "a 1 [x1 >= 1]",
                "a 2 [x2 <= 3]",
                "a 3 [x3 == -3]",
                "i 5 1 2 0 3",
            ],
        );
    }

    #[test]
    fn write_basic_deduction_without_sequence() {
        test_step_serialization(
            Step::Deduction(Deduction {
                constraint_id: TEST_ID,
                premises: vec![atomic("x1", GreaterEqual, 1), atomic("x2", LessEqual, 3)],
                sequence: vec![],
            }),
            vec!["a 1 [x1 >= 1]", "a 2 [x2 <= 3]", "n 5 1 2 0"],
        );
    }

    #[test]
    fn write_basic_deduction_with_sequence() {
        test_step_serialization(
            Step::Deduction(Deduction {
                constraint_id: TEST_ID,
                premises: vec![atomic("x1", GreaterEqual, 1), atomic("x2", LessEqual, 3)],
                sequence: vec![
                    NonZero::new(1).unwrap(),
                    NonZero::new(3).unwrap(),
                    NonZero::new(4).unwrap(),
                    NonZero::new(2).unwrap(),
                ],
            }),
            vec!["a 1 [x1 >= 1]", "a 2 [x2 <= 3]", "n 5 1 2 0 1 3 4 2"],
        );
    }

    #[test]
    fn write_conclusion_unsat() {
        test_step_serialization(Step::Conclusion(Conclusion::Unsat), vec!["c UNSAT"]);
    }

    #[test]
    fn write_conclusion_dual_bound() {
        test_step_serialization(
            Step::Conclusion(Conclusion::DualBound(atomic("x1", GreaterEqual, 100))),
            vec!["a 1 [x1 >= 100]", "c 1"],
        );
    }

    fn atomic(name: &str, comparison: IntComparison, value: i32) -> IntAtomic<&str, i32> {
        IntAtomic {
            name,
            comparison,
            value,
        }
    }

    fn test_step_serialization(step: Step<&str, i32, &str>, lines: Vec<&str>) {
        let mut buffer = Vec::new();

        {
            let mut writer = ProofWriter::new(&mut buffer);

            match step {
                Step::Inference(inference) => writer.log_inference(inference).unwrap(),
                Step::Deduction(deduction) => writer.log_deduction(deduction).unwrap(),
                Step::Conclusion(conclusion) => writer.log_conclusion(conclusion).unwrap(),
            }
        }

        let actual = String::from_utf8(buffer).expect("valid utf8");
        let expected = lines.join("\n");
        assert_eq!(expected.trim(), actual.trim());
    }
}
