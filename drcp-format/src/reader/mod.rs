//! Proofs can be read using a [`ProofReader`]. It reads the proof line-by-line, and can be thought
//! of as an Iterator over [`Step`].
//!
//! The reader does not do any form of validation of the proof. It is up to the consumer of this
//! crate to ensure that constraint IDs are valid, that inference labels make sense, etc.
//!
//! ```
//! use std::num::NonZero;
//! use std::rc::Rc;
//!
//! use drcp_format::reader::ProofReader;
//! use drcp_format::Conclusion;
//! use drcp_format::Deduction;
//! use drcp_format::Inference;
//! use drcp_format::IntAtomic;
//! use drcp_format::IntComparison::*;
//! use drcp_format::Step;
//!
//! let source = r#"
//!     a 1 [x1 >= 0]
//!     a 2 [x2 >= 0]
//!     i 2 1 0 2 c:1 l:inf_name
//!     n 3 1 -2 0 2
//!     c UNSAT
//! "#;
//!
//! let a1 = IntAtomic {
//!     name: Rc::from("x1".to_owned()),
//!     comparison: GreaterEqual,
//!     value: 0,
//! };
//!
//! let a2 = IntAtomic {
//!     name: Rc::from("x2".to_owned()),
//!     comparison: GreaterEqual,
//!     value: 0,
//! };
//!
//! let mut reader = ProofReader::<_, i32>::new(source.as_bytes());
//!
//! let inference = reader
//!     .next_step()
//!     .expect("no error reading")
//!     .expect("proof step exists");
//!
//! assert_eq!(
//!     inference,
//!     Step::Inference(Inference {
//!         constraint_id: NonZero::new(2).unwrap(),
//!         premises: vec![a1.clone()],
//!         consequent: Some(a2.clone()),
//!         generated_by: Some(NonZero::new(1).unwrap()),
//!         label: Some("inf_name".into()),
//!     })
//! );
//!
//! let deduction = reader
//!     .next_step()
//!     .expect("no error reading")
//!     .expect("proof step exists");
//!
//! assert_eq!(
//!     deduction,
//!     Step::Deduction(Deduction {
//!         constraint_id: NonZero::new(3).unwrap(),
//!         premises: vec![a1.clone(), !a2],
//!         sequence: vec![NonZero::new(2).unwrap()],
//!     })
//! );
//!
//! let conclusion = reader
//!     .next_step()
//!     .expect("no error reading")
//!     .expect("proof step exists");
//!
//! assert_eq!(conclusion, Step::Conclusion(Conclusion::Unsat));
//!
//! let eof = reader.next_step().expect("no error reading");
//! assert_eq!(eof, None);
//! ```

mod error;
mod parser;

use std::collections::BTreeMap;
use std::io;
use std::num::NonZero;
use std::rc::Rc;

use chumsky::Parser;
pub use error::*;

use crate::Conclusion;
use crate::Deduction;
use crate::Inference;
use crate::IntAtomic;
use crate::IntValue;
use crate::Step;

/// A parser of DRCP proofs. See module documentation on [`crate::reader`] for examples on how to
/// use it.
#[derive(Debug)]
pub struct ProofReader<Source, Int> {
    /// The source of the proof.
    source: Source,

    /// The defined atomics that are used in the proof.
    atomics: BTreeMap<NonZero<u32>, IntAtomic<Rc<str>, Int>>,

    /// The buffer that holds the current line of `source`.
    line_buffer: String,

    /// The line number currently being processed in the proof.
    ///
    /// Used for error reporting.
    line_nr: usize,
}

impl<Source, Int> ProofReader<Source, Int> {
    /// Create a new [`ProofReader`].
    ///
    /// Note that [`ProofReader::next_step`] is only implemented for sources that implement
    /// [`io::BufRead`]. It is up to the user to set up the buffered reader.
    pub fn new(source: Source) -> Self {
        ProofReader {
            source,
            atomics: BTreeMap::default(),
            line_buffer: String::new(),
            line_nr: 0,
        }
    }

    fn map_atomic_ids(&self, ids: Vec<NonZero<i32>>) -> Result<Vec<IntAtomic<Rc<str>, Int>>, Error>
    where
        Int: IntValue,
    {
        ids.into_iter()
            .map(|code| self.map_atomic_id(code))
            .collect()
    }

    fn map_atomic_id(&self, id: NonZero<i32>) -> Result<IntAtomic<Rc<str>, Int>, Error>
    where
        Int: IntValue,
    {
        self.atomics
            .get(&id.unsigned_abs())
            .cloned()
            .map(|atomic| if id.is_negative() { !atomic } else { atomic })
            .ok_or(Error::UndefinedAtomic {
                line: self.line_nr,
                code: id,
            })
    }
}

/// The [`IntAtomic`] type that is produced by the [`ProofReader`].
pub type ReadAtomic<Int> = IntAtomic<Rc<str>, Int>;
/// The [`Step`] type that is produced by the [`ProofReader`].
pub type ReadStep<Int> = Step<Rc<str>, Int, Rc<str>>;

impl<Source, Int> ProofReader<Source, Int>
where
    Source: io::BufRead,
    Int: IntValue,
{
    /// Parse the next [`Step`] from the proof file.
    ///
    /// The end-of-file is signified by the value `Ok(None)` and is _not_ an error.
    pub fn next_step(&mut self) -> Result<Option<ReadStep<Int>>, Error> {
        loop {
            self.line_buffer.clear();
            let read_bytes = self.source.read_line(&mut self.line_buffer)?;

            // Reading 0 bytes indicates we have reached the end of the file.
            if read_bytes == 0 {
                return Ok(None);
            }

            self.line_nr += 1;

            // We should get rid of leading or trailing whitespace.
            let trimmed_line = self.line_buffer.trim();

            // If the line is empty, go on to the next line.
            if trimmed_line.is_empty() {
                continue;
            }

            let proof_line: parser::ProofLine<'_, Int> = parser::proof_line()
                .parse(trimmed_line)
                .into_result()
                .map_err(|errs| {
                    assert_eq!(
                        errs.len(),
                        1,
                        "since we do no recovery, any error will terminate the parsing immediately"
                    );

                    let reason = format!("{}", errs[0]);
                    let span = errs[0].span();

                    Error::parse_error(self.line_nr, reason, *span)
                })?;

            match proof_line {
                parser::ProofLine::AtomDefinition(atomic_id, atomic) => {
                    let IntAtomic {
                        name,
                        comparison,
                        value,
                    } = atomic;

                    let _ = self.atomics.insert(
                        atomic_id,
                        IntAtomic {
                            name: name.into(),
                            comparison,
                            value,
                        },
                    );
                }

                parser::ProofLine::Inference {
                    constraint_id,
                    premises,
                    consequent,
                    generated_by,
                    label,
                } => {
                    let inference = Inference {
                        constraint_id,
                        premises: self.map_atomic_ids(premises)?,
                        consequent: consequent.map(|c| self.map_atomic_id(c)).transpose()?,
                        generated_by,
                        label: label.map(|label| label.into()),
                    };

                    return Ok(Some(Step::Inference(inference)));
                }

                parser::ProofLine::Deduction {
                    constraint_id,
                    premises,
                    sequence,
                } => {
                    let deduction = Deduction {
                        constraint_id,
                        premises: self.map_atomic_ids(premises)?,
                        sequence,
                    };

                    return Ok(Some(Step::Deduction(deduction)));
                }

                parser::ProofLine::Conclusion(parser::ProofLineConclusion::Unsat) => {
                    return Ok(Some(Step::Conclusion(Conclusion::Unsat)));
                }

                parser::ProofLine::Conclusion(parser::ProofLineConclusion::DualBound(code)) => {
                    let bound = self.map_atomic_id(code)?;
                    return Ok(Some(Step::Conclusion(Conclusion::DualBound(bound))));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::IntComparison::*;

    #[test]
    fn inference_with_consequent() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            i 2 1 0 2 c:1 l:inf_name
        "#;

        let a1 = IntAtomic {
            name: Rc::from("x1".to_owned()),
            comparison: GreaterEqual,
            value: 0,
        };

        let a2 = IntAtomic {
            name: Rc::from("x2".to_owned()),
            comparison: GreaterEqual,
            value: 0,
        };

        test_single_proof_line(
            source,
            Step::Inference(Inference {
                constraint_id: NonZero::new(2).unwrap(),
                premises: vec![a1],
                consequent: Some(a2),
                generated_by: Some(NonZero::new(1).unwrap()),
                label: Some("inf_name".into()),
            }),
        );
    }

    #[test]
    fn inference_without_consequent() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            i 2 1 -2 0 c:1 l:inf_name
        "#;

        let a1 = IntAtomic {
            name: Rc::from("x1".to_owned()),
            comparison: GreaterEqual,
            value: 0,
        };

        let a2 = IntAtomic {
            name: Rc::from("x2".to_owned()),
            comparison: LessEqual,
            value: -1,
        };

        test_single_proof_line(
            source,
            Step::Inference(Inference {
                constraint_id: NonZero::new(2).unwrap(),
                premises: vec![a1, a2],
                consequent: None,
                generated_by: Some(NonZero::new(1).unwrap()),
                label: Some("inf_name".into()),
            }),
        );
    }

    #[test]
    fn deduction_without_inferences() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            n 2 1 -2 0
        "#;

        let a1 = IntAtomic {
            name: Rc::from("x1".to_owned()),
            comparison: GreaterEqual,
            value: 0,
        };

        let a2 = IntAtomic {
            name: Rc::from("x2".to_owned()),
            comparison: LessEqual,
            value: -1,
        };

        test_single_proof_line(
            source,
            Step::Deduction(Deduction {
                constraint_id: NonZero::new(2).unwrap(),
                premises: vec![a1, a2],
                sequence: vec![],
            }),
        );
    }

    #[test]
    fn deduction_with_inferences() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            n 5 1 -2 0 1 3 2 4
        "#;

        let a1 = IntAtomic {
            name: Rc::from("x1".to_owned()),
            comparison: GreaterEqual,
            value: 0,
        };

        let a2 = IntAtomic {
            name: Rc::from("x2".to_owned()),
            comparison: LessEqual,
            value: -1,
        };

        test_single_proof_line(
            source,
            Step::Deduction(Deduction {
                constraint_id: NonZero::new(5).unwrap(),
                premises: vec![a1, a2],
                sequence: vec![
                    NonZero::new(1).unwrap(),
                    NonZero::new(3).unwrap(),
                    NonZero::new(2).unwrap(),
                    NonZero::new(4).unwrap(),
                ],
            }),
        );
    }

    #[test]
    fn conclusion_unsat() {
        let source = r#"
            c UNSAT
        "#;

        test_single_proof_line(source, Step::Conclusion(Conclusion::Unsat));
    }

    #[test]
    fn conclusion_dual_bound() {
        let source = r#"
            a 1 [x1 >= 4]
            c 1
        "#;

        let a1 = IntAtomic {
            name: Rc::from("x1".to_owned()),
            comparison: GreaterEqual,
            value: 4,
        };

        test_single_proof_line(source, Step::Conclusion(Conclusion::DualBound(a1)));
    }

    fn test_single_proof_line(source: &str, expected_step: ReadStep<i32>) {
        let mut reader = ProofReader::<_, i32>::new(source.as_bytes());

        let parsed_step = reader
            .next_step()
            .expect("no error reading")
            .expect("there is one proof step");

        assert_eq!(expected_step, parsed_step);
    }
}
