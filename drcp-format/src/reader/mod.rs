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
use std::collections::HashSet;
use std::io;
use std::num::NonZero;
use std::rc::Rc;

pub use error::*;

use crate::IntAtomic;
use crate::SignedIntValue;
use crate::Step;

/// A parser of DRCP proofs. See module documentation on [`crate::reader`] for examples on how to
/// use it.
#[derive(Debug)]
pub struct ProofReader<Source, Int> {
    /// The source of the proof.
    source: Source,

    /// The defined atomics that are used in the proof.
    atomics: BTreeMap<NonZero<u32>, IntAtomic<Rc<str>, Int>>,

    /// A set of all the identifiers encountered, used for interning.
    identifiers: HashSet<Rc<str>>,

    /// The buffer that holds the current line of `source`.
    line_buffer: Vec<u8>,

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
            identifiers: HashSet::default(),
            line_buffer: Vec::new(),
            line_nr: 0,
        }
    }
}

/// The [`IntAtomic`] type that is produced by the [`ProofReader`].
pub type ReadAtomic<Int> = IntAtomic<Rc<str>, Int>;
/// The [`Step`] type that is produced by the [`ProofReader`].
pub type ReadStep<Int> = Step<Rc<str>, Int, Rc<str>>;

impl<Source, Int> ProofReader<Source, Int>
where
    Source: io::BufRead,
    Int: SignedIntValue,
{
    /// Parse the next [`Step`] from the proof file.
    ///
    /// The end-of-file is signified by the value `Ok(None)` and is _not_ an error.
    pub fn next_step(&mut self) -> Result<Option<ReadStep<Int>>, Error> {
        loop {
            self.line_buffer.clear();
            let read_bytes = self.source.read_until(b'\n', &mut self.line_buffer)?;

            // Reading 0 bytes indicates we have reached the end of the file.
            if read_bytes == 0 {
                return Ok(None);
            }

            self.line_nr += 1;

            let line_parser = parser::LineParser::new(
                &self.line_buffer,
                self.line_nr,
                &mut self.identifiers,
                &mut self.atomics,
            );

            let Some(proof_line) = line_parser.parse()? else {
                continue;
            };

            return Ok(Some(proof_line));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Conclusion;
    use crate::Deduction;
    use crate::Inference;
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
    fn inference_with_consequent_hints_reversed() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            i 2 1 0 2 l:inf_name c:1
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
    fn inference_with_consequent_and_negative_atomic_values() {
        let source = r#"
            a 1 [x1 >= -1]
            a 2 [x2 >= -10]
            i 2 1 0 2 c:1 l:inf_name
        "#;

        let a1 = IntAtomic {
            name: Rc::from("x1".to_owned()),
            comparison: GreaterEqual,
            value: -1,
        };

        let a2 = IntAtomic {
            name: Rc::from("x2".to_owned()),
            comparison: GreaterEqual,
            value: -10,
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
