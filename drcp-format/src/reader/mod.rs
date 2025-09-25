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
use std::iter::Peekable;
use std::num::NonZero;
use std::ops::Add;
use std::ops::Mul;
use std::rc::Rc;

pub use error::*;

use crate::reader::parser::ProofLine;
use crate::Conclusion;
use crate::Deduction;
use crate::Inference;
use crate::IntAtomic;
use crate::IntComparison;
use crate::IntValue;
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
            line_buffer: Vec::new(),
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
    Int: SignedIntValue,
    Int: From<u8> + Mul<Output = Int> + Add<Output = Int>,
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

            let end_index = if self.line_buffer.last().is_some_and(|byte| *byte == b'\n') {
                self.line_buffer.len() - 1
            } else {
                self.line_buffer.len()
            };

            let Some(proof_line) = parse_proof_line(&self.line_buffer[..end_index], self.line_nr)?
            else {
                continue;
            };

            match proof_line {
                ProofLine::AtomDefinition(atomic_id, atomic) => {
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

                ProofLine::Inference {
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

                ProofLine::Deduction {
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

                ProofLine::Conclusion(parser::ProofLineConclusion::Unsat) => {
                    return Ok(Some(Step::Conclusion(Conclusion::Unsat)));
                }

                ProofLine::Conclusion(parser::ProofLineConclusion::DualBound(code)) => {
                    let bound = self.map_atomic_id(code)?;
                    return Ok(Some(Step::Conclusion(Conclusion::DualBound(bound))));
                }
            }
        }
    }
}

fn parse_proof_line<Int: SignedIntValue>(
    line: &[u8],
    line_nr: usize,
) -> Result<Option<ProofLine<'_, Int>>, Error> {
    let mut input = Input {
        position: 0,
        source: line,
        bytes: line.iter().copied().peekable(),
        line_nr,
    };

    let proof_line = loop {
        match input.next() {
            None | Some(b'\n') => return Ok(None),

            Some(byte) if byte.is_ascii_whitespace() => {
                continue;
            }

            Some(b'a') => break parse_atom_definition(&mut input)?,
            Some(b'i') => break parse_inference(&mut input)?,
            Some(b'n') => break parse_deduction(&mut input)?,
            Some(b'c') => break parse_conclusion(&mut input)?,

            Some(byte) => {
                return Err(Error::ParseError {
                    line_nr,
                    reason: if byte.is_ascii() {
                        format!("expected a, i, n, or c. got '{}'", byte as char)
                    } else {
                        "expected a, i, n, or c".to_owned()
                    },
                    span: (input.position, input.position + 1),
                })
            }
        }
    };

    Ok(Some(proof_line))
}

struct Input<'src, Iter: Iterator> {
    position: usize,
    bytes: Peekable<Iter>,
    source: &'src [u8],
    line_nr: usize,
}

impl<'src, Iter: Iterator<Item = u8>> Input<'src, Iter> {
    fn consume_optional_whitespace(&mut self) {
        // Consume the remaining whitespace
        while self.peek().is_some_and(is_separator) {
            let _ = self.next();
        }
    }

    /// Consume at least one whitespace (not new-line).
    fn consume_separator(&mut self) -> Result<(), Error> {
        let byte = self.consume("whitespace")?;

        if is_separator(byte) {
            self.consume_optional_whitespace();
            Ok(())
        } else {
            Err(Error::ParseError {
                line_nr: self.line_nr,
                reason: if byte.is_ascii() {
                    format!("expected whitespace. got '{}'", byte as char)
                } else {
                    "expected whitespace".to_owned()
                },
                span: (self.position, self.position + 1),
            })
        }
    }

    /// Consume the next byte in the input.
    fn next(&mut self) -> Option<u8> {
        let byte = self.bytes.next()?;
        self.position += 1;
        Some(byte)
    }

    /// Look one character ahead.
    fn peek(&mut self) -> Option<u8> {
        self.bytes.peek().copied()
    }

    /// Consume the next byte in the input, and error if it does not exist.
    fn consume(&mut self, expected: &str) -> Result<u8, Error> {
        self.next().ok_or_else(|| Error::ParseError {
            line_nr: self.line_nr,
            reason: format!("unexpected input: expected {expected} - got EOF"),
            span: (self.position, self.position + 1),
        })
    }

    fn consume_unsigned_integer<Int>(&mut self) -> Result<Int, Error>
    where
        Int: IntValue,
    {
        let mut number: Int = self.consume_digit()?;

        while self.peek().is_some_and(|byte| byte.is_ascii_digit()) {
            let next_digit = self.consume_digit()?;
            number = number.shift_left() + next_digit;
        }

        Ok(number)
    }

    fn consume_signed_integer<Int>(&mut self) -> Result<Int, Error>
    where
        Int: SignedIntValue,
    {
        // Remove an optional leading '-'.
        let is_positive = if let Some(b'-') = self.peek() {
            let _ = self.next();
            false
        } else {
            true
        };

        let mut number: Int = self.consume_digit()?;

        while self.peek().is_some_and(|byte| byte.is_ascii_digit()) {
            let next_digit = self.consume_digit()?;
            number = number.shift_left() + next_digit;
        }

        if is_positive {
            Ok(number)
        } else {
            Ok(number.negate())
        }
    }

    fn consume_u32(&mut self) -> Result<NonZero<u32>, Error> {
        let start = self.position;
        let integer = self.consume_unsigned_integer::<u32>()?;
        NonZero::new(integer).ok_or_else(|| Error::ParseError {
            line_nr: self.line_nr,
            reason: "unexpected input: expected non-zero unsigned integer - got 0".to_owned(),
            span: (start, self.position),
        })
    }

    fn consume_i32(&mut self) -> Result<NonZero<i32>, Error> {
        let start = self.position;
        let integer = self.consume_signed_integer::<i32>()?;
        NonZero::new(integer).ok_or_else(|| Error::ParseError {
            line_nr: self.line_nr,
            reason: "unexpected input: expected non-zero unsigned integer - got 0".to_owned(),
            span: (start, self.position),
        })
    }

    fn consume_str(&mut self, literal: &str) -> Result<(), Error> {
        let start = self.position;

        for byte in literal.bytes() {
            let _ = self
                .next()
                .filter(|input_byte| *input_byte == byte)
                .ok_or_else(|| Error::ParseError {
                    line_nr: self.line_nr,
                    reason: format!("unexpected input: expected {literal}"),
                    span: (start, self.position),
                })?;
        }

        Ok(())
    }

    fn consume_digit<Int: IntValue>(&mut self) -> Result<Int, Error> {
        let byte = self.consume("digit")?;

        if byte.is_ascii_digit() {
            Ok(Int::from_digit(byte))
        } else {
            Err(Error::ParseError {
                line_nr: self.line_nr,
                reason: "unexpected input: expected digit".to_owned(),
                span: (self.position, self.position + 1),
            })
        }
    }

    fn consume_identifier(&mut self) -> Result<&'src str, Error> {
        let is_identifier_char = |byte: u8| byte == b'_' || byte.is_ascii_alphanumeric();

        let start = self.position;

        while self.peek().is_some_and(is_identifier_char) {
            let _ = self.consume("identifier")?;
        }

        if start == self.position {
            return Err(Error::ParseError {
                line_nr: self.line_nr,
                reason: "unexpected input: expected identifier".to_owned(),
                span: (start, self.position),
            });
        }

        std::str::from_utf8(&self.source[start..self.position]).map_err(|_| Error::ParseError {
            line_nr: self.line_nr,
            reason: "invalid utf8".to_owned(),
            span: (start, self.position),
        })
    }
}

fn parse_atom_definition<'src, Int, Iter>(
    input: &mut Input<'src, Iter>,
) -> Result<ProofLine<'src, Int>, Error>
where
    Int: SignedIntValue,
    Iter: Iterator<Item = u8>,
{
    input.consume_separator()?;

    let code = input.consume_u32()?;
    input.consume_separator()?;

    input.consume_str("[")?;

    let name = input.consume_identifier()?;
    input.consume_separator()?;

    let comparison = match input.consume("comparison")? {
        b'>' => IntComparison::GreaterEqual,
        b'<' => IntComparison::LessEqual,
        b'=' => IntComparison::Equal,
        b'!' => IntComparison::NotEqual,
        _ => {
            return Err(Error::ParseError {
                line_nr: input.line_nr,
                reason: "unexpected input: expected comparison".to_owned(),
                span: (input.position - 1, input.position),
            });
        }
    };
    input.consume_str("=")?;

    input.consume_separator()?;
    let value = input.consume_signed_integer()?;

    input.consume_str("]")?;

    let atomic = IntAtomic {
        name,
        comparison,
        value,
    };

    Ok(ProofLine::AtomDefinition(code, atomic))
}

fn parse_inference<'src, Int, Iter>(
    input: &mut Input<'src, Iter>,
) -> Result<ProofLine<'src, Int>, Error>
where
    Int: IntValue,
    Iter: Iterator<Item = u8>,
{
    input.consume_separator()?;

    let constraint_id = input.consume_u32()?;

    let mut premises = vec![];

    loop {
        input.consume_separator()?;

        if !input.peek().is_some_and(is_start_of_signed_number) {
            break;
        }

        let next_premise = input.consume_signed_integer::<i32>()?;

        if let Some(premise) = NonZero::new(next_premise) {
            premises.push(premise);
        } else {
            break;
        }
    }

    let mut consequent = None;
    let mut generated_by = None;
    let mut label = None;

    if input.peek().is_some() {
        input.consume_separator()?;
    }

    match input.peek() {
        Some(byte) if is_start_of_signed_number(byte) => {
            consequent = Some(input.consume_i32()?);
        }
        Some(b'c') => {
            input.consume_str("c:")?;
            generated_by = Some(input.consume_u32()?);
        }
        Some(b'l') => {
            input.consume_str("l:")?;
            label = Some(input.consume_identifier()?);
        }
        _ => {}
    }

    if input.peek().is_some() {
        input.consume_separator()?;
    }

    match input.peek() {
        Some(b'c') => {
            input.consume_str("c:")?;
            generated_by = Some(input.consume_u32()?);
        }
        Some(b'l') => {
            input.consume_str("l:")?;
            label = Some(input.consume_identifier()?);
        }
        _ => {}
    }

    if input.peek().is_some() {
        input.consume_separator()?;
    }

    match input.peek() {
        Some(b'c') => {
            input.consume_str("c:")?;
            generated_by = Some(input.consume_u32()?);
        }
        Some(b'l') => {
            input.consume_str("l:")?;
            label = Some(input.consume_identifier()?);
        }
        _ => {}
    }

    Ok(ProofLine::Inference {
        constraint_id,
        premises,
        consequent,
        generated_by,
        label,
    })
}

fn parse_deduction<'src, Int, Iter>(
    input: &mut Input<'src, Iter>,
) -> Result<ProofLine<'src, Int>, Error>
where
    Int: IntValue,
    Iter: Iterator<Item = u8>,
{
    input.consume_separator()?;

    let constraint_id = input.consume_u32()?;

    let mut premises = vec![];

    loop {
        input.consume_separator()?;

        if !input.peek().is_some_and(is_start_of_signed_number) {
            break;
        }

        let next_premise = input.consume_signed_integer::<i32>()?;

        if let Some(premise) = NonZero::new(next_premise) {
            premises.push(premise);
        } else {
            break;
        }
    }

    let mut sequence = vec![];

    loop {
        if input.peek().is_none() {
            break;
        }

        input.consume_separator()?;
        if !input.peek().is_some_and(is_start_of_signed_number) {
            break;
        }

        let next_constraint = input.consume_u32()?;
        sequence.push(next_constraint);
    }

    Ok(ProofLine::Deduction {
        constraint_id,
        premises,
        sequence,
    })
}

fn parse_conclusion<'src, Int, Iter>(
    input: &mut Input<'src, Iter>,
) -> Result<ProofLine<'src, Int>, Error>
where
    Int: IntValue,
    Iter: Iterator<Item = u8>,
{
    input.consume_separator()?;

    match input.peek() {
        Some(b'U') => {
            input.consume_str("UNSAT")?;
            Ok(ProofLine::Conclusion(parser::ProofLineConclusion::Unsat))
        }

        Some(byte) if is_start_of_signed_number(byte) => {
            let code = input.consume_i32()?;
            Ok(ProofLine::Conclusion(
                parser::ProofLineConclusion::DualBound(code),
            ))
        }

        _ => Err(Error::ParseError {
            line_nr: input.line_nr,
            reason: "unexpected input: expected conclusion - got EOF".to_owned(),
            span: (input.position, input.position + 1),
        }),
    }
}

fn is_separator(byte: u8) -> bool {
    byte != b'\n' && byte.is_ascii_whitespace()
}

fn is_start_of_signed_number(byte: u8) -> bool {
    byte.is_ascii_digit() || byte == b'-'
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
