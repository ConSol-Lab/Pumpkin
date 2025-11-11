use std::collections::BTreeMap;
use std::collections::HashSet;
use std::iter::Copied;
use std::iter::Peekable;
use std::num::NonZero;
use std::rc::Rc;
use std::slice::Iter;

use crate::Conclusion;
use crate::Deduction;
use crate::Inference;
use crate::IntComparison;
use crate::IntValue;
use crate::SignedIntValue;
use crate::Step;
use crate::reader::Error;
use crate::reader::ReadAtomic;
use crate::reader::ReadStep;

/// Parses a single line to possibly return a proof step.
///
/// See [`LineParser::parse`] for more information.
pub(super) struct LineParser<'src, 'data, Int> {
    source: &'src [u8],
    bytes: Peekable<Copied<Iter<'src, u8>>>,
    position: usize,
    line_nr: usize,

    identifiers: &'data mut HashSet<Rc<str>>,
    atomics: &'data mut BTreeMap<NonZero<u32>, ReadAtomic<Int>>,
}

impl<'src, 'data, Int> LineParser<'src, 'data, Int>
where
    Int: SignedIntValue,
{
    pub(super) fn new(
        line: &'src [u8],
        line_nr: usize,
        identifiers: &'data mut HashSet<Rc<str>>,
        atomics: &'data mut BTreeMap<NonZero<u32>, ReadAtomic<Int>>,
    ) -> Self {
        let stripped_line_idx = if line.last().is_some_and(|byte| *byte == b'\n') {
            line.len() - 1
        } else {
            line.len()
        };

        let stripped_line = &line[..stripped_line_idx];

        LineParser {
            position: 0,
            source: stripped_line,
            bytes: stripped_line.iter().copied().peekable(),
            line_nr,
            identifiers,
            atomics,
        }
    }

    /// Parses a single line from a proof.
    ///
    /// The line may be empty, in which case this returns `Ok(None)`. Otherwise, if there is
    /// content on the line, it either successfully parses a step or returns a parser error.
    pub(super) fn parse(mut self) -> Result<Option<ReadStep<Int>>, Error> {
        loop {
            match self.next() {
                None | Some(b'\n') => return Ok(None),

                Some(byte) if byte.is_ascii_whitespace() => {
                    continue;
                }

                Some(b'a') => self.parse_atom_definition()?,
                Some(b'i') => return self.parse_inference().map(Some),
                Some(b'n') => return self.parse_deduction().map(Some),
                Some(b'c') => return self.parse_conclusion().map(Some),

                Some(byte) => {
                    return Err(Error::ParseError {
                        line_nr: self.line_nr,
                        reason: if byte.is_ascii() {
                            format!("expected a, i, n, or c. got '{}'", byte as char)
                        } else {
                            "expected a, i, n, or c".to_owned()
                        },
                        span: (self.position, self.position + 1),
                    });
                }
            }
        }
    }

    /// Parses a line in the following shape:
    /// ```ignored
    /// "a" <unsigned non-zero u32> "[" <identifier> <comparison> <integer> "]"
    ///
    /// comparison := ">=" | "<=" | "==" | "!="
    /// ```
    fn parse_atom_definition(&mut self) -> Result<(), Error>
    where
        Int: SignedIntValue,
    {
        self.consume_separator()?;

        let code = self.consume_u32()?;
        self.consume_separator()?;

        self.consume_str("[")?;

        let name = self.consume_identifier()?;
        self.consume_separator()?;

        let comparison = match self.consume("comparison")? {
            b'>' => IntComparison::GreaterEqual,
            b'<' => IntComparison::LessEqual,
            b'=' => IntComparison::Equal,
            b'!' => IntComparison::NotEqual,
            _ => {
                return Err(Error::ParseError {
                    line_nr: self.line_nr,
                    reason: "unexpected self. expected comparison".to_owned(),
                    span: (self.position - 1, self.position),
                });
            }
        };
        self.consume_str("=")?;

        self.consume_separator()?;
        let value = self.consume_signed_integer()?;

        self.consume_str("]")?;

        let interned_name = self.intern(name);

        let _ = self.atomics.insert(
            code,
            ReadAtomic {
                name: interned_name,
                comparison,
                value,
            },
        );

        Ok(())
    }

    /// Parses a line in the following shape:
    /// ```ignored
    /// "i" <unsigned non-zero u32> (<signed non-zero u32>)* "0" (<signed non-zero u32>)? ("l:<label>")? ("c:<unsigned non-zero u32>")?
    /// ```
    fn parse_inference(&mut self) -> Result<ReadStep<Int>, Error>
    where
        Int: IntValue,
    {
        self.consume_separator()?;

        let constraint_id = self.consume_u32()?;

        let mut premises = vec![];

        loop {
            self.consume_separator()?;

            if !self.peek().is_some_and(is_start_of_signed_number) {
                break;
            }

            let next_premise = self.consume_signed_integer::<i32>()?;

            if let Some(premise) = NonZero::new(next_premise) {
                let atomic = self.map_atomic_id(premise)?;
                premises.push(atomic);
            } else {
                break;
            }
        }

        let mut consequent = None;
        let mut generated_by = None;
        let mut label = None;

        if self.peek().is_some() {
            self.consume_separator()?;
        }

        match self.peek() {
            Some(byte) if is_start_of_signed_number(byte) => {
                let consequent_code = self.consume_i32()?;
                let atomic = self.map_atomic_id(consequent_code)?;
                consequent = Some(atomic);
            }
            Some(b'c') => {
                self.consume_str("c:")?;
                generated_by = Some(self.consume_u32()?);
            }
            Some(b'l') => {
                self.consume_str("l:")?;

                let label_str = self.consume_identifier()?;
                let interned_label = self.intern(label_str);

                label = Some(interned_label);
            }
            _ => {}
        }

        if self.peek().is_some() {
            self.consume_separator()?;
        }

        match self.peek() {
            Some(b'c') => {
                self.consume_str("c:")?;
                generated_by = Some(self.consume_u32()?);
            }
            Some(b'l') => {
                self.consume_str("l:")?;
                let label_str = self.consume_identifier()?;
                let interned_label = self.intern(label_str);

                label = Some(interned_label);
            }
            _ => {}
        }

        if self.peek().is_some() {
            self.consume_separator()?;
        }

        match self.peek() {
            Some(b'c') => {
                self.consume_str("c:")?;
                generated_by = Some(self.consume_u32()?);
            }
            Some(b'l') => {
                self.consume_str("l:")?;
                let label_str = self.consume_identifier()?;
                let interned_label = self.intern(label_str);

                label = Some(interned_label);
            }
            _ => {}
        }

        Ok(Step::Inference(Inference {
            constraint_id,
            premises,
            consequent,
            generated_by,
            label,
        }))
    }

    /// Parses a line in the following shape:
    /// ```ignored
    /// "n" <unsigned non-zero u32> (<signed non-zero u32>)* "0" (<unsigned non-zero u32>)*
    /// ```
    fn parse_deduction(&mut self) -> Result<ReadStep<Int>, Error>
    where
        Int: IntValue,
    {
        self.consume_separator()?;

        let constraint_id = self.consume_u32()?;

        let mut premises = vec![];

        loop {
            self.consume_separator()?;

            if !self.peek().is_some_and(is_start_of_signed_number) {
                break;
            }

            let next_premise = self.consume_signed_integer::<i32>()?;

            if let Some(premise) = NonZero::new(next_premise) {
                let atomic = self.map_atomic_id(premise)?;
                premises.push(atomic);
            } else {
                break;
            }
        }

        let mut sequence = vec![];

        loop {
            if self.peek().is_none() {
                break;
            }

            self.consume_separator()?;
            if !self.peek().is_some_and(is_start_of_signed_number) {
                break;
            }

            let next_constraint = self.consume_u32()?;
            sequence.push(next_constraint);
        }

        Ok(Step::Deduction(Deduction {
            constraint_id,
            premises,
            sequence,
        }))
    }

    /// Parses a line in the following shape:
    /// ```ignored
    /// "c" ("UNSAT" | <signed non-zero u32>)
    /// ```
    fn parse_conclusion(&mut self) -> Result<ReadStep<Int>, Error>
    where
        Int: IntValue,
    {
        self.consume_separator()?;

        match self.peek() {
            Some(b'U') => {
                self.consume_str("UNSAT")?;
                Ok(Step::Conclusion(Conclusion::Unsat))
            }

            Some(byte) if is_start_of_signed_number(byte) => {
                let code = self.consume_i32()?;
                let atomic = self.map_atomic_id(code)?;
                Ok(Step::Conclusion(Conclusion::DualBound(atomic)))
            }

            _ => Err(Error::ParseError {
                line_nr: self.line_nr,
                reason: "unexpected self. expected conclusion - got EOF".to_owned(),
                span: (self.position, self.position + 1),
            }),
        }
    }

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

    fn consume_unsigned_integer<I>(&mut self) -> Result<I, Error>
    where
        I: IntValue,
    {
        let mut number: I = self.consume_digit()?;

        while self.peek().is_some_and(|byte| byte.is_ascii_digit()) {
            let next_digit = self.consume_digit()?;
            number = number.shift_left() + next_digit;
        }

        Ok(number)
    }

    fn consume_signed_integer<I>(&mut self) -> Result<I, Error>
    where
        I: SignedIntValue,
    {
        // Remove an optional leading '-'.
        let is_positive = if let Some(b'-') = self.peek() {
            let _ = self.next();
            false
        } else {
            true
        };

        let mut number: I = self.consume_digit()?;

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

    fn consume_digit<I: IntValue>(&mut self) -> Result<I, Error> {
        let byte = self.consume("digit")?;

        if byte.is_ascii_digit() {
            Ok(I::from_digit(byte))
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

    fn map_atomic_id(&self, id: NonZero<i32>) -> Result<ReadAtomic<Int>, Error> {
        self.atomics
            .get(&id.unsigned_abs())
            .cloned()
            .map(|atomic| if id.is_negative() { !atomic } else { atomic })
            .ok_or(Error::UndefinedAtomic {
                line: self.line_nr,
                code: id,
            })
    }

    fn intern(&mut self, string: &str) -> Rc<str> {
        match self.identifiers.get(string) {
            Some(string_ref) => Rc::clone(string_ref),
            None => {
                let string_ref = Rc::from(string);
                let _ = self.identifiers.insert(Rc::clone(&string_ref));
                string_ref
            }
        }
    }
}

fn is_separator(byte: u8) -> bool {
    byte != b'\n' && byte.is_ascii_whitespace()
}

fn is_start_of_signed_number(byte: u8) -> bool {
    byte.is_ascii_digit() || byte == b'-'
}
