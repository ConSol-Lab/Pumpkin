//! Writing croc proofs in the block format (experimental).

use std::collections::HashMap;
use std::hash::Hash;
use std::io::Write;
use std::num::NonZero;
use std::rc::Rc;

use crate::Conclusion;
use crate::ConstraintId;
use crate::IntAtomic;
use crate::IntComparison;
use crate::IntValue;

/// One inference within a [`Block`].
///
/// The atoms in `nogood` cannot all hold at the same time. The last atom is the pivot: the
/// checker negates it and looks for the result in the nogood the chain has reached so far, so
/// a writer must order the atoms to put it last.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockInference<Identifier, Int, Label> {
    /// The nogood of the inference, pivot last.
    pub nogood: Vec<IntAtomic<Identifier, Int>>,
    /// The constraint this inference comes from. [`None`] denotes the initial domains, which
    /// are written as constraint `0`.
    pub constraint: Option<ConstraintId>,
    /// The rule which licensed the inference.
    pub label: Label,
    /// Positions in the cited constraint that the rule reads, written as `h:`; empty for the
    /// rules that read none.
    pub hint: Vec<u32>,
}

/// An inference whose atomic constraints and rule are given by the references
/// [`BlockProofWriter::atomic_reference`] and [`BlockProofWriter::label_reference`] return, for
/// a producer that keeps the references of the atoms it writes many times.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ReferencedInference {
    /// The references of the atoms of the nogood of the inference, pivot last in a block.
    pub nogood: Vec<NonZero<i32>>,
    /// The constraint this inference comes from; [`None`] for the initial domains.
    pub constraint: Option<ConstraintId>,
    /// The reference of the rule which licensed the inference.
    pub label: NonZero<u32>,
    /// Positions in the cited constraint that the rule reads.
    pub hint: Vec<u32>,
}

/// One derivation: a resolution chain and the nogood it derives.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block<Identifier, Int, Label> {
    /// The identifier the derived nogood takes. Derived constraints continue the numbering of
    /// the model's constraints.
    pub constraint_id: ConstraintId,
    /// The derived nogood.
    pub nogood: Vec<IntAtomic<Identifier, Int>>,
    /// The chain, starting at the conflict. Each subsequent inference is resolved against the
    /// nogood reached so far.
    pub inferences: Vec<BlockInference<Identifier, Int, Label>>,
}

/// A nogood and the inferences that refute it: assuming the nogood's atoms, the inferences are
/// applied in order, each whose other atoms hold excluding its last, until one excludes a whole
/// domain. Unlike a [`Block`], the order is the order of propagation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Refutation<Identifier, Int, Label> {
    /// The identifier the refuted nogood takes.
    pub constraint_id: ConstraintId,
    /// The nogood that is refuted.
    pub nogood: Vec<IntAtomic<Identifier, Int>>,
    /// The inferences, in the order they propagate.
    pub inferences: Vec<BlockInference<Identifier, Int, Label>>,
}

/// Write a proof in the block format to some sink, as text or in the binary form.
#[derive(Debug)]
pub struct BlockProofWriter<W, Int> {
    writer: W,
    /// In the binary form, the number of each variable name declared so far; `None` for text.
    binary_names: Option<HashMap<Rc<str>, u64>>,
    /// The first character of the last line that opened a block, `a` after a declaration.
    block_prefix: u8,
    /// Keyed on a shared name, so an atom that has been seen before costs a reference count
    /// rather than an allocation. A large proof writes millions of atom occurrences.
    defined_atomics: HashMap<IntAtomic<Rc<str>, Int>, NonZero<i32>>,
    next_atomic_id: i32,
    defined_labels: HashMap<Box<str>, NonZero<u32>>,
    next_label_id: u32,
}

impl<W, Int> BlockProofWriter<W, Int> {
    /// Create a new proof writer which writes the proof to an underlying sink implementing
    /// [`Write`].
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            binary_names: None,
            block_prefix: b' ',
            defined_atomics: HashMap::default(),
            next_atomic_id: 0,
            defined_labels: HashMap::default(),
            next_label_id: 0,
        }
    }
}

/// The mark a proof in the binary form starts with.
pub const BINARY_MARK: &[u8] = b"CROCB\x01";

impl<W: Write, Int> BlockProofWriter<W, Int> {
    /// Create a writer of the binary form fzn-croc-check reads: the mark,
    /// then records, each a tag byte and its fields, numbers in LEB128 and signed numbers zigzag
    /// encoded. Atomic constraint declarations, variable names and the inference lines inside
    /// blocks are binary records; every other line is a text record.
    pub fn new_binary(mut writer: W) -> std::io::Result<Self> {
        writer.write_all(BINARY_MARK)?;
        let mut proof_writer = Self::new(writer);
        proof_writer.binary_names = Some(HashMap::default());
        Ok(proof_writer)
    }
}

/// The atomic constraint references, cited constraint, label and hint positions of an inference
/// line, `  <references> c:<constraint> l:<label> [h:<positions>]`, or `None` for another line.
fn parse_inference_line(line: &str) -> Option<(Vec<i64>, u32, u32, Vec<u32>)> {
    let mut fields = line.split_whitespace().peekable();
    let mut codes = vec![];
    while let Some(field) = fields.peek() {
        if field.starts_with("c:") {
            break;
        }
        codes.push(field.parse().ok()?);
        let _ = fields.next();
    }
    let constraint = fields.next()?.strip_prefix("c:")?.parse().ok()?;
    let label = fields.next()?.strip_prefix("l:")?.parse().ok()?;
    let hint = match fields.next() {
        Some(field) => field
            .strip_prefix("h:")?
            .split(',')
            .filter(|position| !position.is_empty())
            .map(|position| position.parse().ok())
            .collect::<Option<Vec<u32>>>()?,
        None => vec![],
    };
    fields
        .next()
        .is_none()
        .then_some((codes, constraint, label, hint))
}

/// Write `value` in LEB128.
fn write_varint(writer: &mut impl Write, mut value: u64) -> std::io::Result<()> {
    loop {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        if value == 0 {
            return writer.write_all(&[byte]);
        }
        writer.write_all(&[byte | 0x80])?;
    }
}

/// Write `value` zigzag encoded, in LEB128.
fn write_signed(writer: &mut impl Write, value: i64) -> std::io::Result<()> {
    write_varint(writer, ((value << 1) ^ (value >> 63)) as u64)
}

impl<W: Write, Int: IntValue + Hash> BlockProofWriter<W, Int> {
    /// Write a line: a text record in the binary form, except an inference line inside a
    /// resolution or refutation block, which is a binary record.
    fn write_text_line(&mut self, line: &str) -> std::io::Result<()> {
        match line.as_bytes().first() {
            Some(first) if !first.is_ascii_whitespace() => self.block_prefix = *first,
            _ => {
                if self.binary_names.is_some()
                    && matches!(self.block_prefix, b'r' | b'g')
                    && let Some((codes, constraint, label, hint)) = parse_inference_line(line)
                {
                    return self.write_inference_record(
                        codes.iter().copied(),
                        codes.len(),
                        constraint,
                        label,
                        &hint,
                    );
                }
            }
        }
        if self.binary_names.is_some() {
            self.writer.write_all(&[0])?;
            write_varint(&mut self.writer, line.len() as u64)?;
            self.writer.write_all(line.as_bytes())
        } else {
            writeln!(&mut self.writer, "{line}")
        }
    }

    /// Write the binary record of an inference line.
    fn write_inference_record(
        &mut self,
        codes: impl Iterator<Item = i64>,
        num_codes: usize,
        constraint: u32,
        label: u32,
        hint: &[u32],
    ) -> std::io::Result<()> {
        self.writer.write_all(&[3])?;
        write_varint(&mut self.writer, num_codes as u64)?;
        for code in codes {
            write_signed(&mut self.writer, code)?;
        }
        write_varint(&mut self.writer, u64::from(constraint))?;
        write_varint(&mut self.writer, u64::from(label))?;
        write_varint(&mut self.writer, hint.len() as u64)?;
        for &position in hint {
            write_varint(&mut self.writer, u64::from(position))?;
        }
        Ok(())
    }

    /// Write an inference: the first line of a block, prefixed, or a continuation line, which
    /// is a binary record in the binary form.
    fn write_inference(
        &mut self,
        prefix: Option<&str>,
        codes: &[NonZero<i32>],
        constraint: Option<ConstraintId>,
        label: NonZero<u32>,
        hint: &[u32],
    ) -> std::io::Result<()> {
        let constraint = constraint.map(NonZero::get).unwrap_or(0);
        if prefix.is_none() && self.binary_names.is_some() {
            return self.write_inference_record(
                codes.iter().map(|code| i64::from(code.get())),
                codes.len(),
                constraint,
                label.get(),
                hint,
            );
        }
        let mut line = String::from(prefix.unwrap_or(" "));
        for code in codes {
            line.push_str(&format!(" {code}"));
        }
        line.push_str(&format!(" c:{constraint} l:{label}"));
        for (index, position) in hint.iter().enumerate() {
            if index == 0 {
                line.push_str(&format!(" h:{position}"));
            } else {
                line.push_str(&format!(",{position}"));
            }
        }
        self.write_text_line(&line)
    }

    /// Write one block.
    ///
    /// All the atoms and labels the block uses are declared first, since a declaration must
    /// appear before its first use and the chain does not visit them in declaration order.
    pub fn log_block<Identifier: Into<Rc<str>>, Label: Into<Box<str>>>(
        &mut self,
        block: Block<Identifier, Int, Label>,
    ) -> std::io::Result<()> {
        let Block {
            constraint_id,
            nogood,
            inferences,
        } = block;

        // Resolving the codes writes the declarations, so it has to happen before any part of
        // the block itself is written.
        let mut lines = Vec::with_capacity(inferences.len());

        for inference in inferences {
            let codes = inference
                .nogood
                .into_iter()
                .map(|atomic| self.get_atomic_code(atomic))
                .collect::<Result<Vec<_>, _>>()?;

            lines.push((codes, inference.constraint, inference.label, inference.hint));
        }

        let derived = nogood
            .into_iter()
            .map(|atomic| self.get_atomic_code(atomic))
            .collect::<Result<Vec<_>, _>>()?;

        let mut labelled = Vec::with_capacity(lines.len());
        for (codes, constraint, label, hint) in lines {
            let label = self.get_label_code(label)?;
            labelled.push(ReferencedInference {
                nogood: codes,
                constraint,
                label,
                hint,
            });
        }

        self.log_referenced_block(constraint_id, &derived, &labelled)
    }

    /// Write one block whose atoms and rules are given by their references, which are declared
    /// already.
    pub fn log_referenced_block(
        &mut self,
        constraint_id: ConstraintId,
        nogood: &[NonZero<i32>],
        inferences: &[ReferencedInference],
    ) -> std::io::Result<()> {
        for (index, inference) in inferences.iter().enumerate() {
            let prefix = (index == 0).then_some("r");
            self.write_inference(
                prefix,
                &inference.nogood,
                inference.constraint,
                inference.label,
                &inference.hint,
            )?;
        }

        let mut line = format!("e {constraint_id}");
        for code in nogood {
            line.push_str(&format!(" {code}"));
        }
        self.write_text_line(&line)
    }

    /// Write a refutation: a `g` line with the nogood, and the inferences below it.
    pub fn log_refutation<Identifier: Into<Rc<str>>, Label: Into<Box<str>>>(
        &mut self,
        refutation: Refutation<Identifier, Int, Label>,
    ) -> std::io::Result<()> {
        let Refutation {
            constraint_id,
            nogood,
            inferences,
        } = refutation;

        // Resolving the codes writes the declarations, so it has to happen before any part of
        // the refutation itself is written.
        let refuted = nogood
            .into_iter()
            .map(|atomic| self.get_atomic_code(atomic))
            .collect::<Result<Vec<_>, _>>()?;

        let mut lines = Vec::with_capacity(inferences.len());
        for inference in inferences {
            let codes = inference
                .nogood
                .into_iter()
                .map(|atomic| self.get_atomic_code(atomic))
                .collect::<Result<Vec<_>, _>>()?;
            let label = self.get_label_code(inference.label)?;
            lines.push(ReferencedInference {
                nogood: codes,
                constraint: inference.constraint,
                label,
                hint: inference.hint,
            });
        }

        self.log_referenced_refutation(constraint_id, &refuted, &lines)
    }

    /// Write a refutation whose atoms and rules are given by their references, which are
    /// declared already.
    pub fn log_referenced_refutation(
        &mut self,
        constraint_id: ConstraintId,
        nogood: &[NonZero<i32>],
        inferences: &[ReferencedInference],
    ) -> std::io::Result<()> {
        let mut line = format!("g {constraint_id}");
        for code in nogood {
            line.push_str(&format!(" {code}"));
        }
        self.write_text_line(&line)?;

        for inference in inferences {
            self.write_inference(
                None,
                &inference.nogood,
                inference.constraint,
                inference.label,
                &inference.hint,
            )?;
        }

        Ok(())
    }

    /// Write a line as it is: the steps another program produced for the head of the proof.
    pub fn log_line(&mut self, line: &str) -> std::io::Result<()> {
        self.write_text_line(line)
    }

    /// The reference of an atomic constraint, declaring it first if it is new.
    pub fn atomic_reference<Identifier: Into<Rc<str>>>(
        &mut self,
        atomic: IntAtomic<Identifier, Int>,
    ) -> std::io::Result<NonZero<i32>> {
        self.get_atomic_code(atomic)
    }

    /// Declare a rule label if it is new, and return its number.
    pub fn label_reference(&mut self, label: &str) -> std::io::Result<NonZero<u32>> {
        self.get_label_code(label)
    }

    /// Delete a constraint from the checker's state.
    pub fn log_deletion(&mut self, constraint_id: ConstraintId) -> std::io::Result<()> {
        self.write_text_line(&format!("d {constraint_id}"))
    }

    /// Conclude the proof with a conclusion.
    pub fn log_conclusion<Identifier: Into<Rc<str>>>(
        &mut self,
        conclusion: Conclusion<Identifier, Int>,
    ) -> std::io::Result<()> {
        match conclusion {
            Conclusion::Unsat => self.write_text_line("c UNSAT"),
            Conclusion::DualBound(atomic) => {
                let code = self.get_atomic_code(atomic)?;
                self.write_text_line(&format!("c {code}"))
            }
        }
    }

    fn get_atomic_code<Identifier: Into<Rc<str>>>(
        &mut self,
        atomic: IntAtomic<Identifier, Int>,
    ) -> std::io::Result<NonZero<i32>> {
        let key = IntAtomic {
            name: atomic.name.into(),
            comparison: atomic.comparison,
            value: atomic.value,
        };

        if !self.defined_atomics.contains_key(&key) {
            self.next_atomic_id += 1;

            let id = NonZero::new(self.next_atomic_id)
                .expect("next_atomic_id starts at 0, and is incremented, so it can never be 0");

            let _ = self.defined_atomics.insert(key.clone(), id);
            let _ = self.defined_atomics.insert(!key.clone(), -id);

            self.block_prefix = b'a';
            match self.binary_names.as_mut() {
                Some(names) => {
                    let next = names.len() as u64 + 1;
                    let number = match names.get(&key.name) {
                        Some(&number) => number,
                        None => {
                            let _ = names.insert(Rc::clone(&key.name), next);
                            self.writer.write_all(&[1])?;
                            write_varint(&mut self.writer, key.name.len() as u64)?;
                            self.writer.write_all(key.name.as_bytes())?;
                            next
                        }
                    };
                    self.writer.write_all(&[2])?;
                    write_varint(&mut self.writer, id.get() as u64)?;
                    write_varint(&mut self.writer, number)?;
                    let comparator = match key.comparison {
                        IntComparison::LessEqual => 0,
                        IntComparison::GreaterEqual => 1,
                        IntComparison::Equal => 2,
                        IntComparison::NotEqual => 3,
                    };
                    self.writer.write_all(&[comparator])?;
                    let constant: i64 = key
                        .value
                        .to_string()
                        .parse()
                        .expect("an integer value prints as an integer");
                    write_signed(&mut self.writer, constant)?;
                }
                None => writeln!(&mut self.writer, "a {id} {key}")?,
            }
        }

        Ok(self.defined_atomics[&key])
    }

    fn get_label_code<Label: Into<Box<str>>>(
        &mut self,
        label: Label,
    ) -> std::io::Result<NonZero<u32>> {
        let key = label.into();

        if !self.defined_labels.contains_key(&key) {
            self.next_label_id += 1;

            let id = NonZero::new(self.next_label_id)
                .expect("next_label_id starts at 0, and is incremented, so it can never be 0");

            let _ = self.defined_labels.insert(key.clone(), id);

            self.write_text_line(&format!("l {id} {key}"))?;
        }

        Ok(self.defined_labels[&key])
    }
}

#[cfg(test)]
mod tests {
    use IntComparison::*;

    use super::*;
    use crate::IntComparison;

    #[test]
    fn a_resolution_block_is_written_with_its_declarations() {
        let mut buffer = Vec::new();
        let mut writer = BlockProofWriter::new(&mut buffer);

        writer
            .log_block(Block {
                constraint_id: NonZero::new(3).unwrap(),
                nogood: vec![IntAtomic::new("x1", GreaterEqual, 1)],
                inferences: vec![
                    BlockInference {
                        nogood: vec![IntAtomic::new("x2", GreaterEqual, 4)],
                        constraint: Some(NonZero::new(2).unwrap()),
                        label: "linear_bounds",
                        hint: vec![],
                    },
                    BlockInference {
                        nogood: vec![
                            IntAtomic::new("x1", GreaterEqual, 1),
                            IntAtomic::new("x2", LessEqual, 3),
                        ],
                        constraint: Some(NonZero::new(1).unwrap()),
                        label: "linear_bounds",
                        hint: vec![],
                    },
                ],
            })
            .unwrap();

        writer.log_conclusion::<&str>(Conclusion::Unsat).unwrap();

        let proof_string = String::from_utf8(buffer).expect("valid utf8");
        let expected = r#"
a 1 [x2 >= 4]
a 2 [x1 >= 1]
l 1 linear_bounds
r 1 c:2 l:1
  2 -1 c:1 l:1
e 3 2
c UNSAT
"#;

        assert_eq!(expected.trim(), proof_string.trim());
    }

    #[test]
    fn labels_are_declared_once_and_reused() {
        let mut buffer = Vec::new();
        let mut writer = BlockProofWriter::new(&mut buffer);

        for id in 1..=2_u32 {
            writer
                .log_block(Block {
                    constraint_id: NonZero::new(id + 10).unwrap(),
                    nogood: vec![],
                    inferences: vec![BlockInference {
                        nogood: vec![IntAtomic::new("x", GreaterEqual, 1)],
                        constraint: Some(NonZero::new(1).unwrap()),
                        label: "linear_bounds",
                        hint: vec![],
                    }],
                })
                .unwrap();
        }

        let proof = String::from_utf8(buffer).unwrap();
        assert_eq!(proof.matches("l 1 linear_bounds").count(), 1);
        assert_eq!(proof.matches("a 1 [x >= 1]").count(), 1);
    }

    #[test]
    fn the_binary_form_has_records_for_atomics_and_continuation_lines() {
        let mut buffer = Vec::new();
        let mut writer = BlockProofWriter::new_binary(&mut buffer).unwrap();

        writer
            .log_block(Block {
                constraint_id: NonZero::new(3).unwrap(),
                nogood: vec![],
                inferences: vec![
                    BlockInference {
                        nogood: vec![IntAtomic::new("x", GreaterEqual, -2)],
                        constraint: Some(NonZero::new(1).unwrap()),
                        label: "linear_bounds",
                        hint: vec![],
                    },
                    BlockInference {
                        nogood: vec![!IntAtomic::new("x", GreaterEqual, -2)],
                        constraint: None,
                        label: "initial_domain",
                        hint: vec![4],
                    },
                ],
            })
            .unwrap();

        let mut expected = BINARY_MARK.to_vec();
        // The name x, then [x >= -2] as atomic constraint 1 over name 1.
        expected.extend([1, 1, b'x']);
        expected.extend([2, 1, 1, 1, 3]);
        expected.extend([0, 17]);
        expected.extend(b"l 1 linear_bounds");
        expected.extend([0, 18]);
        expected.extend(b"l 2 initial_domain");
        expected.extend([0, 11]);
        expected.extend(b"r 1 c:1 l:1");
        // One reference, -1, cited constraint 0, label 2, one hint position, 4.
        expected.extend([3, 1, 1, 0, 2, 1, 4]);
        expected.extend([0, 3]);
        expected.extend(b"e 3");
        assert_eq!(buffer, expected);
    }

    #[test]
    fn the_initial_domains_are_constraint_zero() {
        let mut buffer = Vec::new();
        let mut writer = BlockProofWriter::new(&mut buffer);

        writer
            .log_block(Block {
                constraint_id: NonZero::new(7).unwrap(),
                nogood: vec![IntAtomic::new("x", GreaterEqual, 1)],
                inferences: vec![BlockInference {
                    nogood: vec![!IntAtomic::new("x", GreaterEqual, 1)],
                    constraint: None,
                    label: "initial_domain",
                    hint: vec![],
                }],
            })
            .unwrap();

        let proof = String::from_utf8(buffer).unwrap();
        assert!(proof.contains("c:0"), "{proof}");
    }
}
