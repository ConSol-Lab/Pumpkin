//! Implements the parsing of DRCP proof files.
//!
//! See [`ProofReader`] for information on how to parse a DRCP file.

mod error;
mod literal_atomic_map;

use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::num::NonZero;

pub use error::DrcpError;
pub use literal_atomic_map::LiteralAtomicMap;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::combinator::all_consuming;
use nom::combinator::map;
use nom::combinator::map_opt;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::combinator::value;
use nom::multi::many0_count;
use nom::multi::separated_list0;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;

use crate::steps::Conclusion;
use crate::steps::Deletion;
use crate::steps::Inference;
use crate::steps::Nogood;
use crate::steps::Step;
use crate::steps::StepId;

/// The output of [`ProofReader::next_step`].
pub type ReadStep<'a, AtomicConstraint> =
    Step<'a, Vec<AtomicConstraint>, AtomicConstraint, Vec<StepId>>;

/// Used to read and parse DRCP proofs.
///
/// The reader will read the proof line-by-line, using the fact that DRCP is a line-based format.
/// Any leading or trailing whitespace, and empty lines, will be ignored. Literals that are
/// encountered are mapped to atomics through an implementation of [`LiteralAtomicMap`]. It is
/// assumed this is a total mapping, i.e. all literals encountered in the proof can be mapped to an
/// atomic constraint.
///
/// Note that the reader does not perform any type of validity checking of the proof. It will
/// happily parse any garbage that is within the format specification. For example, a valid DRCP
/// proof will contain exactly one conclusion step, which is the last step of the proof. This
/// reader does not validate this, and obtaining a conclusion step does not mean the next
/// invocation to [`ProofReader::next_step`] will return [`None`].
///
/// # Example
/// ```
/// use std::num::NonZero;
///
/// use drcp_format::reader::ProofReader;
/// use drcp_format::steps::*;
///
/// let source = r#"
/// i 1 4 5 0 -2 c:20 l:linear_bound
/// n 2 1 -3 5 0 1
/// d 2
/// c UNSAT
/// "#;
///
/// let mut reader = ProofReader::new(source.as_bytes(), std::convert::identity);
/// let lit = |num: i32| NonZero::new(num).unwrap();
/// let step_id = |num: u64| NonZero::new(num).unwrap();
///
/// let inference_step = reader.next_step().expect("valid drcp inference step");
/// let expected_inference = Inference {
///     id: step_id(1),
///     hint_constraint_id: Some(NonZero::new(20).unwrap()),
///     hint_label: Some("linear_bound"),
///     premises: vec![lit(4), lit(5)],
///     propagated: Some(lit(-2)),
/// };
/// assert_eq!(Some(Step::Inference(expected_inference)), inference_step);
///
/// let nogood_step = reader.next_step().expect("valid drcp nogood step");
/// let expected_nogood = Nogood {
///     id: step_id(2),
///     hints: Some(vec![step_id(1)]),
///     literals: vec![lit(1), lit(-3), lit(5)],
/// };
/// assert_eq!(Some(Step::Nogood(expected_nogood)), nogood_step);
///
/// let deletion_step = reader.next_step().expect("valid drcp deletion step");
/// let expected_deletion = Deletion { id: step_id(2) };
/// assert_eq!(Some(Step::Delete(expected_deletion)), deletion_step);
///
/// let conclusion_step = reader.next_step().expect("valid drcp conclusion step");
/// assert_eq!(
///     Some(Step::Conclusion(Conclusion::Unsatisfiable)),
///     conclusion_step
/// );
///
/// let end = reader.next_step().expect("finished reading source");
/// assert_eq!(None, end);
/// ```
#[derive(Debug)]
pub struct ProofReader<R, AtomicConstraints> {
    source: BufReader<R>,
    string_buffer: String,
    atomics: AtomicConstraints,
}

impl<R: Read, AtomicConstraints> ProofReader<R, AtomicConstraints> {
    /// Construct a new proof reader which reads from `source`.
    ///
    /// The `atomics` are used to map the proof literals to atomic constraints. This is likely
    /// based on a parsed `.lits` file, but that does not have to be the case.
    pub fn new(source: R, atomics: AtomicConstraints) -> ProofReader<R, AtomicConstraints> {
        ProofReader {
            source: BufReader::new(source),
            string_buffer: String::new(),
            atomics,
        }
    }
}

impl<R, AtomicConstraints> ProofReader<R, AtomicConstraints>
where
    R: Read,
    AtomicConstraints: LiteralAtomicMap,
{
    /// Read the next step in the proof.
    ///
    /// If reading is successful, `Some(step)` is returned with the next step, or `None` if the end
    /// of the proof is reached. On an error, be it from IO or other, the `Err` variant is
    /// returned.
    pub fn next_step(
        &mut self,
    ) -> Result<Option<ReadStep<'_, AtomicConstraints::Atomic>>, DrcpError> {
        self.string_buffer.clear();

        // Read lines until we find a non-empty line. The contents of `line` will be trimmed.
        while self.string_buffer.trim().is_empty() {
            let read_bytes = self.source.read_line(&mut self.string_buffer)?;

            if read_bytes == 0 {
                // The end of the file has been reached.
                return Ok(None);
            }
        }

        let (_, step) = proof_step(self.string_buffer.trim())?;

        let step = self.map_step(step);

        Ok(Some(step))
    }

    /// Map the literals to the atomic constraints in the proof step.
    fn map_step<'s>(
        &self,
        step: ReadStep<'s, NonZero<i32>>,
    ) -> ReadStep<'s, AtomicConstraints::Atomic> {
        match step {
            Step::Inference(Inference {
                id,
                hint_constraint_id,
                hint_label,
                premises,
                propagated,
            }) => Step::Inference(Inference {
                id,
                hint_constraint_id,
                hint_label,
                premises: premises
                    .into_iter()
                    .map(|literal| self.atomics.to_atomic(literal))
                    .collect(),
                propagated: propagated.map(|p| self.atomics.to_atomic(p)),
            }),

            Step::Nogood(Nogood {
                id,
                literals,
                hints,
            }) => Step::Nogood(Nogood {
                id,
                literals: literals
                    .into_iter()
                    .map(|literal| self.atomics.to_atomic(literal))
                    .collect(),
                hints,
            }),

            // Here we cannot just forward the input value, as it has a different type due to the
            // generics on `Step`.
            Step::Delete(Deletion { id }) => Step::Delete(Deletion { id }),

            Step::Conclusion(Conclusion::Unsatisfiable) => {
                Step::Conclusion(Conclusion::Unsatisfiable)
            }

            Step::Conclusion(Conclusion::Optimal(literal)) => {
                Step::Conclusion(Conclusion::Optimal(self.atomics.to_atomic(literal)))
            }
        }
    }
}

/// Parse a proof step from a line.
///
/// `input` is assumed to be a single line, with leading and trailing whitespace removed. If this
/// is not the case, the parser will fail.
fn proof_step(input: &str) -> IResult<&str, ReadStep<'_, NonZero<i32>>> {
    all_consuming(alt((
        map(inference_step, Step::Inference),
        map(nogood_step, Step::Nogood),
        map(deletion_step, Step::Delete),
        map(conclusion_step, Step::Conclusion),
    )))(input)
}

/// `i <step_id> <premises> 0 <propagated> [c:<constraint tag>] [l:<filtering algorithm>]`
fn inference_step(input: &str) -> IResult<&str, Inference<'_, Vec<NonZero<i32>>, NonZero<i32>>> {
    map(
        tuple((
            tag("i "),
            step_id,
            tag(" "),
            literal_list,
            opt(preceded(tag(" 0 "), literal)),
            opt(preceded(tag(" c:"), constraint_id)),
            opt(preceded(tag(" l:"), identifier)),
        )),
        |(_, id, _, premises, propagated, hint_constraint_id, hint_label)| Inference {
            id,
            hint_constraint_id,
            hint_label,
            premises,
            propagated,
        },
    )(input)
}

/// `n <step_id> <atomic constraint ids> [0 <propagation hint>]`
fn nogood_step(input: &str) -> IResult<&str, Nogood<Vec<NonZero<i32>>, Vec<StepId>>> {
    map(
        tuple((
            tag("n "),
            step_id,
            tag(" "),
            literal_list,
            opt(preceded(
                // Hack! If `literal_list` is empty, then the space will be parsed already.
                alt((tag("0 "), tag(" 0 "))),
                separated_list0(tag(" "), step_id),
            )),
        )),
        |(_, id, _, literals, hints)| Nogood {
            id,
            hints,
            literals,
        },
    )(input)
}

/// `d <step_id>`
fn deletion_step(input: &str) -> IResult<&str, Deletion> {
    preceded(tag("d "), map(step_id, |id| Deletion { id }))(input)
}

/// `c UNSAT` or `c <objective bound literal>`
fn conclusion_step(input: &str) -> IResult<&str, Conclusion<NonZero<i32>>> {
    preceded(
        tag("c "),
        alt((
            value(Conclusion::Unsatisfiable, tag("UNSAT")),
            map(literal, Conclusion::Optimal),
        )),
    )(input)
}

/// Parses a space-separated list of non-zero signed integers.
fn literal_list(input: &str) -> IResult<&str, Vec<NonZero<i32>>> {
    separated_list0(tag(" "), literal)(input)
}

/// Parses a single non-zero signed integer.
fn literal(input: &str) -> IResult<&str, NonZero<i32>> {
    map_opt(nom::character::complete::i32, NonZero::new)(input)
}

/// Parses a single unsigned non-zero 64-bit integer.
fn step_id(input: &str) -> IResult<&str, NonZero<u64>> {
    map_opt(nom::character::complete::u64, NonZero::new)(input)
}

/// Parses a single unsigned non-zero 32-bit integer.
fn constraint_id(input: &str) -> IResult<&str, NonZero<u32>> {
    map_opt(nom::character::complete::u32, NonZero::new)(input)
}

/// Parses identifiers according to the minizinc spec: `[A-Za-z_][A-Za-z0-9_]*`
fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inference_nogood_without_hints() {
        let source = "i 1 4 5\n";
        let mut reader = ProofReader::new(source.as_bytes(), std::convert::identity);

        let inference_step = reader.next_step().expect("valid drcp inference step");
        let expected_inference = Inference {
            id: NonZero::new(1).unwrap(),
            hint_constraint_id: None,
            hint_label: None,
            premises: vec![NonZero::new(4).unwrap(), NonZero::new(5).unwrap()],
            propagated: None,
        };
        assert_eq!(Some(Step::Inference(expected_inference)), inference_step);
    }

    #[test]
    fn inference_nogood_with_constraint_tag_and_label() {
        let source = "i 1 4 5 c:20 l:linear_bound\n";
        let mut reader = ProofReader::new(source.as_bytes(), std::convert::identity);

        let inference_step = reader.next_step().expect("valid drcp inference step");
        let expected_inference = Inference {
            id: NonZero::new(1).unwrap(),
            hint_constraint_id: Some(NonZero::new(20).unwrap()),
            hint_label: Some("linear_bound"),
            premises: vec![NonZero::new(4).unwrap(), NonZero::new(5).unwrap()],
            propagated: None,
        };
        assert_eq!(Some(Step::Inference(expected_inference)), inference_step);
    }

    #[test]
    fn empty_nogood_with_hints() {
        let source = "n 100 0 1 4 5\n";
        let mut reader = ProofReader::new(source.as_bytes(), std::convert::identity);

        let nogood_step = reader.next_step().expect("valid drcp nogood step");
        let expected_nogood = Nogood {
            id: NonZero::new(100).unwrap(),
            literals: vec![],
            hints: Some(vec![
                NonZero::new(1).unwrap(),
                NonZero::new(4).unwrap(),
                NonZero::new(5).unwrap(),
            ]),
        };
        assert_eq!(Some(Step::Nogood(expected_nogood)), nogood_step);
    }
}
