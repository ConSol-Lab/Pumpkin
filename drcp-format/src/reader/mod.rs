use std::collections::BTreeMap;
use std::io::BufRead;
use std::io::{self};
use std::num::NonZero;
use std::rc::Rc;
use std::str::FromStr;

use chumsky::error::Rich;
use chumsky::extra;
use chumsky::prelude::just;
use chumsky::prelude::{choice, group};
use chumsky::text::ident;
use chumsky::text::int;
use chumsky::IterParser;
use chumsky::Parser;

use crate::Inference;
use crate::IntAtomic;
use crate::IntComparison;
use crate::IntComparison::*;
use crate::IntValue;
use crate::Step;
use crate::{ConstraintId, Deduction};

#[derive(Debug)]
pub struct ProofReader<Source, Int> {
    source: Source,

    atomics: BTreeMap<NonZero<u32>, IntAtomic<Rc<str>, Int>>,

    line_buffer: String,
    line_nr: usize,
}

impl<Source, Int> ProofReader<Source, Int> {
    pub fn new(source: Source) -> Self {
        ProofReader {
            source,
            atomics: BTreeMap::default(),
            line_buffer: String::new(),
            line_nr: 0,
        }
    }

    fn map_atomic_ids(
        &self,
        ids: Vec<NonZero<i32>>,
    ) -> Result<Vec<IntAtomic<Rc<str>, Int>>, NonZero<i32>>
    where
        Int: IntValue,
    {
        ids.into_iter()
            .map(|code| self.map_atomic_id(code))
            .collect()
    }

    fn map_atomic_id(&self, id: NonZero<i32>) -> Result<IntAtomic<Rc<str>, Int>, NonZero<i32>>
    where
        Int: IntValue,
    {
        self.atomics
            .get(&id.unsigned_abs())
            .cloned()
            .map(|atomic| if id.is_negative() { !atomic } else { atomic })
            .ok_or(id)
    }
}

pub type ReadAtomic<Int> = IntAtomic<Rc<str>, Int>;
pub type ReadStep<Int> = Step<Rc<str>, Int, Rc<str>>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to read from source: {0}")]
    IoError(#[from] io::Error),

    #[error("failed to parse proof line {line} {span:?}: {reason}")]
    ParseError {
        line: usize,
        reason: String,
        span: (usize, usize),
    },

    #[error("undefined atomic {code} on line {line}")]
    UndefinedAtomic { line: usize, code: NonZero<i32> },
}

impl<Source, Int> ProofReader<Source, Int>
where
    Source: BufRead,
    Int: IntValue,
{
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

            let proof_line: ProofLine<'_, Int> =
                parser().parse(trimmed_line).into_result().map_err(|errs| {
                    assert_eq!(
                        errs.len(),
                        1,
                        "since we do no recovery, any error will terminate the parsing immediately"
                    );

                    let reason = format!("{}", errs[0]);
                    let parse_span = errs[0].span();

                    Error::ParseError {
                        line: self.line_nr,
                        span: (parse_span.start, parse_span.end),
                        reason,
                    }
                })?;

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
                        premises: self.map_atomic_ids(premises).map_err(|code| {
                            Error::UndefinedAtomic {
                                line: self.line_nr,
                                code,
                            }
                        })?,
                        consequent: consequent
                            .map(|c| self.map_atomic_id(c))
                            .transpose()
                            .map_err(|code| Error::UndefinedAtomic {
                                line: self.line_nr,
                                code,
                            })?,
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
                        premises: self.map_atomic_ids(premises).map_err(|code| {
                            Error::UndefinedAtomic {
                                line: self.line_nr,
                                code,
                            }
                        })?,
                        sequence,
                    };

                    return Ok(Some(Step::Deduction(deduction)));
                }
            }
        }
    }
}

fn parser<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    choice((atom_definition(), inference(), deduction()))
}

fn atom_definition<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    just("a")
        .ignore_then(id())
        .then(atomic())
        .map(|(id, atomic)| ProofLine::AtomDefinition(id, atomic))
}

fn inference<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    group((
        just("i"),
        id(),
        atom_codes(),
        atom_code().or_not(),
        just("c:").ignore_then(id()).padded().or_not(),
        just("l:").ignore_then(ident()).padded().or_not(),
    ))
    .map(
        |(_, constraint_id, premises, consequent, generated_by, label)| ProofLine::Inference {
            constraint_id,
            premises,
            consequent,
            generated_by,
            label,
        },
    )
}

fn deduction<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    group((just("n"), id(), atom_codes(), id().repeated().collect())).map(
        |(_, constraint_id, premises, sequence)| ProofLine::Deduction {
            constraint_id,
            premises,
            sequence,
        },
    )
}

fn atom_codes<'src>(
) -> impl Parser<'src, &'src str, Vec<NonZero<i32>>, extra::Err<Rich<'src, char>>> {
    atom_code()
        .repeated()
        .collect()
        .then_ignore(just("0").padded())
}

fn atom_code<'src>() -> impl Parser<'src, &'src str, NonZero<i32>, extra::Err<Rich<'src, char>>> {
    just("-")
        .or_not()
        .then(int(10))
        .to_slice()
        .try_map(|code: &'src str, span| {
            code.parse::<NonZero<i32>>()
                .map_err(|_| Rich::custom(span, "failed to parse atomic code"))
        })
        .labelled("atom code")
        .padded()
}

fn id<'src>() -> impl Parser<'src, &'src str, NonZero<u32>, extra::Err<Rich<'src, char>>> {
    int(10)
        .try_map(|code: &'src str, span| {
            code.parse::<NonZero<u32>>()
                .map_err(|_| Rich::custom(span, "failed to parse atomic id"))
        })
        .labelled("constraint/atomic id")
        .padded()
}

fn atomic<'src, Int>(
) -> impl Parser<'src, &'src str, IntAtomic<&'src str, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    group((ident(), comparison(), value()))
        .delimited_by(just("["), just("]"))
        .padded()
        .map(|(name, comparison, value)| IntAtomic {
            name,
            comparison,
            value,
        })
}

fn comparison<'src>() -> impl Parser<'src, &'src str, IntComparison, extra::Err<Rich<'src, char>>> {
    choice((
        just(">=").to(GreaterEqual),
        just("<=").to(LessEqual),
        just("==").to(Equal),
        just("!=").to(NotEqual),
    ))
    .padded()
}

fn value<'src, Int>() -> impl Parser<'src, &'src str, Int, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    int(10)
        .try_map(|code, span| {
            Int::from_str(code).map_err(|_| Rich::custom(span, "failed to parse domain value"))
        })
        .padded()
}

enum ProofLine<'src, Int> {
    AtomDefinition(NonZero<u32>, IntAtomic<&'src str, Int>),
    Inference {
        constraint_id: ConstraintId,
        premises: Vec<NonZero<i32>>,
        consequent: Option<NonZero<i32>>,
        generated_by: Option<ConstraintId>,
        label: Option<&'src str>,
    },
    Deduction {
        constraint_id: ConstraintId,
        premises: Vec<NonZero<i32>>,
        sequence: Vec<ConstraintId>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inference_with_consequent() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            i 2 1 0 2 c:1 l:inf_name
        "#;

        let mut reader = ProofReader::<_, i32>::new(source.as_bytes());

        let inference = reader
            .next_step()
            .expect("no error reading")
            .expect("there is one proof step");

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

        let expected_inference = Inference {
            constraint_id: NonZero::new(2).unwrap(),
            premises: vec![a1],
            consequent: Some(a2),
            generated_by: Some(NonZero::new(1).unwrap()),
            label: Some("inf_name".into()),
        };

        assert_eq!(Step::Inference(expected_inference), inference);
    }

    #[test]
    fn inference_without_consequent() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            i 2 1 -2 0 c:1 l:inf_name
        "#;

        let mut reader = ProofReader::<_, i32>::new(source.as_bytes());

        let inference = reader
            .next_step()
            .expect("no error reading")
            .expect("there is one proof step");

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

        let expected_inference = Inference {
            constraint_id: NonZero::new(2).unwrap(),
            premises: vec![a1, a2],
            consequent: None,
            generated_by: Some(NonZero::new(1).unwrap()),
            label: Some("inf_name".into()),
        };

        assert_eq!(Step::Inference(expected_inference), inference);
    }

    #[test]
    fn deduction_without_inferences() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            n 2 1 -2 0
        "#;

        let mut reader = ProofReader::<_, i32>::new(source.as_bytes());

        let deduction = reader
            .next_step()
            .expect("no error reading")
            .expect("there is one proof step");

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

        let expected_deduction = Deduction {
            constraint_id: NonZero::new(2).unwrap(),
            premises: vec![a1, a2],
            sequence: vec![],
        };

        assert_eq!(Step::Deduction(expected_deduction), deduction);
    }

    #[test]
    fn deduction_with_inferences() {
        let source = r#"
            a 1 [x1 >= 0]
            a 2 [x2 >= 0]
            n 5 1 -2 0 1 3 2 4
        "#;

        let mut reader = ProofReader::<_, i32>::new(source.as_bytes());

        let deduction = reader
            .next_step()
            .expect("no error reading")
            .expect("there is one proof step");

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

        let expected_deduction = Deduction {
            constraint_id: NonZero::new(5).unwrap(),
            premises: vec![a1, a2],
            sequence: vec![
                NonZero::new(1).unwrap(),
                NonZero::new(3).unwrap(),
                NonZero::new(2).unwrap(),
                NonZero::new(4).unwrap(),
            ],
        };

        assert_eq!(Step::Deduction(expected_deduction), deduction);
    }
}
