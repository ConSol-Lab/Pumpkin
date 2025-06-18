use std::collections::BTreeMap;
use std::io::BufRead;
use std::io::{self};
use std::num::NonZero;
use std::rc::Rc;
use std::str::FromStr;

use chumsky::error::Rich;
use chumsky::extra;
use chumsky::prelude::choice;
use chumsky::prelude::just;
use chumsky::text::ident;
use chumsky::text::int;
use chumsky::IterParser;
use chumsky::Parser;

use crate::ConstraintId;
use crate::Inference;
use crate::IntAtomic;
use crate::IntComparison;
use crate::IntComparison::*;
use crate::IntValue;
use crate::Step;

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

    #[error("failed to parse proof line {line}: {reason}")]
    ParseError { line: usize, reason: String },

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
                    assert_eq!(errs.len(), 1);

                    let reason = format!("{}", errs[0]);
                    Error::ParseError {
                        line: self.line_nr,
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
            }
        }
    }
}

fn parser<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    choice((atom_definition(), inference()))
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
    just("i")
        .ignore_then(id())
        .then(atom_codes())
        .then(atom_code().or_not())
        .then(just("c:").ignore_then(id()).or_not())
        .then(just("l:").ignore_then(ident()).or_not())
        .map(
            |((((constraint_id, premises), consequent), generated_by), label)| {
                ProofLine::Inference {
                    constraint_id,
                    premises,
                    consequent,
                    generated_by,
                    label,
                }
            },
        )
}

fn atom_codes<'src>(
) -> impl Parser<'src, &'src str, Vec<NonZero<i32>>, extra::Err<Rich<'src, char>>> {
    atom_code().repeated().collect().then_ignore(just("0"))
}

fn atom_code<'src>() -> impl Parser<'src, &'src str, NonZero<i32>, extra::Err<Rich<'src, char>>> {
    just("-")
        .or_not()
        .then(int(10))
        .try_map(|(negation, code): (Option<&'src str>, &'src str), span| {
            let code = code
                .parse::<i32>()
                .map_err(|_| Rich::custom(span, "failed to parse atomic code"))?;

            let code = if negation.is_some() { -code } else { code };

            NonZero::new(code).ok_or_else(|| Rich::custom(span, "atomics cannot have id 0"))
        })
        .padded()
}

fn id<'src>() -> impl Parser<'src, &'src str, NonZero<u32>, extra::Err<Rich<'src, char>>> {
    int(10)
        .try_map(|code: &'src str, span| {
            let code = code
                .parse::<u32>()
                .map_err(|_| Rich::custom(span, "failed to parse atomic id"))?;

            NonZero::new(code).ok_or_else(|| Rich::custom(span, "atomics cannot have id 0"))
        })
        .padded()
}

fn atomic<'src, Int>(
) -> impl Parser<'src, &'src str, IntAtomic<&'src str, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    ident()
        .then(comparison())
        .then(value())
        .delimited_by(just("["), just("]"))
        .padded()
        .map(|((name, comparison), value)| IntAtomic {
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
            comparison: GreaterEqual,
            value: 0,
        };

        let expected_inference = Inference {
            constraint_id: NonZero::new(2).unwrap(),
            premises: vec![a1, !a2],
            consequent: None,
            generated_by: Some(NonZero::new(1).unwrap()),
            label: Some("inf_name".into()),
        };

        assert_eq!(Step::Inference(expected_inference), inference);
    }
}
