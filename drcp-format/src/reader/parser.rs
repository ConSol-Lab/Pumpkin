use std::num::NonZero;
use std::str::FromStr;

use chumsky::error::Rich;
use chumsky::extra;
use chumsky::prelude::choice;
use chumsky::prelude::group;
use chumsky::prelude::just;
use chumsky::text::ident;
use chumsky::text::int;
use chumsky::IterParser;
use chumsky::Parser;

use crate::ConstraintId;
use crate::IntAtomic;
use crate::IntComparison;
use crate::IntComparison::*;

pub(super) enum ProofLine<'src, Int> {
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
    Conclusion(ProofLineConclusion),
}

pub(super) enum ProofLineConclusion {
    Unsat,
    DualBound(NonZero<i32>),
}

pub(super) fn proof_line<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    choice((atom_definition(), inference(), deduction(), conclusion()))
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

fn conclusion<'src, Int>(
) -> impl Parser<'src, &'src str, ProofLine<'src, Int>, extra::Err<Rich<'src, char>>>
where
    Int: FromStr,
{
    just("c")
        .ignore_then(choice((
            just("UNSAT").padded().map(|_| ProofLineConclusion::Unsat),
            atom_code().map(ProofLineConclusion::DualBound),
        )))
        .map(ProofLine::Conclusion)
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
    just("-")
        .or_not()
        .then(int(10))
        .to_slice()
        .try_map(|code, span| {
            Int::from_str(code).map_err(|_| Rich::custom(span, "failed to parse domain value"))
        })
        .padded()
}
