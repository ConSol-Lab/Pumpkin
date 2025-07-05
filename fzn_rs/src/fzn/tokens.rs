use std::rc::Rc;

use chumsky::extra::{self};
use chumsky::input::MapExtra;
use chumsky::prelude::choice;
use chumsky::prelude::just;
use chumsky::text::ascii::ident;
use chumsky::text::int;
use chumsky::text::whitespace;
use chumsky::IterParser;
use chumsky::Parser;

use super::FznExtra;
use super::ParseState;
use crate::ast::{self};

pub(super) fn to_node<'src, T>(
    node: T,
    extra: &mut MapExtra<'src, '_, &'src str, FznExtra<'src>>,
) -> ast::Node<T> {
    let span: chumsky::prelude::SimpleSpan = extra.span();

    ast::Node {
        node,
        span: span.into(),
    }
}

pub(super) fn literal<'src>(
) -> impl Parser<'src, &'src str, ast::Node<ast::Literal>, FznExtra<'src>> {
    choice((
        int_set_literal().map(ast::Literal::IntSet),
        int_literal().map(ast::Literal::Int),
        bool_literal().map(ast::Literal::Bool),
        identifier().map(ast::Literal::Identifier),
    ))
    .map_with(|literal, extra| {
        let state: &mut extra::SimpleState<ParseState> = extra.state();
        state.resolve_literal(literal)
    })
    .map_with(to_node)
}

fn int_literal<'src>() -> impl Parser<'src, &'src str, i64, FznExtra<'src>> {
    just("-")
        .or_not()
        .ignore_then(int(10))
        .to_slice()
        .map(|slice: &str| slice.parse().unwrap())
}

fn bool_literal<'src>() -> impl Parser<'src, &'src str, bool, FznExtra<'src>> {
    choice((just("true").to(true), just("false").to(false)))
}

pub(super) fn int_set_literal<'src>(
) -> impl Parser<'src, &'src str, ast::RangeList<i64>, FznExtra<'src>> {
    choice((interval_set(), sparse_set()))
}

pub(super) fn interval_set<'src>(
) -> impl Parser<'src, &'src str, ast::RangeList<i64>, FznExtra<'src>> {
    int_literal()
        .then_ignore(just(".."))
        .then(int_literal())
        .map(|(lower_bound, upper_bound)| ast::RangeList::from(lower_bound..=upper_bound))
}

fn sparse_set<'src>() -> impl Parser<'src, &'src str, ast::RangeList<i64>, FznExtra<'src>> {
    int_literal()
        .separated_by(just(",").padded())
        .collect::<Vec<_>>()
        .delimited_by(just("{").padded(), just("}").padded())
        .map(ast::RangeList::from_iter)
}

pub(super) fn identifier<'src>() -> impl Parser<'src, &'src str, Rc<str>, FznExtra<'src>> {
    ident().map_with(|id, extra| {
        let state: &mut extra::SimpleState<ParseState> = extra.state();

        state.get_interned(id)
    })
}

macro_rules! punctuation {
    ($name:ident, $seq:expr) => {
        pub(super) fn $name<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
            just($seq).padded().ignored()
        }
    };
}

punctuation!(equal, "=");
punctuation!(comma, ",");
punctuation!(colon, ":");
punctuation!(open_bracket, "[");
punctuation!(close_bracket, "]");
punctuation!(open_paren, "(");
punctuation!(close_paren, ")");

pub(super) fn ws<'src>(minimum: usize) -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
    whitespace().at_least(minimum)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse<'src, T>(
        parser: impl Parser<'src, &'src str, T, FznExtra<'src>>,
        source: &'src str,
    ) -> T {
        let mut state = extra::SimpleState(ParseState::default());
        parser.parse_with_state(source, &mut state).unwrap()
    }

    #[test]
    fn int_literal() {
        assert_eq!(node(0, 2, ast::Literal::Int(23)), parse(literal(), "23"));
        assert_eq!(node(0, 3, ast::Literal::Int(-20)), parse(literal(), "-20"));
    }

    #[test]
    fn bool_literal() {
        assert_eq!(
            node(0, 4, ast::Literal::Bool(true)),
            parse(literal(), "true")
        );

        assert_eq!(
            node(0, 5, ast::Literal::Bool(false)),
            parse(literal(), "false")
        );
    }

    #[test]
    fn identifier_literal() {
        assert_eq!(
            node(0, 2, ast::Literal::Identifier(Rc::from("x1"))),
            parse(literal(), "x1")
        );

        assert_eq!(
            node(0, 15, ast::Literal::Identifier(Rc::from("X_INTRODUCED_9_"))),
            parse(literal(), "X_INTRODUCED_9_")
        );
    }

    #[test]
    fn set_literal() {
        assert_eq!(
            node(
                0,
                9,
                ast::Literal::IntSet(ast::RangeList::from_iter([1, 3, 5]))
            ),
            parse(literal(), "{1, 3, 5}")
        );

        assert_eq!(
            node(0, 6, ast::Literal::IntSet(ast::RangeList::from(-5..=-2))),
            parse(literal(), "-5..-2")
        );
    }

    fn node<T>(start: usize, end: usize, data: T) -> ast::Node<T> {
        ast::Node {
            node: data,
            span: ast::Span { start, end },
        }
    }
}
