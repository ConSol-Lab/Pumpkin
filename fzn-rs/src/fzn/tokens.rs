use std::fmt::Display;

use chumsky::error::Rich;
use chumsky::extra::{self};
use chumsky::prelude::any;
use chumsky::prelude::choice;
use chumsky::prelude::just;
use chumsky::text::ascii::ident;
use chumsky::text::int;
use chumsky::IterParser;
use chumsky::Parser;

use crate::ast;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'src> {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Comma,
    Colon,
    DoubleColon,
    SemiColon,
    DoublePeriod,
    Equal,
    Ident(&'src str),
    Integer(i64),
    Boolean(bool),
}

type LexExtra<'src> = extra::Err<Rich<'src, char>>;

pub(super) fn lex<'src>(
) -> impl Parser<'src, &'src str, Vec<ast::Node<Token<'src>>>, LexExtra<'src>> {
    token()
        .padded_by(comment().repeated())
        .padded()
        .repeated()
        .collect()
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char>>> {
    just("%")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .ignored()
}

fn token<'src>(
) -> impl Parser<'src, &'src str, ast::Node<Token<'src>>, extra::Err<Rich<'src, char>>> {
    choice((
        // Punctuation
        just(";").to(Token::SemiColon),
        just("::").to(Token::DoubleColon),
        just(":").to(Token::Colon),
        just(",").to(Token::Comma),
        just("..").to(Token::DoublePeriod),
        just("[").to(Token::OpenBracket),
        just("]").to(Token::CloseBracket),
        just("{").to(Token::OpenBrace),
        just("}").to(Token::CloseBrace),
        just("(").to(Token::OpenParen),
        just(")").to(Token::CloseParen),
        just("=").to(Token::Equal),
        // Values
        just("true").to(Token::Boolean(true)),
        just("false").to(Token::Boolean(false)),
        int_literal().map(Token::Integer),
        // Identifiers (including keywords)
        ident().map(Token::Ident),
    ))
    .map_with(|token, extra| {
        let span: chumsky::prelude::SimpleSpan = extra.span();

        ast::Node {
            node: token,
            span: span.into(),
        }
    })
}

fn int_literal<'src>() -> impl Parser<'src, &'src str, i64, LexExtra<'src>> {
    just("-")
        .or_not()
        .ignore_then(int(10))
        .to_slice()
        .map(|slice: &str| slice.parse().unwrap())
}
