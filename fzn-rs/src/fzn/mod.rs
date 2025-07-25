use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;

use chumsky::error::Rich;
use chumsky::extra;
use chumsky::input::Input;
use chumsky::input::MapExtra;
use chumsky::input::ValueInput;
use chumsky::prelude::any;
use chumsky::prelude::choice;
use chumsky::prelude::just;
use chumsky::prelude::recursive;
use chumsky::select;
use chumsky::span::SimpleSpan;
use chumsky::IterParser;
use chumsky::Parser;

use crate::ast;

mod tokens;

pub use tokens::Token;
use tokens::Token::*;

#[derive(Clone, Debug, Default)]
struct ParseState {
    /// The identifiers encountered so far.
    strings: BTreeSet<Rc<str>>,
    /// Parameters
    parameters: BTreeMap<Rc<str>, ParameterValue>,
}

impl ParseState {
    fn get_interned(&mut self, string: &str) -> Rc<str> {
        if !self.strings.contains(string) {
            let _ = self.strings.insert(Rc::from(string));
        }

        Rc::clone(self.strings.get(string).unwrap())
    }

    fn resolve_literal(&self, literal: ast::Literal) -> ast::Literal {
        match literal {
            ast::Literal::Identifier(ident) => self
                .parameters
                .get(&ident)
                .map(|value| match value {
                    ParameterValue::Bool(boolean) => ast::Literal::Bool(*boolean),
                    ParameterValue::Int(int) => ast::Literal::Int(*int),
                    ParameterValue::IntSet(set) => ast::Literal::IntSet(set.clone()),
                })
                .unwrap_or(ast::Literal::Identifier(ident)),

            lit @ (ast::Literal::Int(_) | ast::Literal::Bool(_) | ast::Literal::IntSet(_)) => lit,
        }
    }
}

#[derive(Clone, Debug)]
enum ParameterValue {
    Bool(bool),
    Int(i64),
    IntSet(ast::RangeList<i64>),
}

#[derive(Debug, thiserror::Error)]
pub enum FznError<'src> {
    #[error("failed to lex fzn")]
    LexError {
        reasons: Vec<Rich<'src, char, SimpleSpan>>,
    },

    #[error("failed to parse fzn")]
    ParseError {
        reasons: Vec<Rich<'src, Token<'src>, ast::Span>>,
    },
}

pub fn parse(source: &str) -> Result<ast::Ast, FznError<'_>> {
    let mut state = extra::SimpleState(ParseState::default());

    let tokens = tokens::lex()
        .parse(source)
        .into_result()
        .map_err(|reasons| FznError::LexError { reasons })?;

    let parser_input = tokens.map(
        ast::Span {
            start: source.len(),
            end: source.len(),
        },
        |node| (&node.node, &node.span),
    );

    let ast = predicates()
        .ignore_then(parameters())
        .ignore_then(arrays())
        .then(variables())
        .then(arrays())
        .then(constraints())
        .then(solve_item())
        .map(
            |((((parameter_arrays, variables), variable_arrays), constraints), solve)| {
                let mut arrays = parameter_arrays;
                arrays.extend(variable_arrays);

                ast::Ast {
                    variables,
                    arrays,
                    constraints,
                    solve,
                }
            },
        )
        .parse_with_state(parser_input, &mut state)
        .into_result()
        .map_err(
            |reasons: Vec<Rich<'_, Token<'_>, _>>| FznError::ParseError {
                reasons: reasons
                    .into_iter()
                    .map(|error| error.into_owned())
                    .collect(),
            },
        )?;

    Ok(ast)
}

/// The extra data attached to the chumsky parsers.
///
/// We specify a rich error type, as well as an instance of [`ParseState`] for string interning and
/// parameter resolution.
type FznExtra<'tokens, 'src> =
    extra::Full<Rich<'tokens, Token<'src>, ast::Span>, extra::SimpleState<ParseState>, ()>;

fn predicates<'tokens, 'src: 'tokens, I>() -> impl Parser<'tokens, I, (), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    predicate().repeated().collect::<Vec<_>>().ignored()
}

fn predicate<'tokens, 'src: 'tokens, I>() -> impl Parser<'tokens, I, (), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    just(Ident("predicate"))
        .ignore_then(any().and_is(just(SemiColon).not()).repeated())
        .then(just(SemiColon))
        .ignored()
}

fn parameters<'tokens, 'src: 'tokens, I>() -> impl Parser<'tokens, I, (), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    parameter().repeated().collect::<Vec<_>>().ignored()
}

fn parameter_type<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        just(Ident("int")),
        just(Ident("bool")),
        just(Ident("set"))
            .then_ignore(just(Ident("of")))
            .then_ignore(just(Ident("int"))),
    ))
    .ignored()
}

fn parameter<'tokens, 'src: 'tokens, I>() -> impl Parser<'tokens, I, (), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    parameter_type()
        .ignore_then(just(Colon))
        .ignore_then(identifier())
        .then_ignore(just(Equal))
        .then(literal())
        .then_ignore(just(SemiColon))
        .try_map_with(|(name, value), extra| {
            let state = extra.state();

            let value = match value.node {
                ast::Literal::Int(int) => ParameterValue::Int(int),
                ast::Literal::Bool(boolean) => ParameterValue::Bool(boolean),
                ast::Literal::IntSet(set) => ParameterValue::IntSet(set),
                ast::Literal::Identifier(identifier) => {
                    return Err(Rich::custom(
                        value.span,
                        format!("parameter '{identifier}' is undefined"),
                    ))
                }
            };

            let _ = state.parameters.insert(name, value);

            Ok(())
        })
}

fn arrays<'tokens, 'src: 'tokens, I>() -> impl Parser<
    'tokens,
    I,
    BTreeMap<Rc<str>, ast::Node<ast::Array<ast::Annotation>>>,
    FznExtra<'tokens, 'src>,
>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    array()
        .repeated()
        .collect::<Vec<_>>()
        .map(|arrays| arrays.into_iter().collect())
}

fn array<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (Rc<str>, ast::Node<ast::Array<ast::Annotation>>), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    just(Ident("array"))
        .ignore_then(interval_set(integer()).delimited_by(just(OpenBracket), just(CloseBracket)))
        .ignore_then(just(Ident("of")))
        .ignore_then(just(Ident("var")).or_not())
        .ignore_then(domain())
        .ignore_then(just(Colon))
        .ignore_then(identifier())
        .then(annotations())
        .then_ignore(just(Equal))
        .then(
            literal()
                .separated_by(just(Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(OpenBracket), just(CloseBracket)),
        )
        .then_ignore(just(SemiColon))
        .map_with(|((name, annotations), contents), extra| {
            (
                name,
                ast::Node {
                    node: ast::Array {
                        contents,
                        annotations,
                    },
                    span: extra.span(),
                },
            )
        })
}

fn variables<'tokens, 'src: 'tokens, I>() -> impl Parser<
    'tokens,
    I,
    BTreeMap<Rc<str>, ast::Node<ast::Variable<ast::Annotation>>>,
    FznExtra<'tokens, 'src>,
>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    variable()
        .repeated()
        .collect::<Vec<_>>()
        .map(|variables| variables.into_iter().collect())
}

fn variable<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (Rc<str>, ast::Node<ast::Variable<ast::Annotation>>), FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    just(Ident("var"))
        .ignore_then(domain())
        .then_ignore(just(Colon))
        .then(identifier())
        .then(annotations())
        .then(just(Equal).ignore_then(literal()).or_not())
        .then_ignore(just(SemiColon))
        .map_with(to_node)
        .map(|node| {
            let ast::Node {
                node: (((domain, name), annotations), value),
                span,
            } = node;

            let variable = ast::Variable {
                domain,
                value,
                annotations,
            };

            (
                name,
                ast::Node {
                    node: variable,
                    span,
                },
            )
        })
}

fn domain<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::Node<ast::Domain>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        just(Ident("int")).to(ast::Domain::UnboundedInt),
        just(Ident("bool")).to(ast::Domain::Bool),
        set_of(integer()).map(ast::Domain::Int),
    ))
    .map_with(to_node)
}

fn constraints<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<ast::Node<ast::Constraint>>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    constraint().repeated().collect::<Vec<_>>()
}

fn constraint<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::Node<ast::Constraint>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    just(Ident("constraint"))
        .ignore_then(identifier().map_with(to_node))
        .then(
            argument()
                .separated_by(just(Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(OpenParen), just(CloseParen)),
        )
        .then(annotations())
        .then_ignore(just(SemiColon))
        .map(|((name, arguments), annotations)| ast::Constraint {
            name,
            arguments,
            annotations,
        })
        .map_with(to_node)
}

fn argument<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::Node<ast::Argument>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        literal().map(ast::Argument::Literal),
        literal()
            .separated_by(just(Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(OpenBracket), just(CloseBracket))
            .map(ast::Argument::Array),
    ))
    .map_with(to_node)
}

fn solve_item<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::SolveItem<ast::Annotation>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    just(Ident("solve"))
        .ignore_then(annotations())
        .then(solve_method())
        .then_ignore(just(SemiColon))
        .map(|(annotations, method)| ast::SolveItem {
            method,
            annotations,
        })
}

fn solve_method<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::Node<ast::Method>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        just(Ident("satisfy")).to(ast::Method::Satisfy),
        just(Ident("minimize"))
            .ignore_then(identifier())
            .map(|ident| ast::Method::Optimize {
                direction: ast::OptimizationDirection::Minimize,
                objective: ast::Literal::Identifier(ident),
            }),
        just(Ident("maximize"))
            .ignore_then(identifier())
            .map(|ident| ast::Method::Optimize {
                direction: ast::OptimizationDirection::Maximize,
                objective: ast::Literal::Identifier(ident),
            }),
    ))
    .map_with(to_node)
}

fn annotations<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<ast::Node<ast::Annotation>>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    annotation().repeated().collect()
}

fn annotation<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::Node<ast::Annotation>, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    just(DoubleColon)
        .ignore_then(choice((
            annotation_call().map(ast::Annotation::Call),
            identifier().map(ast::Annotation::Atom),
        )))
        .map_with(to_node)
}

fn annotation_call<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::AnnotationCall, FznExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    recursive(|call| {
        identifier()
            .then(
                annotation_argument(call)
                    .separated_by(just(Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(OpenParen), just(CloseParen)),
            )
            .map(|(name, arguments)| ast::AnnotationCall { name, arguments })
    })
}

fn annotation_argument<'tokens, 'src: 'tokens, I>(
    call_parser: impl Parser<'tokens, I, ast::AnnotationCall, FznExtra<'tokens, 'src>> + Clone,
) -> impl Parser<'tokens, I, ast::Node<ast::AnnotationArgument>, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        annotation_literal(call_parser.clone()).map(ast::AnnotationArgument::Literal),
        annotation_literal(call_parser)
            .separated_by(just(Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(OpenBracket), just(CloseBracket))
            .map(ast::AnnotationArgument::Array),
    ))
    .map_with(to_node)
}

fn annotation_literal<'tokens, 'src: 'tokens, I>(
    call_parser: impl Parser<'tokens, I, ast::AnnotationCall, FznExtra<'tokens, 'src>> + Clone,
) -> impl Parser<'tokens, I, ast::Node<ast::AnnotationLiteral>, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        call_parser
            .map(ast::AnnotationLiteral::Annotation)
            .map_with(to_node),
        literal().map(|node| ast::Node {
            node: ast::AnnotationLiteral::BaseLiteral(node.node),
            span: node.span,
        }),
    ))
}

fn to_node<'tokens, 'src: 'tokens, I, T>(
    node: T,
    extra: &mut MapExtra<'tokens, '_, I, FznExtra<'tokens, 'src>>,
) -> ast::Node<T>
where
    I: Input<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    ast::Node {
        node,
        span: extra.span(),
    }
}

fn literal<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ast::Node<ast::Literal>, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    choice((
        set_of(integer()).map(ast::Literal::IntSet),
        integer().map(ast::Literal::Int),
        boolean().map(ast::Literal::Bool),
        identifier().map(ast::Literal::Identifier),
    ))
    .map_with(|literal, extra| {
        let state = extra.state();
        state.resolve_literal(literal)
    })
    .map_with(to_node)
}

fn set_of<'tokens, 'src: 'tokens, I, T: Copy + Ord>(
    value_parser: impl Parser<'tokens, I, T, FznExtra<'tokens, 'src>> + Clone,
) -> impl Parser<'tokens, I, ast::RangeList<T>, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
    ast::RangeList<T>: FromIterator<T>,
{
    let sparse_set = value_parser
        .clone()
        .separated_by(just(Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(OpenBrace), just(CloseBrace))
        .map(ast::RangeList::from_iter);

    choice((
        sparse_set,
        interval_set(value_parser).map(|(lb, ub)| ast::RangeList::from(lb..=ub)),
    ))
}

fn interval_set<'tokens, 'src: 'tokens, I, T: Copy + Ord>(
    value_parser: impl Parser<'tokens, I, T, FznExtra<'tokens, 'src>> + Clone,
) -> impl Parser<'tokens, I, (T, T), FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    value_parser
        .clone()
        .then_ignore(just(DoublePeriod))
        .then(value_parser)
}

fn integer<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, i64, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    select! {
        Integer(int) => int,
    }
}

fn boolean<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, bool, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    select! {
        Boolean(boolean) => boolean,
    }
}

fn identifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Rc<str>, FznExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Span = ast::Span, Token = Token<'src>>,
{
    select! {
        Ident(ident) => ident,
    }
    .map_with(|ident, extra| {
        let state: &mut extra::SimpleState<ParseState> = extra.state();
        state.get_interned(ident)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! btreemap {
        ($($key:expr => $value:expr,)+) => (btreemap!($($key => $value),+));

        ( $($key:expr => $value:expr),* ) => {
            {
                let mut _map = ::std::collections::BTreeMap::new();
                $(
                    let _ = _map.insert($key, $value);
                )*
                _map
            }
        };
    }

    #[test]
    fn empty_satisfaction_model() {
        let source = r#"
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(15, 22, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn predicate_statements_are_ignored() {
        let source = r#"
        predicate some_predicate(int: xs, var int: ys);
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(71, 78, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn empty_minimization_model() {
        let source = r#"
        solve minimize objective;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(
                        15,
                        33,
                        ast::Method::Optimize {
                            direction: ast::OptimizationDirection::Minimize,
                            objective: ast::Literal::Identifier("objective".into()),
                        }
                    ),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn empty_maximization_model() {
        let source = r#"
        solve maximize objective;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(
                        15,
                        33,
                        ast::Method::Optimize {
                            direction: ast::OptimizationDirection::Maximize,
                            objective: ast::Literal::Identifier("objective".into()),
                        }
                    ),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn variables() {
        let source = r#"
        var 1..5: x_interval;
        var bool: x_bool;
        var {1, 3, 5}: x_sparse;
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: btreemap! {
                    "x_interval".into() => node(9, 30, ast::Variable {
                        domain: node(13, 17, ast::Domain::Int(ast::RangeList::from(1..=5))),
                        value: None,
                        annotations: vec![]
                    }),
                    "x_bool".into() => node(39, 56, ast::Variable {
                        domain: node(43, 47, ast::Domain::Bool),
                        value: None,
                        annotations: vec![]
                    }),
                    "x_sparse".into() => node(65, 89, ast::Variable {
                        domain: node(69, 78, ast::Domain::Int(ast::RangeList::from_iter([1, 3, 5]))),
                        value: None,
                        annotations: vec![]
                    }),
                },
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(104, 111, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn variable_with_assignment() {
        let source = r#"
        var 5..5: x1 = 5;
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: btreemap! {
                    "x1".into() => node(9, 26, ast::Variable {
                        domain: node(13, 17, ast::Domain::Int(ast::RangeList::from(5..=5))),
                        value: Some(node(24, 25, ast::Literal::Int(5))),
                        annotations: vec![]
                    }),
                },
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(41, 48, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn variable_with_assignment_to_named_constant() {
        let source = r#"
        int: y = 5;
        var 5..5: x1 = y;
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: btreemap! {
                    "x1".into() => node(29, 46, ast::Variable {
                        domain: node(33, 37, ast::Domain::Int(ast::RangeList::from(5..=5))),
                        value: Some(node(44, 45, ast::Literal::Int(5))),
                        annotations: vec![]
                    }),
                },
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(61, 68, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn arrays_of_constants_and_variables() {
        let source = r#"
        int: p = 5;
        array [1..3] of int: ys = [1, 3, p];

        var int: some_var;
        array [1..2] of var int: vars = [1, some_var];

        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: btreemap! {
                    "some_var".into() => node(75, 93, ast::Variable {
                        domain: node(79, 82, ast::Domain::UnboundedInt),
                        value: None,
                        annotations: vec![]
                    }),
                },
                arrays: btreemap! {
                    "ys".into() => node(29, 65, ast::Array {
                        contents: vec![
                            node(56, 57, ast::Literal::Int(1)),
                            node(59, 60, ast::Literal::Int(3)),
                            node(62, 63, ast::Literal::Int(5)),
                        ],
                        annotations: vec![],
                    }),
                    "vars".into() => node(102, 148, ast::Array {
                        contents: vec![
                            node(135, 136, ast::Literal::Int(1)),
                            node(138, 146, ast::Literal::Identifier("some_var".into())),
                        ],
                        annotations: vec![],
                    }),
                },
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(164, 171, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn constraint_item() {
        let source = r#"
        constraint int_lin_le(weights, [x1, x2, 3], 3);
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![node(
                    9,
                    56,
                    ast::Constraint {
                        name: node(20, 30, "int_lin_le".into()),
                        arguments: vec![
                            node(
                                31,
                                38,
                                ast::Argument::Literal(node(
                                    31,
                                    38,
                                    ast::Literal::Identifier("weights".into())
                                ))
                            ),
                            node(
                                40,
                                51,
                                ast::Argument::Array(vec![
                                    node(41, 43, ast::Literal::Identifier("x1".into())),
                                    node(45, 47, ast::Literal::Identifier("x2".into())),
                                    node(49, 50, ast::Literal::Int(3)),
                                ]),
                            ),
                            node(
                                53,
                                54,
                                ast::Argument::Literal(node(53, 54, ast::Literal::Int(3)))
                            ),
                        ],
                        annotations: vec![],
                    }
                )],
                solve: ast::SolveItem {
                    method: node(71, 78, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn annotations_on_variables() {
        let source = r#"
        var 5..5: x1 :: output_var = 5;
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: btreemap! {
                    "x1".into() => node(9, 40, ast::Variable {
                        domain: node(13, 17, ast::Domain::Int(ast::RangeList::from(5..=5))),
                        value: Some(node(38, 39, ast::Literal::Int(5))),
                        annotations: vec![node(22, 35, ast::Annotation::Atom("output_var".into()))],
                    }),
                },
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(55, 62, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn annotations_on_arrays() {
        let source = r#"
        array [1..2] of var 1..10: xs :: output_array([1..2]) = [];
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: btreemap! {
                    "xs".into() => node(9, 68, ast::Array {
                        contents: vec![],
                        annotations: vec![
                            node(39, 62, ast::Annotation::Call(ast::AnnotationCall {
                                name: "output_array".into(),
                                arguments: vec![node(55, 61,
                                    ast::AnnotationArgument::Array(vec![node(56, 60,
                                        ast::AnnotationLiteral::BaseLiteral(
                                            ast::Literal::IntSet(
                                                ast::RangeList::from(1..=2)
                                            )
                                        )
                                    )])
                                )],
                            })),
                        ],
                    }),
                },
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(83, 90, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn annotations_on_constraints() {
        let source = r#"
        constraint predicate() :: defines_var(x1);
        solve satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![node(
                    9,
                    51,
                    ast::Constraint {
                        name: node(20, 29, "predicate".into()),
                        arguments: vec![],
                        annotations: vec![node(
                            32,
                            50,
                            ast::Annotation::Call(ast::AnnotationCall {
                                name: "defines_var".into(),
                                arguments: vec![node(
                                    47,
                                    49,
                                    ast::AnnotationArgument::Literal(node(
                                        47,
                                        49,
                                        ast::AnnotationLiteral::BaseLiteral(
                                            ast::Literal::Identifier("x1".into())
                                        )
                                    ))
                                )]
                            })
                        )],
                    }
                )],
                solve: ast::SolveItem {
                    method: node(66, 73, ast::Method::Satisfy),
                    annotations: vec![],
                }
            }
        );
    }

    #[test]
    fn annotations_on_solve_item() {
        let source = r#"
        solve :: int_search(first_fail(xs), indomain_min) satisfy;
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(59, 66, ast::Method::Satisfy),
                    annotations: vec![node(
                        15,
                        58,
                        ast::Annotation::Call(ast::AnnotationCall {
                            name: "int_search".into(),
                            arguments: vec![
                                node(
                                    29,
                                    43,
                                    ast::AnnotationArgument::Literal(node(
                                        29,
                                        43,
                                        ast::AnnotationLiteral::Annotation(ast::AnnotationCall {
                                            name: "first_fail".into(),
                                            arguments: vec![node(
                                                40,
                                                42,
                                                ast::AnnotationArgument::Literal(node(
                                                    40,
                                                    42,
                                                    ast::AnnotationLiteral::BaseLiteral(
                                                        ast::Literal::Identifier("xs".into())
                                                    )
                                                ))
                                            )]
                                        })
                                    ))
                                ),
                                node(
                                    45,
                                    57,
                                    ast::AnnotationArgument::Literal(node(
                                        45,
                                        57,
                                        ast::AnnotationLiteral::BaseLiteral(
                                            ast::Literal::Identifier("indomain_min".into())
                                        )
                                    ))
                                ),
                            ]
                        })
                    )],
                }
            }
        );
    }

    fn node<T>(start: usize, end: usize, data: T) -> ast::Node<T> {
        ast::Node {
            node: data,
            span: ast::Span { start, end },
        }
    }
}
