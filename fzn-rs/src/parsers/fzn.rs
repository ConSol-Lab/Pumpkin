use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;

use chumsky::error::Rich;
use chumsky::extra;
use chumsky::input::MapExtra;
use chumsky::prelude::any;
use chumsky::prelude::choice;
use chumsky::prelude::just;
use chumsky::prelude::recursive;
use chumsky::text::int;
use chumsky::IterParser;
use chumsky::Parser;

use crate::ast;

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
#[error("failed to parse flatzinc")]
pub struct FznError<'src> {
    reasons: Vec<Rich<'src, char>>,
}

pub fn parse(source: &str) -> Result<ast::Ast, FznError<'_>> {
    let mut state = extra::SimpleState(ParseState::default());

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
        .padded()
        .parse_with_state(source, &mut state)
        .into_result()
        .map_err(|reasons: Vec<Rich<'_, char, _>>| FznError {
            reasons: reasons
                .into_iter()
                .map(|error| error.into_owned())
                .collect(),
        })?;

    Ok(ast)
}

/// The extra data attached to the chumsky parsers.
///
/// We specify a rich error type, as well as an instance of [`ParseState`] for string interning and
/// parameter resolution.
type FznExtra<'src> = extra::Full<Rich<'src, char>, extra::SimpleState<ParseState>, ()>;

fn predicates<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
    predicate().repeated().collect::<Vec<_>>().ignored()
}

fn token<'src>(token: &'static str) -> impl Parser<'src, &'src str, (), FznExtra<'src>> + Clone {
    just(token)
        .padded_by(comment().repeated())
        .padded()
        .ignored()
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> + Clone {
    just("%")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .ignored()
}

fn predicate<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
    token("predicate")
        .ignore_then(any().and_is(token(";").not()).repeated())
        .then(token(";"))
        .ignored()
}

fn parameters<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>>
where
{
    parameter().repeated().collect::<Vec<_>>().ignored()
}

fn parameter_type<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>>
where
{
    choice((
        token("int"),
        token("bool"),
        token("set")
            .then_ignore(token("of"))
            .then_ignore(token("int")),
    ))
    .ignored()
}

fn parameter<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>>
where
{
    parameter_type()
        .ignore_then(token(":"))
        .ignore_then(identifier())
        .then_ignore(token("="))
        .then(literal())
        .then_ignore(token(";"))
        .try_map_with(|(name, value), extra| {
            let state = extra.state();

            let value = match value.node {
                ast::Literal::Int(int) => ParameterValue::Int(int),
                ast::Literal::Bool(boolean) => ParameterValue::Bool(boolean),
                ast::Literal::IntSet(set) => ParameterValue::IntSet(set),
                ast::Literal::Identifier(identifier) => {
                    return Err(Rich::custom(
                        value.span.into(),
                        format!("parameter '{identifier}' is undefined"),
                    ))
                }
            };

            let _ = state.parameters.insert(name, value);

            Ok(())
        })
}

fn arrays<'src>() -> impl Parser<
    'src,
    &'src str,
    BTreeMap<Rc<str>, ast::Node<ast::Array<ast::Annotation>>>,
    FznExtra<'src>,
>
where
{
    array()
        .repeated()
        .collect::<Vec<_>>()
        .map(|arrays| arrays.into_iter().collect())
}

fn array<'src>(
) -> impl Parser<'src, &'src str, (Rc<str>, ast::Node<ast::Array<ast::Annotation>>), FznExtra<'src>>
where
{
    token("array")
        .ignore_then(interval_set(integer()).delimited_by(token("["), token("]")))
        .ignore_then(token("of"))
        .ignore_then(token("var").or_not())
        .ignore_then(domain())
        .then_ignore(token(":"))
        .then(identifier())
        .then(annotations())
        .then_ignore(token("="))
        .then(
            literal()
                .separated_by(token(","))
                .collect::<Vec<_>>()
                .delimited_by(token("["), token("]")),
        )
        .then_ignore(token(";"))
        .map_with(|(((domain, name), annotations), contents), extra| {
            (
                name,
                ast::Node {
                    node: ast::Array {
                        domain,
                        contents,
                        annotations,
                    },
                    span: extra.span().into(),
                },
            )
        })
}

fn variables<'src>() -> impl Parser<
    'src,
    &'src str,
    BTreeMap<Rc<str>, ast::Node<ast::Variable<ast::Annotation>>>,
    FznExtra<'src>,
>
where
{
    variable()
        .repeated()
        .collect::<Vec<_>>()
        .map(|variables| variables.into_iter().collect())
}

fn variable<'src>(
) -> impl Parser<'src, &'src str, (Rc<str>, ast::Node<ast::Variable<ast::Annotation>>), FznExtra<'src>>
where
{
    token("var")
        .ignore_then(domain())
        .then_ignore(token(":"))
        .then(identifier())
        .then(annotations())
        .then(token("=").ignore_then(literal()).or_not())
        .then_ignore(token(";"))
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

fn domain<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Domain>, FznExtra<'src>>
where
{
    choice((
        token("int").to(ast::Domain::UnboundedInt),
        token("bool").to(ast::Domain::Bool),
        set_of(integer()).map(ast::Domain::Int),
    ))
    .map_with(to_node)
}

fn constraints<'src>(
) -> impl Parser<'src, &'src str, Vec<ast::Node<ast::Constraint>>, FznExtra<'src>>
where
{
    constraint().repeated().collect::<Vec<_>>()
}

fn constraint<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Constraint>, FznExtra<'src>>
where
{
    token("constraint")
        .ignore_then(identifier().map_with(to_node))
        .then(
            argument()
                .separated_by(token(","))
                .collect::<Vec<_>>()
                .delimited_by(token("("), token(")")),
        )
        .then(annotations())
        .then_ignore(token(";"))
        .map(|((name, arguments), annotations)| ast::Constraint {
            name,
            arguments,
            annotations,
        })
        .map_with(to_node)
}

fn argument<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Argument>, FznExtra<'src>>
where
{
    choice((
        literal().map(ast::Argument::Literal),
        literal()
            .separated_by(token(","))
            .collect::<Vec<_>>()
            .delimited_by(token("["), token("]"))
            .map(ast::Argument::Array),
    ))
    .map_with(to_node)
}

fn solve_item<'src>(
) -> impl Parser<'src, &'src str, ast::SolveItem<ast::Annotation>, FznExtra<'src>>
where
{
    token("solve")
        .ignore_then(annotations())
        .then(solve_method())
        .then_ignore(token(";"))
        .map(|(annotations, method)| ast::SolveItem {
            method,
            annotations,
        })
}

fn solve_method<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Method>, FznExtra<'src>>
where
{
    choice((
        token("satisfy").to(ast::Method::Satisfy),
        token("minimize")
            .ignore_then(identifier())
            .map(|ident| ast::Method::Optimize {
                direction: ast::OptimizationDirection::Minimize,
                objective: ast::Literal::Identifier(ident),
            }),
        token("maximize")
            .ignore_then(identifier())
            .map(|ident| ast::Method::Optimize {
                direction: ast::OptimizationDirection::Maximize,
                objective: ast::Literal::Identifier(ident),
            }),
    ))
    .map_with(to_node)
}

fn annotations<'src>(
) -> impl Parser<'src, &'src str, Vec<ast::Node<ast::Annotation>>, FznExtra<'src>>
where
{
    annotation().repeated().collect()
}

fn annotation<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Annotation>, FznExtra<'src>>
where
{
    token("::")
        .ignore_then(choice((
            annotation_call().map(ast::Annotation::Call),
            identifier().map(ast::Annotation::Atom),
        )))
        .map_with(to_node)
}

fn annotation_call<'src>() -> impl Parser<'src, &'src str, ast::AnnotationCall, FznExtra<'src>>
where
{
    recursive(|call| {
        identifier()
            .then(
                annotation_argument(call)
                    .separated_by(token(","))
                    .collect::<Vec<_>>()
                    .delimited_by(token("("), token(")")),
            )
            .map(|(name, arguments)| ast::AnnotationCall { name, arguments })
    })
}

fn annotation_argument<'src>(
    call_parser: impl Parser<'src, &'src str, ast::AnnotationCall, FznExtra<'src>> + Clone,
) -> impl Parser<'src, &'src str, ast::Node<ast::AnnotationArgument>, FznExtra<'src>> + Clone
where
{
    choice((
        annotation_literal(call_parser.clone()).map(ast::AnnotationArgument::Literal),
        annotation_literal(call_parser)
            .separated_by(token(","))
            .collect::<Vec<_>>()
            .delimited_by(token("["), token("]"))
            .map(ast::AnnotationArgument::Array),
    ))
    .map_with(to_node)
}

fn annotation_literal<'src>(
    call_parser: impl Parser<'src, &'src str, ast::AnnotationCall, FznExtra<'src>> + Clone,
) -> impl Parser<'src, &'src str, ast::Node<ast::AnnotationLiteral>, FznExtra<'src>> + Clone
where
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

fn to_node<'src, T>(
    node: T,
    extra: &mut MapExtra<'src, '_, &'src str, FznExtra<'src>>,
) -> ast::Node<T>
where
{
    ast::Node {
        node,
        span: extra.span().into(),
    }
}

fn literal<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Literal>, FznExtra<'src>> + Clone
where
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

fn set_of<'src, T: Copy + Ord>(
    value_parser: impl Parser<'src, &'src str, T, FznExtra<'src>> + Clone,
) -> impl Parser<'src, &'src str, ast::RangeList<T>, FznExtra<'src>> + Clone
where
    ast::RangeList<T>: FromIterator<T>,
{
    let sparse_set = value_parser
        .clone()
        .separated_by(token(","))
        .collect::<Vec<_>>()
        .delimited_by(token("{"), token("}"))
        .map(ast::RangeList::from_iter);

    choice((
        sparse_set,
        interval_set(value_parser).map(|(lb, ub)| ast::RangeList::from(lb..=ub)),
    ))
}

fn interval_set<'src, T: Copy + Ord>(
    value_parser: impl Parser<'src, &'src str, T, FznExtra<'src>> + Clone,
) -> impl Parser<'src, &'src str, (T, T), FznExtra<'src>> + Clone
where
{
    value_parser
        .clone()
        .then_ignore(token(".."))
        .then(value_parser)
}

fn integer<'src>() -> impl Parser<'src, &'src str, i64, FznExtra<'src>> + Clone
where
{
    just("-")
        .or_not()
        .ignore_then(int(10))
        .to_slice()
        .map(|slice: &str| slice.parse().unwrap())
}

fn boolean<'src>() -> impl Parser<'src, &'src str, bool, FznExtra<'src>> + Clone
where
{
    choice((token("true").to(true), token("false").to(false)))
}

fn identifier<'src>() -> impl Parser<'src, &'src str, Rc<str>, FznExtra<'src>> + Clone
where
{
    chumsky::text::ident().map_with(|ident, extra| {
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
    fn comments_are_ignored() {
        let source = r#"
        % Generated by MiniZinc 2.9.2
        % Solver: bla
        solve satisfy; % This is ignored
        "#;

        let ast = parse(source).expect("valid fzn");

        assert_eq!(
            ast,
            ast::Ast {
                variables: BTreeMap::default(),
                arrays: BTreeMap::default(),
                constraints: vec![],
                solve: ast::SolveItem {
                    method: node(75, 82, ast::Method::Satisfy),
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
                        domain: node(45, 48, ast::Domain::UnboundedInt),
                        contents: vec![
                            node(56, 57, ast::Literal::Int(1)),
                            node(59, 60, ast::Literal::Int(3)),
                            node(62, 63, ast::Literal::Int(5)),
                        ],
                        annotations: vec![],
                    }),
                    "vars".into() => node(102, 148, ast::Array {
                        domain: node(122, 125, ast::Domain::UnboundedInt),
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
                        domain: node(29, 34, ast::Domain::Int(ast::RangeList::from(1..=10))),
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
