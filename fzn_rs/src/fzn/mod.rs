use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;

use chumsky::error::Rich;
use chumsky::extra::{self};
use chumsky::prelude::choice;
use chumsky::prelude::just;
use chumsky::IterParser;
use chumsky::Parser;

use crate::ast::{self};

mod tokens;

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
#[error("failed to parse fzn")]
pub struct ParseError<'src> {
    reasons: Vec<Rich<'src, char>>,
}

pub fn parse(source: &str) -> Result<ast::Ast, ParseError<'_>> {
    let mut state = extra::SimpleState(ParseState::default());

    parameters()
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
        .parse_with_state(source, &mut state)
        .into_result()
        .map_err(|reasons| ParseError { reasons })
}

type FznExtra<'src> = extra::Full<Rich<'src, char>, extra::SimpleState<ParseState>, ()>;

fn parameters<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
    parameter().repeated().collect::<Vec<_>>().ignored()
}

fn parameter_type<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
    choice((
        just("int"),
        just("bool"),
        just("of").delimited_by(
            just("set").then(tokens::ws(1)),
            tokens::ws(1).then(just("int")),
        ),
    ))
    .ignored()
}

fn parameter<'src>() -> impl Parser<'src, &'src str, (), FznExtra<'src>> {
    parameter_type()
        .ignore_then(just(":").padded())
        .ignore_then(tokens::identifier())
        .then_ignore(tokens::equal())
        .then(tokens::literal())
        .then_ignore(tokens::ws(0).then(just(";")))
        .try_map_with(|(name, value), extra| {
            let state: &mut extra::SimpleState<ParseState> = extra.state();

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
        .padded()
}

fn arrays<'src>(
) -> impl Parser<'src, &'src str, BTreeMap<Rc<str>, ast::Node<ast::Array>>, FznExtra<'src>> {
    array()
        .repeated()
        .collect::<Vec<_>>()
        .map(|arrays| arrays.into_iter().collect())
}

fn array<'src>() -> impl Parser<'src, &'src str, (Rc<str>, ast::Node<ast::Array>), FznExtra<'src>> {
    just("array")
        .ignore_then(
            tokens::interval_set().delimited_by(tokens::open_bracket(), tokens::close_bracket()),
        )
        .ignore_then(just("of"))
        .ignore_then(tokens::ws(1))
        .ignore_then(just("var").then(tokens::ws(1)).or_not())
        .ignore_then(domain())
        .ignore_then(tokens::colon())
        .ignore_then(tokens::identifier())
        .then_ignore(tokens::equal())
        .then(
            tokens::literal()
                .separated_by(tokens::comma())
                .collect::<Vec<_>>()
                .delimited_by(tokens::open_bracket(), tokens::close_bracket()),
        )
        .then_ignore(tokens::ws(0).then(just(";")))
        .map_with(|(name, contents), extra| {
            (
                name,
                ast::Node {
                    node: ast::Array {
                        contents,
                        annotations: vec![],
                    },
                    span: extra.span().into(),
                },
            )
        })
        .padded()
}

fn variables<'src>() -> impl Parser<
    'src,
    &'src str,
    BTreeMap<Rc<str>, ast::Node<ast::Variable<ast::Annotation>>>,
    FznExtra<'src>,
> {
    variable()
        .repeated()
        .collect::<Vec<_>>()
        .map(|variables| variables.into_iter().collect())
}

fn variable<'src>(
) -> impl Parser<'src, &'src str, (Rc<str>, ast::Node<ast::Variable<ast::Annotation>>), FznExtra<'src>>
{
    just("var")
        .ignore_then(tokens::ws(1))
        .ignore_then(domain())
        .then_ignore(tokens::colon())
        .then(tokens::identifier())
        .then(tokens::equal().ignore_then(tokens::literal()).or_not())
        .then_ignore(just(";"))
        .map_with(tokens::to_node)
        .map(|node| {
            let ast::Node {
                node: ((domain, name), value),
                span,
            } = node;

            let variable = ast::Variable {
                domain,
                value,
                annotations: vec![],
            };

            (
                name,
                ast::Node {
                    node: variable,
                    span,
                },
            )
        })
        .padded()
}

fn domain<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Domain>, FznExtra<'src>> {
    choice((
        just("int").to(ast::Domain::UnboundedInt),
        just("bool").to(ast::Domain::Bool),
        tokens::int_set_literal().map(ast::Domain::Int),
    ))
    .map_with(tokens::to_node)
}

fn constraints<'src>(
) -> impl Parser<'src, &'src str, Vec<ast::Node<ast::Constraint>>, FznExtra<'src>> {
    constraint().repeated().collect::<Vec<_>>()
}

fn constraint<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Constraint>, FznExtra<'src>> {
    just("constraint")
        .ignore_then(tokens::ws(1))
        .ignore_then(tokens::identifier().map_with(tokens::to_node))
        .then(
            argument()
                .separated_by(tokens::comma())
                .collect::<Vec<_>>()
                .delimited_by(tokens::open_paren(), tokens::close_paren()),
        )
        .then_ignore(just(";"))
        .map(|(name, arguments)| ast::Constraint {
            name,
            arguments,
            annotations: vec![],
        })
        .map_with(tokens::to_node)
        .padded()
}

fn argument<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Argument>, FznExtra<'src>> {
    choice((
        tokens::literal().map(ast::Argument::Literal),
        tokens::literal()
            .separated_by(tokens::comma())
            .collect::<Vec<_>>()
            .delimited_by(tokens::open_bracket(), tokens::close_bracket())
            .map(ast::Argument::Array),
    ))
    .map_with(tokens::to_node)
}

fn solve_item<'src>() -> impl Parser<'src, &'src str, ast::SolveObjective, FznExtra<'src>> {
    just("solve")
        .ignore_then(tokens::ws(1))
        .ignore_then(solve_method())
        .then_ignore(tokens::ws(0).then(just(";")))
        .map(|method| ast::SolveObjective {
            method,
            annotations: vec![],
        })
        .padded()
}

fn solve_method<'src>() -> impl Parser<'src, &'src str, ast::Node<ast::Method>, FznExtra<'src>> {
    choice((
        just("satisfy").to(ast::Method::Satisfy),
        just("minimize")
            .ignore_then(tokens::ws(1))
            .ignore_then(tokens::identifier())
            .map(|ident| ast::Method::Optimize {
                direction: ast::OptimizationDirection::Minimize,
                objective: ident,
            }),
        just("maximize")
            .ignore_then(tokens::ws(1))
            .ignore_then(tokens::identifier())
            .map(|ident| ast::Method::Optimize {
                direction: ast::OptimizationDirection::Maximize,
                objective: ident,
            }),
    ))
    .map_with(tokens::to_node)
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
                solve: ast::SolveObjective {
                    method: node(15, 22, ast::Method::Satisfy),
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
                solve: ast::SolveObjective {
                    method: node(
                        15,
                        33,
                        ast::Method::Optimize {
                            direction: ast::OptimizationDirection::Minimize,
                            objective: "objective".into(),
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
                solve: ast::SolveObjective {
                    method: node(
                        15,
                        33,
                        ast::Method::Optimize {
                            direction: ast::OptimizationDirection::Maximize,
                            objective: "objective".into(),
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
                solve: ast::SolveObjective {
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
                solve: ast::SolveObjective {
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
                solve: ast::SolveObjective {
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
                solve: ast::SolveObjective {
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
                solve: ast::SolveObjective {
                    method: node(71, 78, ast::Method::Satisfy),
                    annotations: vec![],
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
