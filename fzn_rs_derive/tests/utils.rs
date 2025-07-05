#![allow(
    dead_code,
    reason = "it is used in other test files, but somehow compiler can't see it"
)]

use std::collections::BTreeMap;
use std::rc::Rc;

use fzn_rs::ast::{self};

pub(crate) fn satisfy_solve() -> ast::SolveObjective {
    ast::SolveObjective {
        method: test_node(ast::Method::Satisfy),
        annotations: vec![],
    }
}

pub(crate) fn test_node<T>(node: T) -> ast::Node<T> {
    ast::Node {
        node,
        span: ast::Span { start: 0, end: 0 },
    }
}

pub(crate) fn unbounded_variables<'a>(
    names: impl IntoIterator<Item = &'a str>,
) -> BTreeMap<Rc<str>, ast::Node<ast::Variable<ast::Annotation>>> {
    names
        .into_iter()
        .map(|name| {
            (
                Rc::from(name),
                test_node(ast::Variable {
                    domain: test_node(ast::Domain::UnboundedInt),
                    value: None,
                    annotations: vec![],
                }),
            )
        })
        .collect()
}
