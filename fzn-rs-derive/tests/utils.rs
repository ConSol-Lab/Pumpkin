#![allow(
    dead_code,
    reason = "it is used in other test files, but somehow compiler can't see it"
)]
#![cfg(test)]

use std::collections::BTreeMap;
use std::rc::Rc;

use fzn_rs::ast::{self};

pub(crate) fn satisfy_solve() -> ast::SolveItem<ast::Annotation> {
    ast::SolveItem {
        method: test_node(ast::Method::Satisfy),
        annotations: vec![],
    }
}

pub(crate) fn test_node<T>(data: T) -> ast::Node<T> {
    node(data, usize::MAX, usize::MAX)
}

pub(crate) fn node<T>(data: T, span_start: usize, span_end: usize) -> ast::Node<T> {
    ast::Node {
        node: data,
        span: ast::Span {
            start: span_start,
            end: span_end,
        },
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
