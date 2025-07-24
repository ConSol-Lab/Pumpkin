//! FlatZinc files can contain variables that are never used in any constraints. This step removes
//! those variables from the AST.
//!
//! To implement this, first we go through all constraints to mark any identifiers they have as
//! arguments. Then, we go through all _marked_ arrays to mark any identifiers contained in the
//! arrays. Finally, we go through the variables and constants and remove them if they are
//! unmarked.
use std::collections::BTreeSet;
use std::rc::Rc;

use fzn_rs::ast::Ast;

use crate::flatzinc::error::FlatZincError;

pub(crate) fn run(ast: &mut Ast) -> Result<(), FlatZincError> {
    let mut marked_identifiers = BTreeSet::new();

    mark_identifiers_in_constraints(ast, &mut marked_identifiers);
    mark_identifiers_in_arrays(ast, &mut marked_identifiers);

    // Make sure the objective, which can be unconstrained, is always marked.
    match &ast.solve.method.node {
        fzn_rs::ast::Method::Optimize {
            objective: fzn_rs::ast::Literal::Identifier(objective),
            ..
        } => {
            let _ = marked_identifiers.insert(Rc::clone(objective));
        }

        _ => {}
    }

    ast.variables.retain(|name, variable| {
        marked_identifiers.contains(name)
            || variable
                .node
                .annotations
                .iter()
                .any(|node| node.node.name() == "output_var")
    });

    Ok(())
}

/// Go over all arrays and mark the identifiers that are elements of the array.
fn mark_identifiers_in_arrays(ast: &Ast, marked_identifiers: &mut BTreeSet<Rc<str>>) {
    ast.arrays
        .values()
        .flat_map(|array| array.node.contents.iter())
        .for_each(|node| {
            mark_literal(&node.node, marked_identifiers);
        });
}

/// Go over all constraints and add any identifier in the arguments to the `marked_identifiers` set.
fn mark_identifiers_in_constraints(ast: &Ast, marked_identifiers: &mut BTreeSet<Rc<str>>) {
    for argument_node in ast
        .constraints
        .iter()
        .flat_map(|constraint| &constraint.node.arguments)
    {
        match &argument_node.node {
            fzn_rs::ast::Argument::Array(nodes) => nodes.iter().for_each(|node| {
                mark_literal(&node.node, marked_identifiers);
            }),

            fzn_rs::ast::Argument::Literal(node) => {
                mark_literal(&node.node, marked_identifiers);
            }
        }
    }
}

fn mark_literal(literal: &fzn_rs::ast::Literal, marked_identifiers: &mut BTreeSet<Rc<str>>) {
    match literal {
        fzn_rs::ast::Literal::Identifier(ident) => {
            let _ = marked_identifiers.insert(Rc::clone(ident));
        }
        _ => {}
    }
}
