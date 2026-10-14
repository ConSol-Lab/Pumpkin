//! Writing the steps of the symmetry and dominance breaking passes: in a croc proof in the block
//! format, or, for a scaffold, in its preamble.

use std::rc::Rc;

use flatzinc::Expr;

use super::context::CompilationContext;
use super::symmetry_breaking::canonical_form::Element;
use super::symmetry_breaking::canonical_form::ModelIndex;
use super::symmetry_breaking::canonical_form::Resolved;
use crate::core::proof::ConstraintTag;
use crate::core::proof::HeadField;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::preamble::BreakingItem;
use crate::flatzinc::preamble::Posted;

/// The constraints fzn-drcp-check reads as linear or atomic constraints, clauses, or half
/// reifications of those, whose dominance steps it decides.
pub(crate) const LINEAR_CONSTRAINTS: &[&str] = &[
    "int_lin_le",
    "int_lin_eq",
    "int_lin_ne",
    "int_le",
    "int_lt",
    "int_eq",
    "int_ne",
    "bool2int",
    "bool_eq",
    "bool_le",
    "bool_lt",
    "bool_not",
    "bool_lin_eq",
    "bool_clause",
    "int_lin_le_imp",
    "int_lin_eq_imp",
    "int_lin_ne_imp",
    "int_le_imp",
    "int_lt_imp",
    "int_eq_imp",
    "int_ne_imp",
];

/// For each variable, the other variables of the model the compiler made views of it, with the
/// scale and offset.
pub(crate) fn aliases_of(
    ast: &FlatZincAst,
    index: &ModelIndex,
    context: &mut CompilationContext,
) -> Vec<Vec<(Rc<str>, i64, i64)>> {
    let mut aliases = vec![vec![]; index.variables.len()];
    for declaration in &ast.single_variables {
        let (SingleVarDecl::Bool { id, .. }
        | SingleVarDecl::IntInRange { id, .. }
        | SingleVarDecl::IntInSet { id, .. }) = declaration;
        let name = context.identifiers.get_interned(id);
        if context.equivalences.representative(&name) == name {
            continue;
        }
        if let Ok(resolved) = index.resolve(&Expr::VarParIdentifier(id.clone())) {
            match resolved.as_ref() {
                Resolved::Scalar(Element::Var(var)) => aliases[*var].push((name, 1, 0)),
                Resolved::Scalar(Element::View { var, scale, offset }) => {
                    aliases[*var].push((name, *scale, *offset));
                }
                _ => {}
            }
        }
    }
    aliases
}

/// `scale * variable + offset` as a replacement of a witness.
pub(crate) fn affine_fields(
    scale: i64,
    variable: crate::core::variables::DomainId,
    offset: i64,
) -> Vec<HeadField> {
    let mut fields = match scale {
        1 => vec![],
        -1 => vec![HeadField::Text("-".to_owned())],
        scale => vec![HeadField::Text(format!("{scale}*"))],
    };
    fields.push(HeadField::Name(variable));
    if offset != 0 {
        fields.push(HeadField::Text(format!("{offset:+}")));
    }
    fields
}

/// Write a line of a step: in a proof in the block format, or, for a scaffold, in its preamble.
pub(crate) fn log(context: &mut CompilationContext, fields: Vec<HeadField>) {
    if context.next_preamble_identifier.is_some() {
        let line = context.solver.render_head_line_as_text(&fields);
        context.preamble.breaking.push(BreakingItem::Step(line));
        return;
    }
    context
        .solver
        .log_proof_head_line(&fields)
        .expect("the proof file is writable");
}

/// Write a key of the order, a `p` line, before the head and the steps.
pub(crate) fn log_order_key(context: &mut CompilationContext, fields: Vec<HeadField>) {
    if context.next_preamble_identifier.is_some() {
        let line = context.solver.render_head_line_as_text(&fields);
        context.preamble.breaking.push(BreakingItem::Key(line));
        return;
    }
    context
        .solver
        .log_proof_order_key_line(&fields)
        .expect("the proof file is writable");
}

/// The identifier the step that adds the constraint with the tag takes: in a proof in the block
/// format, or, for a scaffold, in its preamble. `None` when neither is written.
pub(crate) fn take_identifier(context: &mut CompilationContext, tag: ConstraintTag) -> Option<u32> {
    let Some(next) = context.next_preamble_identifier.as_mut() else {
        return context.solver.take_proof_identifier(tag);
    };
    let identifier = *next;
    *next += 1;
    let _ = context
        .preamble_identifiers
        .insert(tag, context.preamble.breaking.len());
    context.preamble.breaking.push(BreakingItem::Identifier {
        identifier: u64::from(identifier),
        posted: None,
    });
    Some(identifier)
}

/// For a scaffold, state in its preamble the constraint posted under the tag of a step.
pub(crate) fn record_posted(context: &mut CompilationContext, tag: ConstraintTag, posted: Posted) {
    let Some(&position) = context.preamble_identifiers.get(&tag) else {
        return;
    };
    if let BreakingItem::Identifier { posted: slot, .. } = &mut context.preamble.breaking[position]
    {
        *slot = Some(posted);
    }
}
