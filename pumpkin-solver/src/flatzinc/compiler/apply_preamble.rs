//! Apply the preprocessing a preamble states, in place of the view pass: the variables it makes
//! views are made views, its parameters are created, its constants and literals are made, the
//! constraints it skips are not posted, and its steps head the proof. Nothing is decided here;
//! a preamble that does not fit the model is refused.

use std::collections::HashSet;
use std::num::NonZero;
use std::rc::Rc;

use pumpkin_solver::core::proof::ConstraintTag;

use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::compiler::context::Affine;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::compiler::context::Domain;
use crate::flatzinc::compiler::context::LiteralPredicate;
use crate::flatzinc::preamble::Preamble;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    preamble: &Preamble,
) -> Result<(), FlatZincError> {
    for single_var_decl in &ast.single_variables {
        if let SingleVarDecl::Bool { id, .. } = single_var_decl {
            let id = context.identifiers.get_interned(id);
            let _ = context.equivalences.booleans.insert(id);
        }
    }
    let after_last_constraint = context
        .constraints
        .iter()
        .map(|(tag, _)| tag_number(*tag))
        .max()
        .unwrap_or(0)
        + 1;

    for parameter in &preamble.parameters {
        let name = context.identifiers.get_interned(parameter);
        if context.equivalences.is_defined(&name) {
            return Err(refused(format!(
                "the parameter '{parameter}' is a variable of the model"
            )));
        }
        context.equivalences.create_parameter_class(name);
    }

    for view in &preamble.views {
        let variable = defined_variable(context, &view.variable)?;
        let representative = defined_variable(context, &view.representative)?;
        if context.equivalences.representative(&representative) != representative {
            return Err(refused(format!(
                "the representative '{}' of '{}' is itself a view",
                view.representative, view.variable
            )));
        }
        if context.equivalences.representative(&variable) != variable
            || context.equivalences.class_size(&variable) != 1
        {
            return Err(refused(format!(
                "'{}' is a view of more than one variable, or a representative",
                view.variable
            )));
        }
        let (Ok(scale), Ok(offset)) = (i32::try_from(view.scale), i32::try_from(view.offset))
        else {
            return Err(refused(format!(
                "the view of '{}' does not fit",
                view.variable
            )));
        };
        if scale == 0 {
            return Err(refused(format!(
                "the view of '{}' has scale 0",
                view.variable
            )));
        }
        context
            .equivalences
            .absorb(&representative, &variable, Affine { scale, offset });
    }

    for (name, value) in &preamble.constants {
        let variable = defined_variable(context, name)?;
        if context.equivalences.representative(&variable) != variable {
            return Err(refused(format!("the constant '{name}' is a view")));
        }
        let fits = i32::try_from(*value).ok().filter(|&value| {
            match context.equivalences.domain(&variable) {
                Domain::IntervalDomain { lb, ub } => lb <= value && value <= ub,
                Domain::SparseDomain { values } => values.contains(&value),
            }
        });
        let Some(value) = fits else {
            return Err(refused(format!(
                "the value {value} of the constant '{name}' is not in its domain"
            )));
        };
        *context.equivalences.get_mut_domain(&variable) = Domain::IntervalDomain {
            lb: value,
            ub: value,
        };
        context.declared_constants.push(variable);
    }

    let skipped: HashSet<u64> = preamble.skipped.iter().copied().collect();
    let numbers: HashSet<u64> = context
        .constraints
        .iter()
        .map(|(tag, _)| tag_number(*tag))
        .collect();
    if let Some(number) = skipped.iter().find(|number| !numbers.contains(number)) {
        return Err(refused(format!(
            "the skipped constraint {number} is not a constraint of the model"
        )));
    }
    context
        .constraints
        .retain(|(tag, _)| !skipped.contains(&tag_number(*tag)));

    for literal in &preamble.literals {
        let Some(tag) = context
            .constraints
            .iter()
            .map(|(tag, _)| *tag)
            .find(|tag| tag_number(*tag) == literal.constraint)
        else {
            return Err(refused(format!(
                "the constraint {} defining the literal '{}' is not posted",
                literal.constraint, literal.boolean
            )));
        };
        let boolean = defined_variable(context, &literal.boolean)?;
        let variable = defined_variable(context, &literal.variable)?;
        if !context.equivalences.booleans.contains(&boolean)
            || context.equivalences.class_size(&boolean) != 1
            || context.equivalences.representative(&variable) != variable
        {
            return Err(refused(format!(
                "'{}' is not a boolean of its own, or '{}' is a view",
                literal.boolean, literal.variable
            )));
        }
        let Ok(value) = i32::try_from(literal.value) else {
            return Err(refused(format!(
                "the value of the literal '{}' does not fit",
                literal.boolean
            )));
        };
        context.literal_predicates.push(LiteralPredicate {
            boolean,
            variable,
            comparator: literal.comparator.as_str(),
            value,
            tag,
        });
    }

    if context.solver.is_logging_proof() {
        let first_free = preamble.first_free.unwrap_or(after_last_constraint);
        let first_free = u32::try_from(first_free)
            .map_err(|_| refused(format!("the first identifier {first_free} does not fit")))?;
        context
            .solver
            .log_proof_head(&preamble.steps, first_free)
            .map_err(|_| FlatZincError::InvalidIdentifier {
                identifier: "proof".into(),
                expected_type: "writable proof file".into(),
            })?;
        if let Some((objective, evidence)) = &preamble.objective {
            let objective = defined_variable(context, objective)?;
            let (representative, view) = context.equivalences.view(&objective);
            let evidence = u32::try_from(*evidence)
                .ok()
                .filter(|&evidence| evidence != 0)
                .ok_or_else(|| refused(format!("the constraint {evidence} does not fit")))?;
            context.solver.name_objective_view(
                &objective,
                &representative,
                view.scale,
                view.offset,
                evidence,
            );
        }
    }

    Ok(())
}

/// Every decomposition the preamble states is one the compiler made, with the same fresh
/// variables. The compiler may decompose more, over variables of its own.
pub(crate) fn check_decompositions(
    context: &CompilationContext,
    preamble: &Preamble,
) -> Result<(), FlatZincError> {
    for stated in &preamble.decompositions {
        if !context.preamble.decompositions.contains(stated) {
            return Err(refused(format!(
                "the decomposition of constraint {} with {} is not the one the compiler makes",
                stated.constraint,
                stated.fresh_variables.join(" ")
            )));
        }
    }
    Ok(())
}

fn defined_variable(
    context: &mut CompilationContext,
    name: &str,
) -> Result<Rc<str>, FlatZincError> {
    let interned = context.identifiers.get_interned(name);
    if context.equivalences.is_defined(&interned) {
        Ok(interned)
    } else {
        Err(refused(format!("'{name}' is not a variable of the model")))
    }
}

fn refused(message: String) -> FlatZincError {
    FlatZincError::Preamble(message)
}

fn tag_number(tag: ConstraintTag) -> u64 {
    u64::from(NonZero::<u32>::from(tag).get())
}
