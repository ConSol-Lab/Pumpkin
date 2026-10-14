//! Apply the preprocessing a preamble states, in place of the view pass: the variables it makes
//! views are made views, its parameters are created, its constants and literals are made, the
//! constraints it skips are not posted, and its steps head the proof. Nothing is decided here;
//! a preamble that does not fit the model is refused.

use std::collections::HashMap;
use std::collections::HashSet;
use std::num::NonZero;
use std::rc::Rc;

use crate::core::constraints::Constraint;
use crate::core::predicate;
use crate::core::predicates::Predicate;
use crate::core::proof::ConstraintTag;
use crate::core::proof::HeadField;
use crate::core::variables::DomainId;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::compiler::context::Affine;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::compiler::context::Domain;
use crate::flatzinc::compiler::context::LiteralPredicate;
use crate::flatzinc::preamble::Atom;
use crate::flatzinc::preamble::BreakingItem;
use crate::flatzinc::preamble::Comparator;
use crate::flatzinc::preamble::Posted;
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

/// Post the constraints of the preamble's symmetry and dominance breaking steps and hold the
/// steps, after the model's constraints. Each identifier the preamble gives a step is replaced by
/// the one the proof gives it when the held steps are written, where the step takes it and where
/// a step cites it, and the constraint posted under it is posted once the step's lines are held,
/// since posting it can derive a nogood, which is written after the steps held so far.
pub(crate) fn apply_breaking(
    context: &mut CompilationContext,
    preamble: &Preamble,
) -> Result<(), FlatZincError> {
    let mut identifiers: HashMap<u64, ConstraintTag> = HashMap::new();
    let mut pending: Option<(ConstraintTag, &Posted)> = None;
    for item in &preamble.breaking {
        match item {
            BreakingItem::Key(line) => {
                let fields = head_fields(context, line, &identifiers)?;
                context
                    .solver
                    .log_proof_order_key_line(&fields)
                    .map_err(|_| unwritable())?;
            }
            BreakingItem::Step(line) => {
                let fields = head_fields(context, line, &identifiers)?;
                context
                    .solver
                    .log_proof_head_line(&fields)
                    .map_err(|_| unwritable())?;
            }
            BreakingItem::Identifier { identifier, posted } => {
                if let Some((tag, posted)) = pending.take() {
                    post(context, tag, posted)?;
                }
                let tag = context.solver.new_constraint_tag();
                context.solver.hold_proof_step(tag);
                let _ = identifiers.insert(*identifier, tag);
                pending = posted.as_ref().map(|posted| (tag, posted));
            }
        }
    }
    if let Some((tag, posted)) = pending {
        post(context, tag, posted)?;
    }
    Ok(())
}

fn post(
    context: &mut CompilationContext,
    tag: ConstraintTag,
    posted: &Posted,
) -> Result<(), FlatZincError> {
    match posted {
        Posted::Clause(atoms) => {
            let predicates = atoms
                .iter()
                .map(|atom| predicate_of(context, atom))
                .collect::<Result<Vec<_>, _>>()?;
            context.solver.add_clause(predicates, tag);
        }
        Posted::LexLessOrEqual(first, second) => {
            let domains = |context: &CompilationContext, names: &[String]| {
                names
                    .iter()
                    .map(|name| solver_domain(context, name))
                    .collect::<Result<Vec<DomainId>, _>>()
            };
            let x = domains(context, first)?;
            let y = domains(context, second)?;
            pumpkin_constraints::lex_less_or_equal(x, y, tag).post(context.solver);
        }
    }
    Ok(())
}

/// The domain the solver gives the variable: a representative, a parameter, or a fresh variable
/// of a decomposition.
fn solver_domain(context: &CompilationContext, name: &str) -> Result<DomainId, FlatZincError> {
    let is_literal = context
        .literal_predicates
        .iter()
        .any(|literal| literal.boolean.as_ref() == name);
    context
        .variable_map
        .get(name)
        .copied()
        .filter(|_| !is_literal)
        .ok_or_else(|| {
            refused(format!(
                "the breaking steps name '{name}', which the solver has no variable for"
            ))
        })
}

fn predicate_of(context: &CompilationContext, atom: &Atom) -> Result<Predicate, FlatZincError> {
    let domain = solver_domain(context, &atom.variable)?;
    let value = i32::try_from(atom.value)
        .map_err(|_| refused(format!("the value of the atom {atom} does not fit")))?;
    Ok(match atom.comparator {
        Comparator::LessOrEqual => predicate![domain <= value],
        Comparator::GreaterOrEqual => predicate![domain >= value],
        Comparator::Equal => predicate![domain == value],
        Comparator::NotEqual => predicate![domain != value],
    })
}

/// The line as the fields of a proof line: each atom `[x <= 3]` an atom, each rule `l:<name>` a
/// label, and each identifier of a breaking step, where a step takes it (`y <id>`) or cites it
/// (`c:<id>`), the identifier of the step's constraint.
fn head_fields(
    context: &CompilationContext,
    line: &str,
    identifiers: &HashMap<u64, ConstraintTag>,
) -> Result<Vec<HeadField>, FlatZincError> {
    let step_of = |token: &str| -> Option<ConstraintTag> {
        token
            .parse::<u64>()
            .ok()
            .and_then(|identifier| identifiers.get(&identifier).copied())
    };
    let mut fields = vec![];
    let mut text = String::new();
    let mut rest = line;
    let mut first_token = true;
    while !rest.is_empty() {
        if let Some(inner) = rest.strip_prefix('[') {
            let end = inner
                .find(']')
                .ok_or_else(|| refused(format!("an atom is not closed in '{line}'")))?;
            let atom = Atom::parse_list(&rest[..end + 2])
                .and_then(|atoms| atoms.into_iter().next())
                .ok_or_else(|| refused(format!("an atom cannot be read in '{line}'")))?;
            fields.push(HeadField::Text(std::mem::take(&mut text)));
            fields.push(HeadField::Atom(predicate_of(context, &atom)?));
            rest = &inner[end + 1..];
            continue;
        }
        let token_end = rest
            .find(|c: char| c.is_whitespace() || c == '[')
            .unwrap_or(rest.len());
        let (token, after) = rest.split_at(token_end);
        if token.is_empty() {
            let space_end = rest
                .find(|c: char| !c.is_whitespace())
                .unwrap_or(rest.len());
            text.push_str(&rest[..space_end]);
            rest = &rest[space_end..];
            continue;
        }
        if let Some(label) = token.strip_prefix("l:") {
            text.push_str("l:");
            fields.push(HeadField::Text(std::mem::take(&mut text)));
            fields.push(HeadField::Label(static_label(label)));
        } else if let Some(cited) = token.strip_prefix("c:") {
            text.push_str("c:");
            match step_of(cited) {
                Some(tag) => {
                    fields.push(HeadField::Text(std::mem::take(&mut text)));
                    fields.push(HeadField::Identifier(tag));
                }
                None => text.push_str(cited),
            }
        } else if !first_token && fields.is_empty() && text.trim().len() == 1 {
            // The identifier a step takes follows its letter.
            match step_of(token) {
                Some(tag) => {
                    fields.push(HeadField::Text(std::mem::take(&mut text)));
                    fields.push(HeadField::Identifier(tag));
                }
                None => text.push_str(token),
            }
        } else {
            text.push_str(token);
        }
        first_token = false;
        rest = after;
    }
    fields.push(HeadField::Text(text));
    Ok(fields)
}

/// The rule names a proof line refers to by label live as long as the proof.
fn static_label(label: &str) -> &'static str {
    thread_local! {
        static LABELS: std::cell::RefCell<HashMap<String, &'static str>> =
            std::cell::RefCell::new(HashMap::new());
    }
    LABELS.with(|labels| {
        *labels
            .borrow_mut()
            .entry(label.to_owned())
            .or_insert_with(|| Box::leak(label.to_owned().into_boxed_str()))
    })
}

fn unwritable() -> FlatZincError {
    FlatZincError::InvalidIdentifier {
        identifier: "proof".into(),
        expected_type: "writable proof file".into(),
    }
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
