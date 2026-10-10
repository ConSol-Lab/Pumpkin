//! Collect the views the instance states.
//!
//! A two-term equality between variables, `a * x + b * y = c`, whether written as `int_eq`,
//! `bool_eq`, `bool2int`, `bool_not`, a two-term `int_lin_eq` or a declaration `var ..: b = a`,
//! makes one variable an affine view of the other when one coefficient divides the other and
//! the constant, and otherwise makes both views of a fresh parameter. The equality is then not
//! posted: every other constraint is posted on the views, and the equality's propagation is the
//! view's arithmetic.
//!
//! The proof records each step in the checker's terms: a definition is numbered with `n`, the
//! equality is rewritten by the views its variables already are (`s` in place), a fresh parameter
//! is introduced with `v`, the equalities that relate the absorbed class's members to their old
//! representative are rewritten to the new one, and finally every posted constraint that mentions
//! a view is rewritten, with its result stated where the checker requires it. The invariant kept
//! throughout is that every member `m` of a class has a cited constraint reading
//! `c_m * m + c_r * r = k` with `r` the class's current representative.
//!
//! The domain a member was declared with narrows the representative's; in the proof the declared
//! domains (constraint 0) are rewritten by the views last, which states the same narrowing.

use std::collections::HashMap;
use std::collections::HashSet;
use std::num::NonZero;
use std::rc::Rc;

use flatzinc::ConstraintItem;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::proof::view_log::LinearForm;
use pumpkin_solver::core::proof::view_log::Relation;
use pumpkin_solver::core::proof::view_log::Replacement;
use pumpkin_solver::core::proof::view_log::Term;
use pumpkin_solver::core::proof::view_log::ViewEvent;
use pumpkin_solver::core::proof::view_log::ViewLog;

use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::ast::VarArrayDecl;
use crate::flatzinc::compiler::context::Affine;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::preamble::View;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    let started = std::time::Instant::now();
    let result = collect(ast, context, options);
    context.view_statistics.seconds = started.elapsed().as_secs_f64();
    result
}

fn collect(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    for single_var_decl in &ast.single_variables {
        if let SingleVarDecl::Bool { id, .. } = single_var_decl {
            let id = context.identifiers.get_interned(id);
            let _ = context.equivalences.booleans.insert(id);
        }
    }

    let mut clock = std::time::Instant::now();
    let mut phases: Vec<(&'static str, f64)> = vec![];
    let mut phase = |phases: &mut Vec<(&'static str, f64)>, name: &'static str| {
        phases.push((name, clock.elapsed().as_secs_f64()));
        clock = std::time::Instant::now();
    };
    let first_free_id = context
        .constraints
        .iter()
        .map(|(tag, _)| tag_number(*tag))
        .max()
        .unwrap_or(0)
        + 1;
    let arrays = ast
        .variable_arrays
        .iter()
        .map(|decl| match decl {
            VarArrayDecl::Int { id, .. } | VarArrayDecl::Bool { id, .. } => (id.as_str(), decl),
        })
        .collect();
    let mut views = Views {
        ast,
        arrays,
        evidence: HashMap::default(),
        forms: HashMap::default(),
        next_id: first_free_id,
        next_parameter: 0,
        events: vec![],
        absorbed: HashSet::default(),
        objective: objective_name(ast).map(|name| context.identifiers.get_interned(name)),
    };
    phase(&mut phases, "setup");
    // Only an equality can become a view; the form of any other constraint is built when it is
    // rewritten.
    for (tag, item) in &context.constraints {
        let is_equality = matches!(
            item.id.as_str(),
            "int_lin_eq" | "int_eq" | "bool_eq" | "bool2int" | "bool_not"
        );
        if !is_equality {
            continue;
        }
        if let Some(form) = views.linear_form_of(context, item) {
            let _ = views.forms.insert(tag_number(*tag), form);
        }
    }

    phase(&mut phases, "forms");
    views.collect_definitions(context)?;

    phase(&mut phases, "definitions");

    let constraints = std::mem::take(&mut context.constraints);
    let mut kept = vec![];
    let mut skipped = vec![];
    for (tag, item) in constraints {
        if views.collect_equality(context, tag_number(tag), &item) {
            skipped.push(tag_number(tag));
            continue;
        }
        kept.push((tag, item));
    }
    for (_, item) in &kept {
        if !matches!(
            item.id.as_str(),
            "int_lin_eq" | "int_eq" | "bool_eq" | "bool2int" | "bool_not"
        ) {
            continue;
        }
        let Some(form) = views.linear_form_of(context, item) else {
            continue;
        };
        if let Some((x, _, y, _, _)) = Views::two_variable_equality(&form) {
            if context.equivalences.view(&x).0 == context.equivalences.view(&y).0 {
                context.view_statistics.two_term_equalities_in_one_class += 1;
            } else {
                context.view_statistics.two_term_equalities_declined += 1;
            }
        }
    }
    context.constraints = kept;

    phase(&mut phases, "equalities");
    views.rewrite_posted_constraints(context);

    phase(&mut phases, "rewrites");
    {
        let names: Vec<Rc<str>> = context.equivalences.classes.keys().cloned().collect();
        let statistics = &mut context.view_statistics;
        for name in &names {
            let (representative, view) = context.equivalences.view(name);
            if representative.as_ref() == name.as_ref() {
                continue;
            }
            match (view.scale, view.offset) {
                (1, 0) => statistics.aliases += 1,
                (-1, 1) if context.equivalences.booleans.contains(name) => {
                    statistics.negations += 1
                }
                _ => statistics.other_views += 1,
            }
        }
        statistics.parameters = context.equivalences.parameters.len();
        statistics.skipped = skipped.len();
        for event in &views.events {
            match event {
                ViewEvent::Definition { .. } => statistics.definitions += 1,
                ViewEvent::Substitute { .. } => statistics.rewrites += 1,
                ViewEvent::Parametrise { .. } => {}
            }
        }
    }

    phase(&mut phases, "statistics");
    if let Some(path) = options.head_path.as_ref() {
        let head = ViewLog {
            events: views.events.clone(),
        };
        let lines = head.render();
        let mut text = lines.join("\n");
        if !lines.is_empty() {
            text.push('\n');
        }
        std::fs::write(path, text).map_err(|_| FlatZincError::InvalidIdentifier {
            identifier: path.display().to_string().into(),
            expected_type: "writable proof file".into(),
        })?;
    }

    // A proof in the block format starts with the view steps, and its learned nogoods are
    // numbered on from them.
    if context.solver.is_logging_proof() {
        let head = ViewLog {
            events: views.events.clone(),
        };
        let lines = head.render();
        context
            .solver
            .log_proof_head(&lines, views.next_id as u32)
            .map_err(|_| FlatZincError::InvalidIdentifier {
                identifier: "proof".into(),
                expected_type: "writable proof file".into(),
            })?;
        // The proof concludes a bound on the objective, which the solver bounds through the
        // representative of its class; the evidence of the view ties the two.
        if let Some(objective) = views.objective.as_ref()
            && context.equivalences.is_defined(objective)
        {
            let (representative, view) = context.equivalences.view(objective);
            if representative != *objective
                && let Some(&evidence) = views.evidence.get(objective)
            {
                context.solver.name_objective_view(
                    objective,
                    &representative,
                    view.scale,
                    view.offset,
                    evidence as u32,
                );
            }
        }
    }

    phase(&mut phases, "head");
    if std::env::var_os("PUMPKIN_VIEW_PHASES").is_some() {
        for (name, seconds) in &phases {
            println!("%%%mzn-stat: viewPhase_{name}={seconds}");
        }
    }

    // The preamble states what the pass decided, for a proof processor that reads the scaffold.
    let preamble = &mut context.preamble;
    let mut names: Vec<Rc<str>> = context.equivalences.classes.keys().cloned().collect();
    names.sort();
    for name in &names {
        let (representative, view) = context.equivalences.view(name);
        if representative.as_ref() != name.as_ref() {
            preamble.views.push(View {
                variable: name.to_string(),
                representative: representative.to_string(),
                scale: i64::from(view.scale),
                offset: i64::from(view.offset),
            });
        }
    }
    preamble.parameters = context
        .equivalences
        .parameters
        .iter()
        .map(|name| name.to_string())
        .collect();
    preamble.skipped = skipped;
    if let Some(objective) = views.objective.as_ref()
        && context.equivalences.is_defined(objective)
        && context.equivalences.representative(objective) != *objective
        && let Some(&evidence) = views.evidence.get(objective)
    {
        preamble.objective = Some((objective.to_string(), evidence));
    }
    preamble.first_free = Some(views.next_id);
    preamble.steps = ViewLog {
        events: views.events,
    }
    .render();

    Ok(())
}

/// The variable an optimisation minimises or maximises.
fn objective_name(ast: &FlatZincAst) -> Option<&str> {
    match &ast.solve_item.goal {
        flatzinc::Goal::OptimizeBool(_, flatzinc::BoolExpr::VarParIdentifier(name)) => {
            Some(name.as_str())
        }
        flatzinc::Goal::OptimizeInt(_, flatzinc::IntExpr::VarParIdentifier(name)) => {
            Some(name.as_str())
        }
        _ => None,
    }
}

fn tag_number(tag: ConstraintTag) -> u64 {
    u64::from(NonZero::<u32>::from(tag).get())
}

enum Operand {
    Variable(Rc<str>),
    Constant(i64),
}

struct Views<'a> {
    ast: &'a FlatZincAst,
    /// The variable arrays by name, so that a reference is found without scanning them all.
    arrays: HashMap<&'a str, &'a VarArrayDecl>,
    /// For every member of a class other than its representative, the constraint reading
    /// `c_m * m + c_r * r = k` with `r` the representative.
    evidence: HashMap<Rc<str>, u64>,
    /// The linear constraints the checker holds, in its form, under their numbers.
    forms: HashMap<u64, LinearForm>,
    next_id: u64,
    next_parameter: u64,
    events: Vec<ViewEvent>,
    /// The variables that are views of another and no longer appear in posted constraints.
    absorbed: HashSet<Rc<str>>,
    /// The objective variable, which keeps its name in the proof.
    objective: Option<Rc<str>>,
}

impl Views<'_> {
    fn fresh_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn operand(&self, context: &CompilationContext, expr: &flatzinc::Expr) -> Option<Operand> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => self.operand_of_identifier(context, id),
            flatzinc::Expr::Int(value) => Some(Operand::Constant(*value as i64)),
            flatzinc::Expr::Bool(value) => Some(Operand::Constant(i64::from(*value))),
            _ => None,
        }
    }

    fn operand_of_identifier(&self, context: &CompilationContext, id: &str) -> Option<Operand> {
        if let Some(value) = context.integer_parameters.get(id) {
            return Some(Operand::Constant(i64::from(*value)));
        }
        if let Some(value) = context.boolean_parameters.get(id) {
            return Some(Operand::Constant(i64::from(*value)));
        }
        if let Some((name, _)) = context.equivalences.classes.get_key_value(id) {
            return Some(Operand::Variable(Rc::clone(name)));
        }
        None
    }

    fn array_operands(
        &self,
        context: &CompilationContext,
        expr: &flatzinc::Expr,
    ) -> Option<Vec<Operand>> {
        match expr {
            flatzinc::Expr::ArrayOfInt(items) => items
                .iter()
                .map(|item| match item {
                    flatzinc::IntExpr::Int(value) => Some(Operand::Constant(*value as i64)),
                    flatzinc::IntExpr::VarParIdentifier(id) => {
                        self.operand_of_identifier(context, id)
                    }
                })
                .collect(),
            flatzinc::Expr::ArrayOfBool(items) => items
                .iter()
                .map(|item| match item {
                    flatzinc::BoolExpr::Bool(value) => Some(Operand::Constant(i64::from(*value))),
                    flatzinc::BoolExpr::VarParIdentifier(id) => {
                        self.operand_of_identifier(context, id)
                    }
                })
                .collect(),
            flatzinc::Expr::VarParIdentifier(id) => {
                if let Some(values) = context.integer_array_parameters.get(id.as_str()) {
                    return Some(
                        values
                            .iter()
                            .map(|value| Operand::Constant(i64::from(*value)))
                            .collect(),
                    );
                }
                if let Some(values) = context.boolean_array_parameters.get(id.as_str()) {
                    return Some(
                        values
                            .iter()
                            .map(|value| Operand::Constant(i64::from(*value)))
                            .collect(),
                    );
                }
                match self.arrays.get(id.as_str()) {
                    Some(VarArrayDecl::Int {
                        array_expr: Some(flatzinc::ArrayOfIntExpr::Array(items)),
                        ..
                    }) => {
                        return items
                            .iter()
                            .map(|item| match item {
                                flatzinc::IntExpr::Int(value) => {
                                    Some(Operand::Constant(*value as i64))
                                }
                                flatzinc::IntExpr::VarParIdentifier(id) => {
                                    self.operand_of_identifier(context, id)
                                }
                            })
                            .collect();
                    }
                    Some(VarArrayDecl::Bool {
                        array_expr: Some(flatzinc::ArrayOfBoolExpr::Array(items)),
                        ..
                    }) => {
                        return items
                            .iter()
                            .map(|item| match item {
                                flatzinc::BoolExpr::Bool(value) => {
                                    Some(Operand::Constant(i64::from(*value)))
                                }
                                flatzinc::BoolExpr::VarParIdentifier(id) => {
                                    self.operand_of_identifier(context, id)
                                }
                            })
                            .collect();
                    }
                    _ => {}
                }
                None
            }
            _ => None,
        }
    }

    fn constant(&self, context: &CompilationContext, expr: &flatzinc::Expr) -> Option<i64> {
        match self.operand(context, expr)? {
            Operand::Constant(value) => Some(value),
            Operand::Variable(_) => None,
        }
    }

    /// The constraint as the checker's lowering states it, for the constraints it states as
    /// linear. `bool_not` is stated as `x + y = 1`.
    fn linear_form_of(
        &self,
        context: &CompilationContext,
        item: &ConstraintItem,
    ) -> Option<LinearForm> {
        let term_of = |operand: Operand| match operand {
            Operand::Variable(name) => Term::Variable(name),
            Operand::Constant(value) => Term::Constant(value),
        };
        let difference = |relation: Relation, bound: i64| -> Option<LinearForm> {
            if item.exprs.len() != 2 {
                return None;
            }
            let left = self.operand(context, &item.exprs[0])?;
            let right = self.operand(context, &item.exprs[1])?;
            Some(LinearForm {
                terms: vec![(1, term_of(left)), (-1, term_of(right))],
                relation,
                bound,
            })
        };
        match item.id.as_str() {
            "int_lin_le" | "int_lin_eq" | "int_lin_ne" => {
                if item.exprs.len() != 3 {
                    return None;
                }
                let weights = context
                    .resolve_array_integer_constants(&item.exprs[0])
                    .ok()?;
                let operands = self.array_operands(context, &item.exprs[1])?;
                let bound = self.constant(context, &item.exprs[2])?;
                if weights.len() != operands.len() {
                    return None;
                }
                let relation = match item.id.as_str() {
                    "int_lin_le" => Relation::LessOrEqual,
                    "int_lin_eq" => Relation::Equal,
                    _ => Relation::NotEqual,
                };
                Some(LinearForm {
                    terms: weights
                        .iter()
                        .zip(operands)
                        .map(|(weight, operand)| (i64::from(*weight), term_of(operand)))
                        .collect(),
                    relation,
                    bound,
                })
            }
            "bool_lin_eq" => {
                // The checker reads it as the weighted booleans minus the sum, equal to zero.
                if item.exprs.len() != 3 {
                    return None;
                }
                let weights = context
                    .resolve_array_integer_constants(&item.exprs[0])
                    .ok()?;
                let operands = self.array_operands(context, &item.exprs[1])?;
                let sum = self.operand(context, &item.exprs[2])?;
                if weights.len() != operands.len() {
                    return None;
                }
                let mut terms: Vec<(i64, Term)> = weights
                    .iter()
                    .zip(operands)
                    .map(|(weight, operand)| (i64::from(*weight), term_of(operand)))
                    .collect();
                terms.push((-1, term_of(sum)));
                Some(LinearForm {
                    terms,
                    relation: Relation::Equal,
                    bound: 0,
                })
            }
            "int_times" => {
                // `left * right = product` with a constant factor, `factor * other - product = 0`.
                if item.exprs.len() != 3 {
                    return None;
                }
                let left = self.operand(context, &item.exprs[0])?;
                let right = self.operand(context, &item.exprs[1])?;
                let product = self.operand(context, &item.exprs[2])?;
                let (factor, other) = match (left, right) {
                    (Operand::Constant(factor), other) | (other, Operand::Constant(factor)) => {
                        (factor, other)
                    }
                    _ => return None,
                };
                Some(LinearForm {
                    terms: vec![(factor, term_of(other)), (-1, term_of(product))],
                    relation: Relation::Equal,
                    bound: 0,
                })
            }
            "int_le" | "bool_le" => difference(Relation::LessOrEqual, 0),
            "int_lt" | "bool_lt" => difference(Relation::LessOrEqual, -1),
            "int_eq" | "bool_eq" | "bool2int" => difference(Relation::Equal, 0),
            "int_ne" => difference(Relation::NotEqual, 0),
            "bool_not" => {
                if item.exprs.len() != 2 {
                    return None;
                }
                let left = self.operand(context, &item.exprs[0])?;
                let right = self.operand(context, &item.exprs[1])?;
                Some(LinearForm {
                    terms: vec![(1, term_of(left)), (1, term_of(right))],
                    relation: Relation::Equal,
                    bound: 1,
                })
            }
            _ => None,
        }
    }

    /// `a * x + b * y = c` when the form is an equality over exactly two variables.
    fn two_variable_equality(form: &LinearForm) -> Option<TwoVariableEquality> {
        if form.relation != Relation::Equal {
            return None;
        }
        let mut variables: Vec<(i64, &str)> = vec![];
        let mut bound = form.bound;
        for (coefficient, term) in &form.terms {
            match term {
                Term::Variable(name) => variables.push((*coefficient, name.as_ref())),
                Term::Constant(value) => bound -= coefficient * value,
            }
        }
        match variables[..] {
            [(a, x), (b, y)] if x != y && a != 0 && b != 0 => {
                Some((Rc::from(x), a, Rc::from(y), b, bound))
            }
            _ => None,
        }
    }

    fn collect_definitions(
        &mut self,
        context: &mut CompilationContext,
    ) -> Result<(), FlatZincError> {
        for single_var_decl in &self.ast.single_variables {
            let (id, other) = match single_var_decl {
                SingleVarDecl::Bool {
                    id,
                    expr: Some(flatzinc::BoolExpr::VarParIdentifier(other)),
                    ..
                } => (id, other),
                SingleVarDecl::IntInRange {
                    id,
                    expr: Some(flatzinc::IntExpr::VarParIdentifier(other)),
                    ..
                } => (id, other),
                _ => continue,
            };
            let id = context.identifiers.get_interned(id);
            let Some(Operand::Variable(other)) = self.operand_of_identifier(context, other) else {
                // The definition names a parameter; the declaration already fixed the domain.
                continue;
            };
            if !context.equivalences.is_defined(&id) {
                return Err(FlatZincError::InvalidIdentifier {
                    identifier: id.as_ref().into(),
                    expected_type: "variable".into(),
                });
            }
            let number = self.fresh_id();
            self.events.push(ViewEvent::Definition {
                id: number,
                variable: Rc::clone(&id),
            });
            let _ = self.forms.insert(
                number,
                LinearForm {
                    terms: vec![
                        (1, Term::Variable(Rc::clone(&id))),
                        (-1, Term::Variable(Rc::clone(&other))),
                    ],
                    relation: Relation::Equal,
                    bound: 0,
                },
            );
            let _ = self.relate(context, number, &other, -1, &id, 1, 0);
        }
        Ok(())
    }

    /// Returns `true` when the constraint became a view and must not be posted.
    fn collect_equality(
        &mut self,
        context: &mut CompilationContext,
        number: u64,
        item: &ConstraintItem,
    ) -> bool {
        let Some(form) = self.forms.get(&number).cloned() else {
            return false;
        };
        let _ = item;
        let Some((x, a, y, b, c)) = Self::two_variable_equality(&form) else {
            return false;
        };
        self.relate(context, number, &x, a, &y, b, c)
    }

    /// `a * x + b * y = c` under the constraint `number`. Returns `true` when the classes were
    /// related and the constraint need not be posted.
    #[allow(
        clippy::too_many_arguments,
        reason = "the two terms, the right-hand side and the constraint"
    )]
    fn relate(
        &mut self,
        context: &mut CompilationContext,
        number: u64,
        x: &Rc<str>,
        a: i64,
        y: &Rc<str>,
        b: i64,
        c: i64,
    ) -> bool {
        let (r1, v1) = context.equivalences.view(x);
        let (r2, v2) = context.equivalences.view(y);
        if r1 == r2 {
            return false;
        }
        // The relation between the representatives: A * r1 + B * r2 = k.
        let big_a = a * i64::from(v1.scale);
        let big_b = b * i64::from(v2.scale);
        let k = c - a * i64::from(v1.offset) - b * i64::from(v2.offset);

        // r2 as a view of r1, or r1 as a view of r2. When both are integral the smaller class
        // is absorbed into the larger, so that a member's evidence is rewritten a logarithmic
        // number of times however the equalities are ordered.
        let second_of_first =
            (big_b != 0 && big_a % big_b == 0 && k % big_b == 0).then(|| Plan::Absorb {
                target: Rc::clone(&r1),
                absorbed: Rc::clone(&r2),
                view: (-big_a / big_b, k / big_b),
            });
        let first_of_second =
            (big_a != 0 && big_b % big_a == 0 && k % big_a == 0).then(|| Plan::Absorb {
                target: Rc::clone(&r2),
                absorbed: Rc::clone(&r1),
                view: (-big_b / big_a, k / big_a),
            });
        let plan = if let (Some(into_first), Some(into_second)) =
            (&second_of_first, &first_of_second)
        {
            let _ = (into_first, into_second);
            if context.equivalences.class_size(&r1) >= context.equivalences.class_size(&r2) {
                second_of_first.unwrap()
            } else {
                first_of_second.unwrap()
            }
        } else if let Some(plan) = second_of_first {
            plan
        } else if let Some(plan) = first_of_second {
            plan
        } else {
            let g = gcd(big_a.abs(), big_b.abs());
            if g == 0 || k % g != 0 {
                // No integer solution: left to the solver, which will find the model infeasible.
                return false;
            }
            Plan::Parametrise { g }
        };

        // A boolean must stay a boolean: its view of the final representative is the identity or
        // the negation.
        let keeps_booleans = |members: &[(Rc<str>, Affine)], view: Affine| {
            members.iter().all(|(name, member_view)| {
                !context.equivalences.booleans.contains(name) || {
                    let composed = member_view.compose(view);
                    (composed.scale, composed.offset) == (1, 0)
                        || (composed.scale, composed.offset) == (-1, 1)
                }
            })
        };

        match plan {
            Plan::Absorb {
                target,
                absorbed,
                view: (scale, offset),
            } => {
                let (Ok(scale), Ok(offset)) = (i32::try_from(scale), i32::try_from(offset)) else {
                    return false;
                };
                let view = Affine { scale, offset };
                let moved = context.equivalences.members(&absorbed);
                if !keeps_booleans(&moved, view) {
                    return false;
                }
                self.rewrite_by_views(context, number, x, y);
                // The rewritten constraint reads A * r1 + B * r2 = k and now relates the absorbed
                // representative to the target.
                let _ = self.evidence.insert(Rc::clone(&absorbed), number);
                self.rewrite_members(&moved, &absorbed, &target, view, number);
                context.equivalences.absorb(&target, &absorbed, view);
                let _ = self.absorbed.insert(absorbed);
                true
            }
            Plan::Parametrise { g } => {
                // r1 = x0 + p * t, r2 = y0 + q * t with p = B / g, q = -A / g, and (x0, y0) a
                // particular solution from a Bezout pair u * A + v * B = g.
                let (u, v) = bezout(big_a, big_b);
                let p = big_b / g;
                let q = -big_a / g;
                let x0 = u * (k / g);
                let y0 = v * (k / g);
                let (Ok(p32), Ok(q32), Ok(x032), Ok(y032)) = (
                    i32::try_from(p),
                    i32::try_from(q),
                    i32::try_from(x0),
                    i32::try_from(y0),
                ) else {
                    return false;
                };
                let first_view = Affine {
                    scale: p32,
                    offset: x032,
                };
                let second_view = Affine {
                    scale: q32,
                    offset: y032,
                };
                let first_members = context.equivalences.members(&r1);
                let second_members = context.equivalences.members(&r2);
                if !keeps_booleans(&first_members, first_view)
                    || !keeps_booleans(&second_members, second_view)
                {
                    return false;
                }
                self.rewrite_by_views(context, number, x, y);

                let parameter: Rc<str> = format!("view_parameter_{}", self.next_parameter).into();
                self.next_parameter += 1;
                let first_id = self.fresh_id();
                let second_id = self.fresh_id();
                self.events.push(ViewEvent::Parametrise {
                    parameter: Rc::clone(&parameter),
                    source: number,
                    equalities: [
                        (first_id, Rc::clone(&r1), p, x0),
                        (second_id, Rc::clone(&r2), q, y0),
                    ],
                });
                let _ = self.forms.insert(
                    first_id,
                    LinearForm {
                        terms: vec![
                            (1, Term::Variable(Rc::clone(&r1))),
                            (-p, Term::Variable(Rc::clone(&parameter))),
                        ],
                        relation: Relation::Equal,
                        bound: x0,
                    },
                );
                let _ = self.forms.insert(
                    second_id,
                    LinearForm {
                        terms: vec![
                            (1, Term::Variable(Rc::clone(&r2))),
                            (-q, Term::Variable(Rc::clone(&parameter))),
                        ],
                        relation: Relation::Equal,
                        bound: y0,
                    },
                );
                let _ = self.evidence.insert(Rc::clone(&r1), first_id);
                let _ = self.evidence.insert(Rc::clone(&r2), second_id);

                let _ = context.identifiers.get_interned(&parameter);
                context
                    .equivalences
                    .create_parameter_class(Rc::clone(&parameter));
                self.rewrite_members(&first_members, &r1, &parameter, first_view, first_id);
                self.rewrite_members(&second_members, &r2, &parameter, second_view, second_id);
                context.equivalences.absorb(&parameter, &r1, first_view);
                context.equivalences.absorb(&parameter, &r2, second_view);
                let _ = self.absorbed.insert(r1);
                let _ = self.absorbed.insert(r2);
                true
            }
        }
    }

    /// Rewrite the constraint in place by the views its two variables already are, so that it
    /// reads over the representatives.
    fn rewrite_by_views(
        &mut self,
        context: &CompilationContext,
        number: u64,
        x: &Rc<str>,
        y: &Rc<str>,
    ) {
        let mut replacements = vec![];
        for variable in [x, y] {
            let (representative, view) = context.equivalences.view(variable);
            if representative.as_ref() != variable.as_ref() {
                replacements.push(Replacement {
                    variable: Rc::clone(variable),
                    replacing: Rc::clone(&representative),
                    scale: i64::from(view.scale),
                    offset: i64::from(view.offset),
                    evidence: self.evidence[variable],
                });
            }
        }
        if !replacements.is_empty() {
            self.rewrite(number, replacements);
        }
    }

    /// The members of a class whose representative `old` becomes `view` of `new`: the constraint
    /// relating each member to `old` is rewritten to relate it to `new`.
    fn rewrite_members(
        &mut self,
        members: &[(Rc<str>, Affine)],
        old: &Rc<str>,
        new: &Rc<str>,
        view: Affine,
        evidence: u64,
    ) {
        for (member, _) in members {
            if member == old {
                continue;
            }
            let target = self.evidence[member];
            self.rewrite(
                target,
                vec![Replacement {
                    variable: Rc::clone(old),
                    replacing: Rc::clone(new),
                    scale: i64::from(view.scale),
                    offset: i64::from(view.offset),
                    evidence,
                }],
            );
        }
    }

    /// An `s` step on a constraint the checker holds as linear, kept under its number.
    fn rewrite(&mut self, target: u64, replacements: Vec<Replacement>) {
        let form = self.forms[&target].substitute(&replacements);
        let _ = self.forms.insert(target, form.clone());
        self.events.push(ViewEvent::Substitute {
            target,
            replacements,
            stated: Some(form),
        });
    }

    /// The variables a posted constraint mentions.
    fn variables_of(&self, context: &CompilationContext, item: &ConstraintItem) -> Vec<Rc<str>> {
        let mut variables = vec![];
        for expr in &item.exprs {
            if let Some(Operand::Variable(name)) = self.operand(context, expr) {
                variables.push(name);
                continue;
            }
            if let Some(operands) = self.array_operands(context, expr) {
                for operand in operands {
                    if let Operand::Variable(name) = operand {
                        variables.push(name);
                    }
                }
            }
        }
        variables.sort();
        variables.dedup();
        variables
    }

    /// Every posted constraint mentioning a view is rewritten to the views, stated where the
    /// checker holds it as linear.
    fn rewrite_posted_constraints(&mut self, context: &CompilationContext) {
        for (tag, item) in context.constraints.iter() {
            let number = tag_number(*tag);
            let variables = self.variables_of(context, item);
            let replacements: Vec<Replacement> = variables
                .iter()
                .filter_map(|variable| {
                    let (representative, view) = context.equivalences.view(variable);
                    if representative.as_ref() == variable.as_ref() {
                        return None;
                    }
                    Some(Replacement {
                        variable: Rc::clone(variable),
                        replacing: Rc::clone(&representative),
                        scale: i64::from(view.scale),
                        offset: i64::from(view.offset),
                        evidence: self.evidence[variable],
                    })
                })
                .collect();
            if replacements.is_empty() {
                continue;
            }
            let stated = match self.forms.get(&number) {
                Some(form) => Some(form.substitute(&replacements)),
                None => self
                    .linear_form_of(context, item)
                    .map(|form| form.substitute(&replacements)),
            };
            if let Some(form) = stated.clone() {
                let _ = self.forms.insert(number, form);
            }
            self.events.push(ViewEvent::Substitute {
                target: number,
                replacements,
                stated,
            });
        }

        // The declared domains, rewritten by every view: a member's declaration becomes bounds
        // on the representative, rounded inward, which is the domain the representative is
        // given. The result need not be stated.
        let mut variables: Vec<Rc<str>> = context.equivalences.classes.keys().cloned().collect();
        variables.sort();
        let replacements: Vec<Replacement> = variables
            .iter()
            .filter_map(|variable| {
                let (representative, view) = context.equivalences.view(variable);
                if representative.as_ref() == variable.as_ref() {
                    return None;
                }
                Some(Replacement {
                    variable: Rc::clone(variable),
                    replacing: Rc::clone(&representative),
                    scale: i64::from(view.scale),
                    offset: i64::from(view.offset),
                    evidence: self.evidence[variable],
                })
            })
            .collect();
        if !replacements.is_empty() {
            self.events.push(ViewEvent::Substitute {
                target: 0,
                replacements,
                stated: None,
            });
        }
    }
}

enum Plan {
    Absorb {
        target: Rc<str>,
        absorbed: Rc<str>,
        view: (i64, i64),
    },
    Parametrise {
        g: i64,
    },
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 { a } else { gcd(b, a % b) }
}

/// `u`, `v` with `u * a + v * b = gcd(a, b)`.
fn bezout(a: i64, b: i64) -> (i64, i64) {
    if b == 0 {
        return (a.signum(), 0);
    }
    let (u, v) = bezout(b, a.rem_euclid(b));
    // gcd = u * b + v * (a - b * floor(a / b))
    (v, u - v * a.div_euclid(b))
}

/// `a * x + b * y = c` as `(x, a, y, b, c)`.
type TwoVariableEquality = (Rc<str>, i64, Rc<str>, i64, i64);
