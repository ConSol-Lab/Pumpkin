//! The lex-leader steps of a croc proof: for a symmetry of 0/1 variables, the step that adds the
//! lex-leader constraint of its support, which is posted as the `lex_less_or_equal` propagator.

use std::collections::HashMap;
use std::rc::Rc;

use pumpkin_solver::core::constraints::Constraint;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::proof::HeadField;
use pumpkin_solver::core::variables::DomainId;

use super::super::breaking_steps::LINEAR_CONSTRAINTS;
use super::super::breaking_steps::affine_fields;
use super::super::breaking_steps::aliases_of;
use super::super::breaking_steps::log;
use super::super::breaking_steps::log_order_key;
use super::super::breaking_steps::record_posted;
use super::super::breaking_steps::take_identifier;
use super::super::context::CompilationContext;
use super::super::context::Domain;
use super::candidate_permutations::Permutation;
use super::canonical_form::CanonicalConstraint;
use super::canonical_form::Graph;
use super::canonical_form::ModelIndex;
use super::canonical_form::VarIndex;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::preamble::Posted;

/// A symmetry the proof states, with each constraint it moves and the constraint it maps onto, as
/// numbers.
pub(super) struct LoggedSymmetry {
    permutation: Permutation,
    mapped: Vec<(usize, usize)>,
}

pub(super) struct SymmetryProof<'graph> {
    positions: HashMap<&'graph CanonicalConstraint, usize>,
    aliases: Vec<Vec<(Rc<str>, i64, i64)>>,
    logged: Vec<LoggedSymmetry>,
}

impl<'graph> SymmetryProof<'graph> {
    pub(super) fn new(
        ast: &FlatZincAst,
        index: &ModelIndex,
        graph: &'graph Graph,
        context: &mut CompilationContext,
    ) -> Self {
        let mut positions = HashMap::new();
        for (position, constraint) in graph.constraints.iter().enumerate() {
            let _ = positions.entry(constraint).or_insert(position);
        }
        SymmetryProof {
            positions,
            aliases: aliases_of(ast, index, context),
            logged: vec![],
        }
    }

    /// Keep the symmetry for the proof if the checker decides its step: its variables are 0/1
    /// and every constraint it moves is linear, atomic, a clause or a half reification of those.
    pub(super) fn keep(
        &mut self,
        ast: &FlatZincAst,
        index: &ModelIndex,
        graph: &Graph,
        permutation: &Permutation,
    ) -> bool {
        let support = permutation.support();
        let is_binary = |var: VarIndex| match &index.variables[var].domain {
            Domain::IntervalDomain { lb, ub } => (*lb, *ub) == (0, 1),
            Domain::SparseDomain { values } => {
                values.len() == 2 && values.contains(&0) && values.contains(&1)
            }
        };
        if !support.iter().all(|&var| is_binary(var)) {
            return false;
        }
        let mut moved: Vec<usize> = support
            .iter()
            .flat_map(|&var| graph.incident[var].iter().map(|&(position, _)| position))
            .collect();
        moved.sort_unstable();
        moved.dedup();
        let mut mapped = Vec::with_capacity(moved.len());
        for position in moved {
            if !LINEAR_CONSTRAINTS.contains(&ast.constraint_decls[position].id.as_str()) {
                return false;
            }
            let under_permutation = graph.constraints[position].map(&permutation.map);
            let Some(&target_position) = self.positions.get(&under_permutation) else {
                return false;
            };
            mapped.push((position, target_position));
        }
        self.logged.push(LoggedSymmetry {
            permutation: permutation.clone(),
            mapped,
        });
        true
    }

    /// Write the order and the steps, and post each lex-leader constraint.
    pub(super) fn log_and_post(&self, context: &mut CompilationContext, index: &ModelIndex) {
        let mut keys: Vec<VarIndex> = self
            .logged
            .iter()
            .flat_map(|symmetry| symmetry.permutation.support())
            .collect();
        keys.sort_unstable();
        keys.dedup();
        for var in keys {
            let domain = index.domain_ids[var];
            log_order_key(
                context,
                vec![
                    HeadField::Text("p ".to_owned()),
                    HeadField::Name(domain),
                    HeadField::Text(" smaller ".to_owned()),
                    HeadField::Atom(predicate![domain <= -1]),
                    HeadField::Text(" c:0 l:".to_owned()),
                    HeadField::Label("initial_domain"),
                ],
            );
        }
        for symmetry in &self.logged {
            let tag = context.solver.new_constraint_tag();
            self.log_step(context, index, symmetry, tag);
            let support = symmetry.permutation.support();
            let x: Vec<DomainId> = support.iter().map(|&var| index.domain_ids[var]).collect();
            let y: Vec<DomainId> = support
                .iter()
                .map(|&var| index.domain_ids[symmetry.permutation.map[var]])
                .collect();
            let names = |context: &CompilationContext, domains: &[DomainId]| -> Vec<String> {
                domains
                    .iter()
                    .map(|&domain| {
                        context
                            .solver
                            .render_head_line_as_text(&[HeadField::Name(domain)])
                    })
                    .collect()
            };
            let posted = Posted::LexLessOrEqual(names(context, &x), names(context, &y));
            record_posted(context, tag, posted);
            pumpkin_constraints::lex_less_or_equal(x, y, tag).post(context.solver);
        }
    }

    fn log_step(
        &self,
        context: &mut CompilationContext,
        index: &ModelIndex,
        symmetry: &LoggedSymmetry,
        tag: ConstraintTag,
    ) {
        let Some(identifier) = take_identifier(context, tag) else {
            return;
        };
        let map = &symmetry.permutation.map;
        let support = symmetry.permutation.support();
        let mut fields = vec![HeadField::Text(format!("y {identifier}"))];
        for &var in &support {
            fields.push(HeadField::Text(" ".to_owned()));
            fields.push(HeadField::Name(index.domain_ids[var]));
            fields.push(HeadField::Text(":".to_owned()));
            fields.push(HeadField::Name(index.domain_ids[map[var]]));
        }
        if support.iter().any(|&var| !self.aliases[var].is_empty()) {
            fields.push(HeadField::Text(" ;".to_owned()));
            for &var in &support {
                let replacing = index.domain_ids[map[var]];
                for (alias, scale, offset) in &self.aliases[var] {
                    fields.push(HeadField::Text(format!(" {alias}:=")));
                    fields.extend(affine_fields(*scale, replacing, *offset));
                }
            }
        }
        log(context, fields);
        // The declared domains map onto themselves: the moved variables are 0/1, and the
        // permutation exchanges their declarations.
        log(context, vec![HeadField::Text("  under c:0 c:0".to_owned())]);
        for &(position, target_position) in &symmetry.mapped {
            log(
                context,
                vec![HeadField::Text(format!(
                    "  under c:{} c:{}",
                    position + 1,
                    target_position + 1
                ))],
            );
        }
    }

    pub(super) fn logged(&self) -> usize {
        self.logged.len()
    }
}
