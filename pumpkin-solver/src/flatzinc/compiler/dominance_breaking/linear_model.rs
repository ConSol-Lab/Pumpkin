//! The linear part of the model, as far as dominance detection needs it.
//!
//! Every constraint with an exact linear form (see [`linear_forms`](super::linear_forms)) is
//! replaced by its rows. A variable is a candidate only if every constraint it occurs in has an
//! exact linear form; any other occurrence (a global constraint, a reified equality) makes it
//! ineligible. Rows that hold for every assignment within the bounds are dropped, since no
//! exchange or fixing can violate them.
//!
//! For example, `maximise p` with `p = 5a + 5b + 4c` reaches FlatZinc as
//! `int_lin_eq([5, 5, 4, -1], [a, b, c, p], 0)`. That constraint defines the objective, so it
//! gives `a` the objective weight 5 and no term. The knapsack constraint
//! `int_lin_le([2, 3, 3], [a, b, c], 5)` gives `a` the coefficient 2 in that row. Under
//! `minimise p` the objective weight of `a` is -5, so a larger objective weight is still better.
//! If `a` also occurred in `int_eq_reif(a, 0, r)`, it would not be a candidate.

use std::collections::BTreeMap;

use flatzinc::Goal;
use flatzinc::OptimizationType;

use super::super::context::Domain;
use super::super::symmetry_breaking::canonical_form::Element;
use super::super::symmetry_breaking::canonical_form::ModelIndex;
use super::super::symmetry_breaking::canonical_form::Resolved;
use super::super::symmetry_breaking::canonical_form::VarIndex;
use super::super::symmetry_breaking::canonical_form::Variable;
use super::linear_forms::Row;
use super::linear_forms::rows_of;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;

/// The relation of a linear row.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Relation {
    /// `sum <= rhs`: moving weight to a term with a smaller coefficient keeps it satisfied.
    LessOrEqual,
    /// `sum = rhs` or `sum != rhs`: the sum must not change, so coefficients must be equal.
    Equal,
}

#[derive(Clone, Debug)]
pub(super) struct Candidate {
    pub(super) var: VarIndex,
    /// Summed coefficient per row the variable occurs in, excluding the row that defines the
    /// objective.
    pub(super) terms: BTreeMap<usize, i64>,
    /// Contribution to the objective, oriented so that a larger value is better. Zero for
    /// satisfaction problems and for variables that do not occur in the objective.
    pub(super) objective_weight: i64,
}

pub(super) struct LinearModel {
    pub(super) candidates: Vec<Candidate>,
    /// The relation of each row.
    pub(super) relations: Vec<Relation>,
}

pub(super) fn bounds(variable: &Variable) -> (i64, i64) {
    match &variable.domain {
        Domain::IntervalDomain { lb, ub } => (i64::from(*lb), i64::from(*ub)),
        Domain::SparseDomain { values } => (
            values.iter().min().map_or(0, |&v| i64::from(v)),
            values.iter().max().map_or(0, |&v| i64::from(v)),
        ),
    }
}

fn variables_in<Arg: AsRef<Resolved>>(args: &[Arg]) -> impl Iterator<Item = VarIndex> + '_ {
    args.iter()
        .flat_map(|resolved| match resolved.as_ref() {
            Resolved::Scalar(element) => std::slice::from_ref(element),
            Resolved::Array(elements) => elements.as_slice(),
        })
        .filter_map(|element| match element {
            Element::Var(var) | Element::View { var, .. } => Some(*var),
            Element::Const(_) => None,
        })
}

impl LinearModel {
    pub(super) fn extract(ast: &FlatZincAst, index: &ModelIndex) -> Result<Self, FlatZincError> {
        let num_variables = index.variables.len();
        let variable_bounds = |var: VarIndex| bounds(&index.variables[var]);

        let mut rows: Vec<Row> = Vec::new();
        // Rows that come from an `int_lin_eq`, the only form an objective definition takes.
        let mut is_linear_equality = Vec::new();
        let mut occurs = vec![false; num_variables];
        let mut ineligible = vec![false; num_variables];

        for item in &ast.constraint_decls {
            let args: Vec<std::borrow::Cow<'_, Resolved>> = item
                .exprs
                .iter()
                .map(|expr| index.resolve(expr))
                .collect::<Result<_, _>>()?;
            let new_rows = rows_of(&item.id, &args, &variable_bounds);
            for var in variables_in(&args) {
                occurs[var] = true;
                ineligible[var] |= new_rows.is_none();
            }
            for row in new_rows.into_iter().flatten() {
                if !row.is_entailed(&variable_bounds) {
                    is_linear_equality.push(item.id == "int_lin_eq");
                    rows.push(row);
                }
            }
        }

        let sense: i64 = match &ast.solve_item.goal {
            Goal::OptimizeBool(OptimizationType::Maximize, _)
            | Goal::OptimizeInt(OptimizationType::Maximize, _) => 1,
            Goal::OptimizeBool(OptimizationType::Minimize, _)
            | Goal::OptimizeInt(OptimizationType::Minimize, _) => -1,
            _ => 0,
        };

        // The objective definition is the only linear equality containing the objective
        // variable: `a_o * o + sum(a_i * x_i) = r`, so `o` changes by `-a_i / a_o` per unit of
        // `x_i`. It is folded into objective weights only if the objective variable occurs in no
        // other constraint; its domain `lo..hi` then still restricts the sum, which is kept as
        // the rows `sum(a_i * x_i) <= r - m` and `-sum(a_i * x_i) <= M - r`, where `m..M` is the
        // range of `a_o * o`. Otherwise the definition stays an ordinary equality row and the
        // objective variable itself carries the objective weight.
        let mut objective_weight = vec![0_i64; num_variables];
        let mut definition = None;
        if let Some(objective) = index.objective {
            let containing: Vec<usize> = rows
                .iter()
                .enumerate()
                .filter(|(_, row)| row.terms.iter().any(|&(var, _)| var == objective))
                .map(|(i, _)| i)
                .collect();
            let foldable = match containing.as_slice() {
                [only] if is_linear_equality[*only] && !ineligible[objective] => Some(*only),
                _ => None,
            };
            match foldable {
                Some(i) => {
                    let row = &rows[i];
                    let objective_coefficient = row
                        .terms
                        .iter()
                        .find(|&&(var, _)| var == objective)
                        .map_or(0, |&(_, coefficient)| coefficient);
                    for &(var, coefficient) in &row.terms {
                        objective_weight[var] -=
                            coefficient * objective_coefficient.signum() * sense;
                    }
                    let others: Vec<(VarIndex, i64)> = row
                        .terms
                        .iter()
                        .copied()
                        .filter(|&(var, _)| var != objective)
                        .collect();
                    let (lower, upper) = bounds(&index.variables[objective]);
                    let (m, big_m) = (
                        (objective_coefficient * lower).min(objective_coefficient * upper),
                        (objective_coefficient * lower).max(objective_coefficient * upper),
                    );
                    let rhs = row.rhs;
                    definition = Some(i);
                    for bound_row in [
                        Row {
                            relation: Relation::LessOrEqual,
                            terms: others.clone(),
                            rhs: rhs - m,
                        },
                        Row {
                            relation: Relation::LessOrEqual,
                            terms: others.iter().map(|&(var, a)| (var, -a)).collect(),
                            rhs: big_m - rhs,
                        },
                    ] {
                        if !bound_row.is_entailed(&variable_bounds) {
                            is_linear_equality.push(false);
                            rows.push(bound_row);
                        }
                    }
                }
                None => objective_weight[objective] += sense,
            }
        }

        let mut terms: Vec<BTreeMap<usize, i64>> = vec![BTreeMap::new(); num_variables];
        for (i, row) in rows.iter().enumerate() {
            if Some(i) == definition {
                continue;
            }
            for &(var, coefficient) in &row.terms {
                let _ = terms[var].insert(i, coefficient);
            }
        }

        let candidates = (0..num_variables)
            .filter(|&var| {
                occurs[var]
                    && !ineligible[var]
                    && !(definition.is_some() && Some(var) == index.objective)
                    && !is_fixed(&index.variables[var])
            })
            .map(|var| Candidate {
                var,
                terms: std::mem::take(&mut terms[var]),
                objective_weight: objective_weight[var],
            })
            .collect();

        Ok(LinearModel {
            candidates,
            relations: rows.iter().map(|row| row.relation).collect(),
        })
    }
}

fn is_fixed(variable: &Variable) -> bool {
    let (lower, upper) = bounds(variable);
    lower == upper
}
