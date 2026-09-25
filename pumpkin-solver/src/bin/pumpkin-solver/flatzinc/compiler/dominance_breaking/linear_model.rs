//! The linear part of the model, as far as dominance detection needs it.
//!
//! A variable is a candidate only if every one of its occurrences is a term of a plain linear
//! constraint. Any other occurrence (a reified linear constraint, a clause, a global constraint)
//! makes it ineligible, because the dominance argument is only established for linear terms.
//!
//! For example, `maximise p` with `p = 5a + 5b + 4c` reaches FlatZinc as
//! `int_lin_eq([5, 5, 4, -1], [a, b, c, p], 0)`. That constraint defines the objective, so it
//! gives `a` the objective weight 5 and no term. The knapsack constraint
//! `int_lin_le([2, 3, 3], [a, b, c], 5)` gives `a` the coefficient 2 in that constraint. Under
//! `minimise p` the objective weight of `a` is -5, so a larger objective weight is still better.
//! If `a` also occurred in `int_le_reif(a, 0, r)`, it would not be a candidate.

use std::collections::BTreeMap;

use flatzinc::Goal;
use flatzinc::OptimizationType;

use super::super::context::Domain;
use super::super::symmetry_breaking::canonical_form::Graph;
use super::super::symmetry_breaking::canonical_form::ModelIndex;
use super::super::symmetry_breaking::canonical_form::SlotKey;
use super::super::symmetry_breaking::canonical_form::VarIndex;
use super::super::symmetry_breaking::canonical_form::Variable;

/// The relation of a plain linear constraint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Relation {
    /// `sum <= rhs`: moving weight to a term with a smaller coefficient keeps it satisfied.
    LessOrEqual,
    /// `sum = rhs` or `sum != rhs`: the sum must not change, so coefficients must be equal.
    Equal,
}

fn relation_of(id: &str) -> Option<Relation> {
    match id {
        "int_lin_le" | "bool_lin_le" => Some(Relation::LessOrEqual),
        "int_lin_eq" | "int_lin_ne" | "bool_lin_eq" => Some(Relation::Equal),
        _ => None,
    }
}

#[derive(Clone, Debug)]
pub(super) struct Candidate {
    pub(super) var: VarIndex,
    /// Summed coefficient per linear constraint the variable occurs in, excluding the
    /// constraint that defines the objective.
    pub(super) terms: BTreeMap<usize, i64>,
    /// Contribution to the objective, oriented so that a larger value is better. Zero for
    /// satisfaction problems and for variables that do not occur in the objective.
    pub(super) objective_weight: i64,
}

pub(super) struct LinearModel {
    pub(super) candidates: Vec<Candidate>,
    /// The relation of each constraint, `None` for constraints that are not plain linear.
    pub(super) relations: Vec<Option<Relation>>,
}

impl LinearModel {
    pub(super) fn extract(graph: &Graph, index: &ModelIndex, goal: &Goal) -> LinearModel {
        let relations: Vec<Option<Relation>> = graph
            .constraints
            .iter()
            .map(|constraint| relation_of(&constraint.id))
            .collect();

        let sense: i64 = match goal {
            Goal::OptimizeBool(OptimizationType::Maximize, _)
            | Goal::OptimizeInt(OptimizationType::Maximize, _) => 1,
            Goal::OptimizeBool(OptimizationType::Minimize, _)
            | Goal::OptimizeInt(OptimizationType::Minimize, _) => -1,
            _ => 0,
        };

        // The objective definition is the only linear equality containing the objective
        // variable: `a_o * o + sum(a_i * x_i) = r`, so `o` changes by `-a_i / a_o` per unit of
        // `x_i`. If there is no unique such equality, every constraint is treated as an ordinary
        // one, which only admits pairs that leave the objective unchanged.
        let definition = index.objective.and_then(|objective| {
            let mut defining = graph.incident[objective]
                .iter()
                .map(|&(constraint, _)| constraint)
                .filter(|&constraint| graph.constraints[constraint].id.as_ref() == "int_lin_eq");
            let first = defining.next()?;
            if defining.any(|constraint| constraint != first) {
                return None;
            }
            let coefficient: i64 = graph.constraints[first]
                .slots
                .iter()
                .filter(|&&(_, var)| var == objective)
                .filter_map(|&(key, _)| coefficient_of(key))
                .sum();
            (coefficient != 0).then_some((first, coefficient))
        });

        let mut candidates = Vec::new();
        'variables: for (var, occurrences) in graph.incident.iter().enumerate() {
            if occurrences.is_empty()
                || Some(var) == index.objective
                || is_fixed(&index.variables[var])
            {
                continue;
            }
            let mut terms = BTreeMap::new();
            let mut objective_weight = 0;
            for &(constraint, key) in occurrences {
                let (Some(_), Some(coefficient)) = (relations[constraint], coefficient_of(key))
                else {
                    continue 'variables;
                };
                match definition {
                    Some((defining, objective_coefficient)) if defining == constraint => {
                        objective_weight -= coefficient * objective_coefficient.signum() * sense;
                    }
                    _ => *terms.entry(constraint).or_insert(0) += coefficient,
                }
            }
            candidates.push(Candidate {
                var,
                terms,
                objective_weight,
            });
        }

        LinearModel {
            candidates,
            relations,
        }
    }
}

fn coefficient_of(key: SlotKey) -> Option<i64> {
    match key {
        SlotKey::Coefficient { coefficient, .. } => Some(coefficient),
        _ => None,
    }
}

fn is_fixed(variable: &Variable) -> bool {
    match &variable.domain {
        Domain::IntervalDomain { lb, ub } => lb == ub,
        Domain::SparseDomain { values } => values.len() <= 1,
    }
}
