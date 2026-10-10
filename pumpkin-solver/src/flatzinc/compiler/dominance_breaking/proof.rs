//! The dominance steps of a croc proof for the fixings and pairs the pass posts.
//!
//! Only steps the checker decides are logged, and the pass posts only the logged ones: the
//! variables must occur only in linear or atomic constraints, clauses and half reifications of
//! those, a fixed variable must not occur in the objective, and a pair must be of 0/1
//! variables.

use std::collections::HashMap;
use std::rc::Rc;

use pumpkin_solver::core::predicate;
use pumpkin_solver::core::predicates::Predicate;
use pumpkin_solver::core::predicates::PredicateType;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::proof::HeadField;

use super::super::breaking_steps::LINEAR_CONSTRAINTS;
use super::super::breaking_steps::affine_fields;
use super::super::breaking_steps::aliases_of;
use super::super::breaking_steps::log;
use super::super::breaking_steps::log_order_key;
use super::super::breaking_steps::take_identifier;
use super::super::context::CompilationContext;
use super::super::symmetry_breaking::canonical_form::Constant;
use super::super::symmetry_breaking::canonical_form::Element;
use super::super::symmetry_breaking::canonical_form::ModelIndex;
use super::super::symmetry_breaking::canonical_form::Resolved;
use super::super::symmetry_breaking::canonical_form::VarIndex;
use super::dual_fixing::FixedBound;
use super::linear_model::bounds;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::preamble::Atom;
use crate::flatzinc::preamble::Comparator;

/// The definition of the objective variable, `sum(a_i * x_i) + a_o * o = rhs`, the only
/// constraint it occurs in.
struct ObjectiveDefinition {
    objective: VarIndex,
    /// The number of the constraint in the proof.
    number: u32,
    objective_coefficient: i64,
    coefficients: Vec<(VarIndex, i64)>,
    rhs: i64,
}

pub(super) struct DominanceProof {
    /// Whether every constraint the variable occurs in is read as linear or atomic.
    readable: Vec<bool>,
    /// A constraint that makes the variable unreadable, for the log.
    blocking: Vec<Option<String>>,
    occurrences: Vec<usize>,
    /// For each variable, the other variables of the model the compiler made views of it, with
    /// the scale and offset: the proof keeps them apart, so a witness moves them along with it.
    aliases: Vec<Vec<(Rc<str>, i64, i64)>>,
    /// For each variable, the variables of the half-reified constraints it is the indicator of.
    under_indicator: Vec<Vec<VarIndex>>,
    definition: Option<ObjectiveDefinition>,
    /// For a variable, its value and whether the objective is bounded above, the identifier of
    /// the nogood that bounds the objective when the variable has the value.
    objective_bounds: HashMap<(VarIndex, i64, bool), (u32, i64)>,
}

fn elements(resolved: &Resolved) -> &[Element] {
    match resolved {
        Resolved::Scalar(element) => std::slice::from_ref(element),
        Resolved::Array(elements) => elements.as_slice(),
    }
}

pub(crate) fn int_value(value: i64) -> i32 {
    i32::try_from(value).expect("bounds come from i32 domains")
}

impl DominanceProof {
    pub(super) fn new(
        ast: &FlatZincAst,
        index: &ModelIndex,
        context: &mut CompilationContext,
    ) -> Result<Self, FlatZincError> {
        let num_variables = index.variables.len();
        let aliases = aliases_of(ast, index, context);
        let mut readable = vec![true; num_variables];
        let mut blocking: Vec<Option<String>> = vec![None; num_variables];
        let mut occurrences = vec![0; num_variables];
        let mut under_indicator = vec![vec![]; num_variables];
        let mut definitions = Vec::new();

        for (position, item) in ast.constraint_decls.iter().enumerate() {
            let args: Vec<_> = item
                .exprs
                .iter()
                .map(|expr| index.resolve(expr))
                .collect::<Result<_, _>>()?;
            let is_linear = LINEAR_CONSTRAINTS.contains(&item.id.as_str());
            for arg in &args {
                for element in elements(arg) {
                    if let Element::Var(var) | Element::View { var, .. } = element {
                        readable[*var] &= is_linear;
                        if !is_linear && blocking[*var].is_none() {
                            blocking[*var] = Some(item.id.clone());
                        }
                        occurrences[*var] += 1;
                    }
                }
            }
            if item.id.ends_with("_imp")
                && let Some((indicator, inner)) = args.split_last()
                && let Resolved::Scalar(Element::Var(indicator)) = indicator.as_ref()
            {
                for arg in inner {
                    for element in elements(arg) {
                        if let Element::Var(var) | Element::View { var, .. } = element {
                            under_indicator[*indicator].push(*var);
                        }
                    }
                }
            }
            let Some(objective) = index.objective else {
                continue;
            };
            if item.id != "int_lin_eq" || args.len() != 3 {
                continue;
            }
            let (Resolved::Array(weights), Resolved::Array(terms), Resolved::Scalar(rhs)) =
                (args[0].as_ref(), args[1].as_ref(), args[2].as_ref())
            else {
                continue;
            };
            let Element::Const(Constant::Int(mut rhs)) = *rhs else {
                continue;
            };
            let mut coefficients: HashMap<VarIndex, i64> = HashMap::new();
            let mut exact = weights.len() == terms.len();
            for (weight, term) in weights.iter().zip(terms) {
                match (weight, term) {
                    (Element::Const(Constant::Int(a)), Element::Var(var)) => {
                        *coefficients.entry(*var).or_insert(0) += a;
                    }
                    (Element::Const(Constant::Int(a)), Element::View { var, scale, offset }) => {
                        *coefficients.entry(*var).or_insert(0) += a * scale;
                        rhs -= a * offset;
                    }
                    (Element::Const(Constant::Int(a)), Element::Const(Constant::Int(value))) => {
                        rhs -= a * value;
                    }
                    _ => exact = false,
                }
            }
            let Some(objective_coefficient) = coefficients.remove(&objective) else {
                continue;
            };
            if !exact || objective_coefficient == 0 {
                continue;
            }
            let mut coefficients: Vec<_> = coefficients.into_iter().collect();
            coefficients.sort_unstable();
            definitions.push(ObjectiveDefinition {
                objective,
                number: u32::try_from(position + 1).expect("fewer constraints than u32::MAX"),
                objective_coefficient,
                coefficients,
                rhs,
            });
        }

        let definition = match definitions.len() {
            1 if occurrences[definitions[0].objective] == 1 => definitions.pop(),
            _ => None,
        };
        Ok(DominanceProof {
            readable,
            blocking,
            occurrences,
            aliases,
            under_indicator,
            definition,
            objective_bounds: HashMap::new(),
        })
    }

    fn weight_in_definition(&self, var: VarIndex) -> i64 {
        self.definition.as_ref().map_or(0, |definition| {
            definition
                .coefficients
                .iter()
                .find(|&&(other, _)| other == var)
                .map_or(0, |&(_, a)| a)
        })
    }

    /// Whether the checker decides the fixing of `var`.
    pub(super) fn can_fix(&self, index: &ModelIndex, var: VarIndex) -> bool {
        self.readable[var]
            && self.occurrences[var] > 0
            && index.objective != Some(var)
            && self.weight_in_definition(var) == 0
    }

    /// The shift of the objective variable that repairs its definition when `better` goes from
    /// 0 to 1 and `worse` from 1 to 0, if the checker decides the exchange.
    pub(super) fn exchange_shift(
        &self,
        index: &ModelIndex,
        better: VarIndex,
        worse: VarIndex,
    ) -> Option<i64> {
        let is_binary = |var: VarIndex| bounds(&index.variables[var]) == (0, 1);
        if !(self.readable[better] && self.readable[worse]) {
            log::debug!(
                "dominance proof: pair dropped, it occurs in {:?}",
                self.blocking[better]
                    .as_ref()
                    .or(self.blocking[worse].as_ref())
            );
            return None;
        }
        if !(is_binary(better) && is_binary(worse)) {
            log::debug!("dominance proof: pair dropped, not 0/1 variables");
            return None;
        }
        if index.objective == Some(better) || index.objective == Some(worse) {
            return None;
        }
        let Some(definition) = &self.definition else {
            return Some(0);
        };
        let change = self.weight_in_definition(better) - self.weight_in_definition(worse);
        if change == 0 {
            return Some(0);
        }
        // `a_o * o` changes by `-change`.
        if change % definition.objective_coefficient != 0 {
            return None;
        }
        let shift = -change / definition.objective_coefficient;
        let (lower, upper) = bounds(&index.variables[definition.objective]);
        self.fixed_for_bound(index, better, worse, shift, lower, upper)
            .map(|_| shift)
    }

    /// Which of `better = 0` and `worse = 1` bounds the objective enough for the shift to keep
    /// it in its domain.
    fn fixed_for_bound(
        &self,
        index: &ModelIndex,
        better: VarIndex,
        worse: VarIndex,
        shift: i64,
        lower: i64,
        upper: i64,
    ) -> Option<(VarIndex, i64)> {
        [(better, 0), (worse, 1)].into_iter().find(|&(var, value)| {
            let (least, most) = self.objective_range(index, var, value);
            if shift > 0 {
                most + shift <= upper
            } else {
                least + shift >= lower
            }
        })
    }

    /// The range of a variable when `fixed` has the value.
    fn range_when_fixed(
        &self,
        index: &ModelIndex,
        var: VarIndex,
        fixed: VarIndex,
        value: i64,
    ) -> (i64, i64) {
        if var == fixed {
            (value, value)
        } else {
            bounds(&index.variables[var])
        }
    }

    /// The bounds of the objective variable its definition gives when `fixed` has the value.
    fn objective_range(&self, index: &ModelIndex, fixed: VarIndex, value: i64) -> (i64, i64) {
        let definition = self.definition.as_ref().expect("the objective is defined");
        let (mut least, mut most) = (0_i64, 0_i64);
        for &(var, a) in &definition.coefficients {
            let (lower, upper) = self.range_when_fixed(index, var, fixed, value);
            least += (a * lower).min(a * upper);
            most += (a * lower).max(a * upper);
        }
        let a_o = definition.objective_coefficient;
        let (first, second) = (definition.rhs - least, definition.rhs - most);
        let (low, high) = (first.min(second), first.max(second));
        if a_o > 0 {
            (
                low.div_euclid(a_o) + i64::from(low.rem_euclid(a_o) != 0),
                high.div_euclid(a_o),
            )
        } else {
            let a = -a_o;
            let (low, high) = (-high, -low);
            (
                low.div_euclid(a) + i64::from(low.rem_euclid(a) != 0),
                high.div_euclid(a),
            )
        }
    }

    /// Write the order: first the fixed variables, each preferring the bound it is fixed to,
    /// then the variables of the pairs in `pair_order`, each preferring larger values.
    pub(super) fn log_order(
        &self,
        context: &mut CompilationContext,
        index: &ModelIndex,
        fixings: &[(VarIndex, FixedBound)],
        pair_order: &[VarIndex],
    ) {
        let keys = fixings
            .iter()
            .copied()
            .chain(pair_order.iter().map(|&var| (var, FixedBound::Upper)));
        for (var, side) in keys {
            let domain = index.domain_ids[var];
            let (lower, upper) = bounds(&index.variables[var]);
            let (preference, bound) = match side {
                FixedBound::Lower => ("smaller", predicate![domain <= int_value(lower) - 1]),
                FixedBound::Upper => ("larger", predicate![domain >= int_value(upper) + 1]),
            };
            log_order_key(
                context,
                vec![
                    HeadField::Text("p ".to_owned()),
                    HeadField::Name(domain),
                    HeadField::Text(format!(" {preference} ")),
                    HeadField::Atom(bound),
                    HeadField::Text(" c:0 l:".to_owned()),
                    HeadField::Label("initial_domain"),
                ],
            );
        }
    }

    /// Write the step that fixes `var` to `side`, as the constraint with the tag.
    pub(super) fn log_fixing(
        &self,
        context: &mut CompilationContext,
        index: &ModelIndex,
        var: VarIndex,
        side: FixedBound,
        tag: ConstraintTag,
    ) {
        let Some(identifier) = take_identifier(context, tag) else {
            return;
        };
        let domain = index.domain_ids[var];
        let (lower, upper) = bounds(&index.variables[var]);
        let (lower, upper) = (int_value(lower), int_value(upper));
        let (removed, value, other_side) = match side {
            FixedBound::Lower => (
                predicate![domain >= lower + 1],
                lower,
                predicate![domain >= upper + 1],
            ),
            FixedBound::Upper => (
                predicate![domain <= upper - 1],
                upper,
                predicate![domain <= lower - 1],
            ),
        };
        log(
            context,
            vec![
                HeadField::Text(format!("w {identifier} ")),
                HeadField::Atom(removed),
                HeadField::Text(" : ".to_owned()),
                HeadField::Name(domain),
                HeadField::Text(format!(":={value}")),
                HeadField::Text(
                    self.aliases[var]
                        .iter()
                        .map(|(alias, scale, offset)| {
                            format!(" {alias}:={}", scale * i64::from(value) + offset)
                        })
                        .collect(),
                ),
            ],
        );
        log_domain_fact(context, other_side);
        // A constraint `var` is the indicator of may hold for every value its variables can
        // take, which is why fixing `var` to 1 keeps it.
        for &inner in &self.under_indicator[var] {
            let domain = index.domain_ids[inner];
            let (lower, upper) = bounds(&index.variables[inner]);
            log_domain_fact(context, predicate![domain <= int_value(lower) - 1]);
            log_domain_fact(context, predicate![domain >= int_value(upper) + 1]);
        }
    }

    /// Write the step that adds `worse <= better` for two 0/1 variables, as the constraint with
    /// the tag, where the objective variable shifts by `shift`.
    pub(super) fn log_exchange(
        &mut self,
        context: &mut CompilationContext,
        index: &ModelIndex,
        better: VarIndex,
        worse: VarIndex,
        shift: i64,
        tag: ConstraintTag,
    ) {
        if !context.solver.is_logging_proof() {
            return;
        }
        let objective = self
            .definition
            .as_ref()
            .filter(|_| shift != 0)
            .map(|definition| index.domain_ids[definition.objective]);
        let cited_bound = objective.map(|_| {
            let definition = self.definition.as_ref().expect("the objective is defined");
            let (lower, upper) = bounds(&index.variables[definition.objective]);
            let (fixed, value) = self
                .fixed_for_bound(index, better, worse, shift, lower, upper)
                .expect("the pair was chosen because a bound exists");
            let (nogood, excluded) = self.objective_bound(context, index, fixed, value, shift > 0);
            (nogood, excluded, fixed, value)
        });

        let identifier = take_identifier(context, tag).expect("a proof is logged");
        let better_domain = index.domain_ids[better];
        let worse_domain = index.domain_ids[worse];
        let mut fields = vec![
            HeadField::Text(format!("w {identifier} ")),
            HeadField::Atom(predicate![worse_domain >= 1]),
            HeadField::Text(" ".to_owned()),
            HeadField::Atom(predicate![better_domain <= 0]),
            HeadField::Text(" : ".to_owned()),
            HeadField::Name(better_domain),
            HeadField::Text(":=".to_owned()),
            HeadField::Name(worse_domain),
            HeadField::Text(" ".to_owned()),
            HeadField::Name(worse_domain),
            HeadField::Text(":=".to_owned()),
            HeadField::Name(better_domain),
        ];
        for (moved, replacing) in [(better, worse_domain), (worse, better_domain)] {
            for (alias, scale, offset) in &self.aliases[moved] {
                fields.push(HeadField::Text(format!(" {alias}:=")));
                fields.extend(affine_fields(*scale, replacing, *offset));
            }
        }
        if let Some(objective) = objective {
            fields.extend([
                HeadField::Text(" ".to_owned()),
                HeadField::Name(objective),
                HeadField::Text(":=".to_owned()),
                HeadField::Name(objective),
                HeadField::Text(format!("{shift:+}")),
            ]);
        }
        log(context, fields);
        log_domain_fact(context, predicate![worse_domain >= 2]);
        log_domain_fact(context, predicate![better_domain <= -1]);

        let (Some(objective), Some((nogood, excluded, fixed, value))) = (objective, cited_bound)
        else {
            return;
        };
        let definition = self.definition.as_ref().expect("the objective is defined");
        let (lower, upper) = bounds(&index.variables[definition.objective]);
        let (lower, upper) = (int_value(lower), int_value(upper));
        log_domain_fact(context, predicate![objective <= lower - 1]);
        log_domain_fact(context, predicate![objective >= upper + 1]);
        log(
            context,
            vec![
                HeadField::Text("  ".to_owned()),
                HeadField::Atom(excluded),
                HeadField::Text(" ".to_owned()),
                HeadField::Atom(fixed_atom(index.domain_ids[fixed], value)),
                HeadField::Text(format!(" c:{nogood} l:")),
                HeadField::Label("nogood"),
            ],
        );
    }

    /// The nogood that bounds the objective when `fixed` has the value (0 or 1), above if
    /// `above` and below otherwise, and the atom it excludes: `[o >= M + 1] /\ [fixed <= 0]`
    /// for the largest value `M` the definition allows, say. It is refuted once, by the
    /// definition and the declared domains, and cited by every exchange that fixes `fixed` so.
    fn objective_bound(
        &mut self,
        context: &mut CompilationContext,
        index: &ModelIndex,
        fixed: VarIndex,
        value: i64,
        above: bool,
    ) -> (u32, Predicate) {
        let definition = self.definition.as_ref().expect("the objective is defined");
        let objective = index.domain_ids[definition.objective];
        let (least, most) = self.objective_range(index, fixed, value);
        let excluded = if above {
            predicate![objective >= int_value(most) + 1]
        } else {
            predicate![objective <= int_value(least) - 1]
        };
        if let Some(&(identifier, _)) = self.objective_bounds.get(&(fixed, value, above)) {
            return (identifier, excluded);
        }

        let tag = context.solver.new_constraint_tag();
        let identifier = take_identifier(context, tag).expect("a proof is logged");
        let fixed_domain = index.domain_ids[fixed];
        let assumed = fixed_atom(fixed_domain, value);
        log(
            context,
            vec![
                HeadField::Text(format!("g {identifier} ")),
                HeadField::Atom(excluded),
                HeadField::Text(" ".to_owned()),
                HeadField::Atom(assumed),
            ],
        );
        // Each variable at the bound that moves the objective towards `excluded`; `o` grows with
        // `var` when `-a / a_o > 0`.
        let mut premises = Vec::with_capacity(definition.coefficients.len() + 1);
        for &(var, a) in &definition.coefficients {
            if a == 0 {
                continue;
            }
            let domain = index.domain_ids[var];
            let grows = (a > 0) != (definition.objective_coefficient > 0);
            let (var_lower, var_upper) = self.range_when_fixed(index, var, fixed, value);
            let premise = if grows == above {
                predicate![domain <= int_value(var_upper)]
            } else {
                predicate![domain >= int_value(var_lower)]
            };
            if premise != assumed {
                log_domain_fact(context, !premise);
            }
            premises.push(premise);
        }
        premises.push(excluded);
        let mut fields = vec![HeadField::Text("  ".to_owned())];
        for premise in premises {
            fields.push(HeadField::Atom(premise));
            fields.push(HeadField::Text(" ".to_owned()));
        }
        fields.push(HeadField::Text(format!("c:{} l:", definition.number)));
        fields.push(HeadField::Label("linear_bounds"));
        log(context, fields);

        let _ = self
            .objective_bounds
            .insert((fixed, value, above), (identifier, 0));
        (identifier, excluded)
    }
}

/// The atom a 0/1 variable with the value satisfies and a step assumes.
fn fixed_atom(domain: pumpkin_solver::core::variables::DomainId, value: i64) -> Predicate {
    if value == 0 {
        predicate![domain <= 0]
    } else {
        predicate![domain >= 1]
    }
}

/// Cite that the negation of `atom` holds by the declared domains.
pub(crate) fn log_domain_fact(context: &mut CompilationContext, atom: Predicate) {
    log(
        context,
        vec![
            HeadField::Text("  ".to_owned()),
            HeadField::Atom(atom),
            HeadField::Text(" c:0 l:".to_owned()),
            HeadField::Label("initial_domain"),
        ],
    );
}

/// The atoms as a preamble states them.
pub(crate) fn preamble_atoms(context: &CompilationContext, predicates: &[Predicate]) -> Vec<Atom> {
    predicates
        .iter()
        .map(|&predicate| {
            let name = context
                .solver
                .render_head_line_as_text(&[HeadField::Name(predicate.get_domain())]);
            let comparator = match predicate.get_predicate_type() {
                PredicateType::LowerBound => Comparator::GreaterOrEqual,
                PredicateType::UpperBound => Comparator::LessOrEqual,
                PredicateType::Equal => Comparator::Equal,
                PredicateType::NotEqual => Comparator::NotEqual,
            };
            Atom {
                variable: name,
                comparator,
                value: i64::from(predicate.get_right_hand_side()),
            }
        })
        .collect()
}

/// An order of the variables of the pairs where `better` precedes `worse` in every pair, if the
/// pairs have no cycle.
pub(super) fn order_of_pairs(pairs: &[(VarIndex, VarIndex)]) -> Option<Vec<VarIndex>> {
    let mut successors: HashMap<VarIndex, Vec<VarIndex>> = HashMap::new();
    let mut predecessors: HashMap<VarIndex, usize> = HashMap::new();
    let mut variables = Vec::new();
    for &(better, worse) in pairs {
        for var in [better, worse] {
            if let std::collections::hash_map::Entry::Vacant(entry) = predecessors.entry(var) {
                let _ = entry.insert(0);
                variables.push(var);
            }
        }
        successors.entry(better).or_default().push(worse);
        *predecessors.get_mut(&worse).unwrap() += 1;
    }
    variables.sort_unstable();
    let mut ready: Vec<VarIndex> = variables
        .iter()
        .copied()
        .filter(|var| predecessors[var] == 0)
        .rev()
        .collect();
    let mut order = Vec::with_capacity(variables.len());
    while let Some(var) = ready.pop() {
        order.push(var);
        for &next in successors.get(&var).map_or(&[][..], Vec::as_slice) {
            let count = predecessors.get_mut(&next).unwrap();
            *count -= 1;
            if *count == 0 {
                ready.push(next);
            }
        }
    }
    (order.len() == variables.len()).then_some(order)
}
