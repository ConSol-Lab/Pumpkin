//! Exact linear forms of FlatZinc constraints.
//!
//! A constraint has an exact linear form when, on the bounded domains of its variables, it is
//! equivalent to a conjunction of linear rows `sum <= rhs` or `sum = rhs` (`sum != rhs` is read as
//! a row whose sum must not change, see [`Relation::Equal`]). Because the rows are equivalent to
//! the constraint, an exchange or a fixing that preserves every row preserves the constraint.
//!
//! Besides the linear constraints themselves this covers clauses, the Boolean gates, binary
//! comparisons, and half- and fully reified linear inequalities, which are written with big-M
//! coefficients computed from the variable bounds. A constraint without an exact linear form
//! returns `None`, and its variables are then not considered by dominance detection.
//!
//! For example, `bool_clause([a, b], [c])`, which is `a \/ b \/ ~c`, becomes the row
//! `-a - b + c <= 0`. The half reification `int_lin_le_imp([1], [x], 4, r)`, which is
//! `r -> x <= 4`, becomes `x + 6r <= 10` when `x` is in `0..10`: for `r = 1` it is `x <= 4`, and
//! for `r = 0` it is `x <= 10`, which holds anyway.

use super::super::symmetry_breaking::canonical_form::Constant;
use super::super::symmetry_breaking::canonical_form::Element;
use super::super::symmetry_breaking::canonical_form::Resolved;
use super::super::symmetry_breaking::canonical_form::VarIndex;
use super::linear_model::Relation;

/// One linear row over the model's variables, with duplicate variables combined.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Row {
    pub(super) relation: Relation,
    pub(super) terms: Vec<(VarIndex, i64)>,
    pub(super) rhs: i64,
}

impl Row {
    fn new(relation: Relation, expression: &Expression, rhs: i64) -> Option<Row> {
        let mut terms = expression.terms.clone();
        terms.sort_unstable();
        let mut combined: Vec<(VarIndex, i64)> = Vec::with_capacity(terms.len());
        for (var, coefficient) in terms {
            match combined.last_mut() {
                Some((last, sum)) if *last == var => *sum = sum.checked_add(coefficient)?,
                _ => combined.push((var, coefficient)),
            }
        }
        combined.retain(|&(_, coefficient)| coefficient != 0);
        Some(Row {
            relation,
            terms: combined,
            rhs: rhs.checked_sub(expression.constant)?,
        })
    }

    /// Whether a `<=` row holds for every assignment within the bounds.
    pub(super) fn is_entailed(&self, bounds: &impl Fn(VarIndex) -> (i64, i64)) -> bool {
        self.relation == Relation::LessOrEqual
            && maximum(&self.terms, bounds).is_some_and(|maximum| maximum <= self.rhs)
    }
}

/// A linear expression `sum(terms) + constant`.
#[derive(Clone, Debug, Default)]
struct Expression {
    terms: Vec<(VarIndex, i64)>,
    constant: i64,
}

impl Expression {
    fn add(&mut self, coefficient: i64, element: &Element) -> Option<()> {
        match element {
            Element::Var(var) => self.terms.push((*var, coefficient)),
            Element::View { var, scale, offset } => {
                self.terms.push((*var, coefficient.checked_mul(*scale)?));
                self.constant = self
                    .constant
                    .checked_add(coefficient.checked_mul(*offset)?)?;
            }
            Element::Const(constant) => {
                let value = constant_value(constant)?;
                self.constant = self.constant.checked_add(coefficient.checked_mul(value)?)?;
            }
        }
        Some(())
    }

    fn of(pairs: impl IntoIterator<Item = (i64, Element)>) -> Option<Expression> {
        let mut expression = Expression::default();
        for (coefficient, element) in pairs {
            expression.add(coefficient, &element)?;
        }
        Some(expression)
    }

    fn negated(&self) -> Option<Expression> {
        Some(Expression {
            terms: self
                .terms
                .iter()
                .map(|&(var, coefficient)| Some((var, coefficient.checked_neg()?)))
                .collect::<Option<_>>()?,
            constant: self.constant.checked_neg()?,
        })
    }

    fn with(&self, coefficient: i64, element: &Element) -> Option<Expression> {
        let mut expression = self.clone();
        expression.add(coefficient, element)?;
        Some(expression)
    }

    fn bounds(&self, bounds: &impl Fn(VarIndex) -> (i64, i64)) -> Option<(i64, i64)> {
        let minimum = maximum(&self.negated()?.terms, bounds)?.checked_neg()?;
        let maximum = maximum(&self.terms, bounds)?;
        Some((
            minimum.checked_add(self.constant)?,
            maximum.checked_add(self.constant)?,
        ))
    }
}

fn maximum(terms: &[(VarIndex, i64)], bounds: &impl Fn(VarIndex) -> (i64, i64)) -> Option<i64> {
    terms.iter().try_fold(0_i64, |sum, &(var, coefficient)| {
        let (lower, upper) = bounds(var);
        let term = coefficient
            .checked_mul(lower)?
            .max(coefficient.checked_mul(upper)?);
        sum.checked_add(term)
    })
}

fn constant_value(constant: &Constant) -> Option<i64> {
    match constant {
        Constant::Int(value) => Some(*value),
        Constant::Bool(value) => Some(i64::from(*value)),
        _ => None,
    }
}

fn scalar(resolved: &Resolved) -> Option<Element> {
    match resolved {
        Resolved::Scalar(element) => Some(element.clone()),
        Resolved::Array(_) => None,
    }
}

fn array(resolved: &Resolved) -> Option<&[Element]> {
    match resolved {
        Resolved::Array(elements) => Some(elements),
        Resolved::Scalar(_) => None,
    }
}

fn integers(resolved: &Resolved) -> Option<Vec<i64>> {
    array(resolved)?
        .iter()
        .map(|element| match element {
            Element::Const(constant) => constant_value(constant),
            Element::Var(_) | Element::View { .. } => None,
        })
        .collect()
}

fn integer(resolved: &Resolved) -> Option<i64> {
    match scalar(resolved)? {
        Element::Const(constant) => constant_value(&constant),
        Element::Var(_) | Element::View { .. } => None,
    }
}

/// `sum(coefficients[i] * elements[i])` for the coefficient and variable arrays of a linear
/// constraint.
fn weighted_sum(coefficients: &Resolved, elements: &Resolved) -> Option<Expression> {
    let coefficients = integers(coefficients)?;
    let elements = array(elements)?;
    if coefficients.len() != elements.len() {
        return None;
    }
    Expression::of(coefficients.into_iter().zip(elements.iter().cloned()))
}

/// `sum(elements)`.
fn sum(elements: &[Element]) -> Option<Expression> {
    Expression::of(elements.iter().map(|element| (1, element.clone())))
}

fn less_or_equal(expression: &Expression, rhs: i64) -> Option<Row> {
    Row::new(Relation::LessOrEqual, expression, rhs)
}

fn equal(expression: &Expression, rhs: i64) -> Option<Row> {
    Row::new(Relation::Equal, expression, rhs)
}

/// The rows of `r -> expression <= rhs`, and additionally of `~r -> expression >= rhs + 1` when
/// `fully_reified`.
fn reified_less_or_equal(
    expression: &Expression,
    rhs: i64,
    reification: &Element,
    fully_reified: bool,
    bounds: &impl Fn(VarIndex) -> (i64, i64),
) -> Option<Vec<Row>> {
    if let Element::Const(constant) = reification {
        return match (constant_value(constant)?, fully_reified) {
            (1, _) => Some(vec![less_or_equal(expression, rhs)?]),
            (_, true) => Some(vec![less_or_equal(
                &expression.negated()?,
                rhs.checked_add(1)?.checked_neg()?,
            )?]),
            (_, false) => Some(Vec::new()),
        };
    }

    let (lower, upper) = expression.bounds(bounds)?;
    // r = 1 gives `expression <= rhs`; r = 0 gives `expression <= upper`, which always holds.
    let big_m = upper.checked_sub(rhs)?.max(0);
    let mut rows = vec![less_or_equal(
        &expression.with(big_m, reification)?,
        rhs.checked_add(big_m)?,
    )?];
    if fully_reified {
        // r = 0 gives `-expression <= -rhs - 1`; r = 1 gives `-expression <= -lower`.
        let big_m = rhs.checked_add(1)?.checked_sub(lower)?.max(0);
        rows.push(less_or_equal(
            &expression
                .negated()?
                .with(big_m.checked_neg()?, reification)?,
            rhs.checked_add(1)?.checked_neg()?,
        )?);
    }
    Some(rows)
}

/// The rows of a Boolean gate over `inputs` and `output`, each given as `(input coefficients,
/// output coefficient, rhs)`.
fn gate(rows: &[(&[i64], i64, i64)], inputs: &[Element], output: &Element) -> Option<Vec<Row>> {
    rows.iter()
        .map(|(coefficients, output_coefficient, rhs)| {
            let expression = Expression::of(
                coefficients
                    .iter()
                    .copied()
                    .zip(inputs.iter().cloned())
                    .chain(std::iter::once((*output_coefficient, output.clone()))),
            )?;
            less_or_equal(&expression, *rhs)
        })
        .collect()
}

/// The exact linear form of a constraint, or `None` if it has none.
pub(super) fn rows_of<Arg: AsRef<Resolved>>(
    id: &str,
    args: &[Arg],
    bounds: &impl Fn(VarIndex) -> (i64, i64),
) -> Option<Vec<Row>> {
    let scalar_at = |position: usize| args.get(position).and_then(|arg| scalar(arg.as_ref()));

    match id {
        "int_lin_le" | "bool_lin_le" if args.len() == 3 => Some(vec![less_or_equal(
            &weighted_sum(args[0].as_ref(), args[1].as_ref())?,
            integer(args[2].as_ref())?,
        )?]),
        "int_lin_eq" | "bool_lin_eq" | "int_lin_ne" if args.len() == 3 => Some(vec![equal(
            &weighted_sum(args[0].as_ref(), args[1].as_ref())?,
            integer(args[2].as_ref())?,
        )?]),

        // Merged by the compiler when both arguments are variables, so the two arguments then
        // resolve to the same variable and the constraint holds trivially.
        "bool2int" | "int_eq" | "bool_eq" | "int_ne" if args.len() == 2 => {
            let row = equal(
                &Expression::of([(1, scalar_at(0)?), (-1, scalar_at(1)?)])?,
                0,
            )?;
            if row.terms.is_empty() && row.rhs == 0 && id != "int_ne" {
                return Some(Vec::new());
            }
            Some(vec![row])
        }
        "int_le" | "bool_le" | "int_lt" | "bool_lt" if args.len() == 2 => {
            let expression = Expression::of([(1, scalar_at(0)?), (-1, scalar_at(1)?)])?;
            let rhs = if id.ends_with("_lt") { -1 } else { 0 };
            Some(vec![less_or_equal(&expression, rhs)?])
        }
        "bool_not" | "bool_xor" if args.len() == 2 => Some(vec![equal(
            &Expression::of([(1, scalar_at(0)?), (1, scalar_at(1)?)])?,
            1,
        )?]),

        "bool_clause" if args.len() == 2 => {
            let positive = array(args[0].as_ref())?;
            let negative = array(args[1].as_ref())?;
            // sum(negative) - sum(positive) <= |negative| - 1
            let expression = Expression::of(
                positive
                    .iter()
                    .map(|element| (-1, element.clone()))
                    .chain(negative.iter().map(|element| (1, element.clone()))),
            )?;
            let rhs = i64::try_from(negative.len()).ok()? - 1;
            Some(vec![less_or_equal(&expression, rhs)?])
        }
        "array_bool_or" | "array_bool_and" if args.len() == 2 => {
            let inputs = array(args[0].as_ref())?;
            let output = scalar_at(1)?;
            let mut rows = Vec::with_capacity(inputs.len() + 1);
            let n = i64::try_from(inputs.len()).ok()?;
            if id == "array_bool_or" {
                // each input implies the output; the output implies some input
                for input in inputs {
                    rows.push(less_or_equal(
                        &Expression::of([(1, input.clone()), (-1, output.clone())])?,
                        0,
                    )?);
                }
                rows.push(less_or_equal(
                    &sum(inputs)?.negated()?.with(1, &output)?,
                    0,
                )?);
            } else {
                // the output implies each input; all inputs imply the output
                for input in inputs {
                    rows.push(less_or_equal(
                        &Expression::of([(1, output.clone()), (-1, input.clone())])?,
                        0,
                    )?);
                }
                rows.push(less_or_equal(&sum(inputs)?.with(-1, &output)?, n - 1)?);
            }
            Some(rows)
        }
        "bool_and" | "bool_or" | "bool_xor" | "bool_eq_reif" | "bool_ne_reif"
            if args.len() == 3 =>
        {
            let inputs = [scalar_at(0)?, scalar_at(1)?];
            let output = scalar_at(2)?;
            let rows: &[(&[i64], i64, i64)] = match id {
                "bool_and" => &[(&[-1, 0], 1, 0), (&[0, -1], 1, 0), (&[1, 1], -1, 1)],
                "bool_or" => &[(&[1, 0], -1, 0), (&[0, 1], -1, 0), (&[-1, -1], 1, 0)],
                "bool_eq_reif" => &[
                    (&[-1, -1], -1, -1),
                    (&[1, 1], -1, 1),
                    (&[1, -1], 1, 1),
                    (&[-1, 1], 1, 1),
                ],
                // bool_xor and bool_ne_reif
                _ => &[
                    (&[-1, -1], 1, 0),
                    (&[1, -1], -1, 0),
                    (&[-1, 1], -1, 0),
                    (&[1, 1], 1, 2),
                ],
            };
            gate(rows, &inputs, &output)
        }

        "int_le_reif" | "int_lt_reif" | "bool_le_reif" | "bool_lt_reif" | "int_le_imp"
        | "int_lt_imp"
            if args.len() == 3 =>
        {
            let expression = Expression::of([(1, scalar_at(0)?), (-1, scalar_at(1)?)])?;
            let rhs = if id.contains("_lt_") { -1 } else { 0 };
            reified_less_or_equal(
                &expression,
                rhs,
                &scalar_at(2)?,
                id.ends_with("_reif"),
                bounds,
            )
        }
        "int_lin_le_reif" | "int_lin_le_imp" | "bool_lin_le_imp" if args.len() == 4 => {
            reified_less_or_equal(
                &weighted_sum(args[0].as_ref(), args[1].as_ref())?,
                integer(args[2].as_ref())?,
                &scalar_at(3)?,
                id.ends_with("_reif"),
                bounds,
            )
        }
        "int_lin_eq_imp" if args.len() == 4 => {
            let expression = weighted_sum(args[0].as_ref(), args[1].as_ref())?;
            let rhs = integer(args[2].as_ref())?;
            let reification = scalar_at(3)?;
            let mut rows = reified_less_or_equal(&expression, rhs, &reification, false, bounds)?;
            rows.extend(reified_less_or_equal(
                &expression.negated()?,
                rhs.checked_neg()?,
                &reification,
                false,
                bounds,
            )?);
            Some(rows)
        }
        _ => None,
    }
}
