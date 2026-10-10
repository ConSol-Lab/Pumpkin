//! The rewrites the FlatZinc compiler performs when it makes variables views of one another,
//! recorded by variable name so that a croc proof can state them before the first learned
//! constraint.

use std::fmt::Write;
use std::rc::Rc;

use crate::containers::HashMap;

/// A term of a linear constraint as the checker states it: a coefficient over a variable or an
/// integer literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    Variable(Rc<str>),
    Constant(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Relation {
    LessOrEqual,
    Equal,
    NotEqual,
}

/// A linear constraint in the form the checker's lowering gives it, term order included: the
/// checker compares a stated result with the one it computes up to the order of terms, but it
/// does not merge terms over one variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearForm {
    pub terms: Vec<(i64, Term)>,
    pub relation: Relation,
    pub bound: i64,
}

/// `variable := scale * replacing + offset`, licensed by the constraint `evidence`, which reads
/// `c_v * variable + c_r * replacing = k` with `c_v * scale + c_r = 0` and `c_v * offset = k`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Replacement {
    pub variable: Rc<str>,
    pub replacing: Rc<str>,
    pub scale: i64,
    pub offset: i64,
    pub evidence: u64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ViewEvent {
    /// `n <id> <variable>`: the definition `variable = other` of the model numbered as a
    /// constraint, so that it can be cited.
    Definition { id: u64, variable: Rc<str> },
    /// `s c:<target>`: the constraint rewritten by the replacements, kept under its number, with
    /// the result stated when the checker requires it (a linear constraint).
    Substitute {
        target: u64,
        replacements: Vec<Replacement>,
        stated: Option<LinearForm>,
    },
    /// `v <parameter> from c:<source>`: a fresh parameter for a two-term equality
    /// whose coefficients divide neither way, with the two equalities
    /// `variable = scale * parameter + offset` under new numbers.
    Parametrise {
        parameter: Rc<str>,
        source: u64,
        equalities: [(u64, Rc<str>, i64, i64); 2],
    },
    /// `s <variable> := <affine> by c:<evidence>`: the variable replaced in every constraint but
    /// the cited equality, from here on.
    Declare(Replacement),
    /// `q <id> : <equality> by c:<k> ...`: an equality over two variables that the cited
    /// constraints entail, which the checker establishes by enumerating the two variables.
    Equality {
        id: u64,
        form: LinearForm,
        evidence: Vec<u64>,
    },
}

/// The view steps of the FlatZinc compiler, in order.
#[derive(Debug, Default)]
pub struct ViewLog {
    pub events: Vec<ViewEvent>,
}

impl LinearForm {
    /// What the checker holds after an `s` step, in its normal form: each term over a replaced
    /// variable becomes the scaled term over the replacing variable, the offsets and the terms
    /// over a constant move into the bound, like terms are merged into the first, and the terms
    /// with coefficient zero are left out.
    pub fn substitute(&self, replacements: &[Replacement]) -> LinearForm {
        let mut bound = self.bound;
        let by_variable: HashMap<&str, &Replacement> = replacements
            .iter()
            .map(|replacement| (replacement.variable.as_ref(), replacement))
            .collect();
        let mut terms = vec![];
        for (coefficient, term) in &self.terms {
            match term {
                Term::Variable(name) => match by_variable.get(name.as_ref()).copied() {
                    Some(replacement) => {
                        bound -= coefficient * replacement.offset;
                        terms.push((
                            coefficient * replacement.scale,
                            Term::Variable(Rc::clone(&replacement.replacing)),
                        ));
                    }
                    None => terms.push((*coefficient, term.clone())),
                },
                Term::Constant(value) => bound -= coefficient * value,
            }
        }
        let terms = merge_like_terms(terms)
            .into_iter()
            .filter(|(coefficient, _)| *coefficient != 0)
            .collect();
        LinearForm {
            terms,
            relation: self.relation,
            bound,
        }
    }

    fn render(&self) -> String {
        let mut text = String::new();
        for (coefficient, term) in &self.terms {
            match term {
                Term::Variable(name) => write!(text, "{coefficient} {name} ").unwrap(),
                Term::Constant(value) => write!(text, "{coefficient} {value} ").unwrap(),
            }
        }
        let relation = match self.relation {
            Relation::LessOrEqual => "<=",
            Relation::Equal => "=",
            Relation::NotEqual => "!=",
        };
        write!(text, "{relation} {}", self.bound).unwrap();
        text
    }
}

/// Like terms merged: a later term over the same expression is merged into the earlier one, so
/// the terms keep the order of their first occurrence and a coefficient may become zero.
fn merge_like_terms(terms: Vec<(i64, Term)>) -> Vec<(i64, Term)> {
    let mut merged: Vec<(i64, Term)> = vec![];
    let mut position: HashMap<Term, usize> = HashMap::default();
    for (coefficient, term) in terms {
        match position.get(&term) {
            Some(&index) => merged[index].0 += coefficient,
            None => {
                let _ = position.insert(term.clone(), merged.len());
                merged.push((coefficient, term));
            }
        }
    }
    merged
}

/// `scale * variable + offset` as the checker's reader accepts it: `t`, `-t`, `2*t+2`, `3*t-1`.
pub fn affine_text(scale: i64, variable: &str, offset: i64) -> String {
    if scale == 0 {
        return offset.to_string();
    }
    let mut text = match scale {
        1 => variable.to_owned(),
        -1 => format!("-{variable}"),
        scale => format!("{scale}*{variable}"),
    };
    if offset > 0 {
        write!(text, "+{offset}").unwrap();
    } else if offset < 0 {
        write!(text, "{offset}").unwrap();
    }
    text
}

impl ViewLog {
    pub fn render(&self) -> Vec<String> {
        // The checker numbers definitions before the first step, block or declared replacement.
        let mut lines: Vec<String> = self
            .events
            .iter()
            .filter_map(|event| match event {
                ViewEvent::Definition { id, variable } => Some(format!("n {id} {variable}")),
                _ => None,
            })
            .collect();
        for event in &self.events {
            match event {
                ViewEvent::Definition { .. } => {}
                ViewEvent::Substitute {
                    target,
                    replacements,
                    stated,
                } => {
                    lines.push(format!("s c:{target}"));
                    for replacement in replacements {
                        lines.push(format!(
                            "  {} := {} by c:{}",
                            replacement.variable,
                            affine_text(
                                replacement.scale,
                                &replacement.replacing,
                                replacement.offset
                            ),
                            replacement.evidence
                        ));
                    }
                    match stated {
                        Some(form) => lines.push(format!("  e {target} : {}", form.render())),
                        None => lines.push(format!("  e {target}")),
                    }
                }
                ViewEvent::Declare(replacement) => lines.push(format!(
                    "s {} := {} by c:{}",
                    replacement.variable,
                    affine_text(
                        replacement.scale,
                        &replacement.replacing,
                        replacement.offset
                    ),
                    replacement.evidence
                )),
                ViewEvent::Equality { id, form, evidence } => {
                    let cited = evidence
                        .iter()
                        .map(|number| format!("c:{number}"))
                        .collect::<Vec<_>>()
                        .join(" ");
                    lines.push(format!("q {id} : {} by {cited}", form.render()));
                }
                ViewEvent::Parametrise {
                    parameter,
                    source,
                    equalities,
                } => {
                    lines.push(format!("v {parameter} from c:{source}"));
                    for (id, variable, scale, offset) in equalities {
                        lines.push(format!(
                            "  {id} : {variable} = {}",
                            affine_text(*scale, parameter, *offset)
                        ));
                    }
                }
            }
        }
        lines
    }
}
