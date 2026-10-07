//! Random examples, generated from the signatures of the constraints the FlatZinc compiler
//! supports, and mutants of existing examples.

use std::fmt::Write;

use pumpkin_core::rand::RngExt;
use pumpkin_core::rand::rngs::SmallRng;

use crate::example::Example;
use crate::statements::Statement;
use crate::statements::classify;
use crate::statements::integer_literals;

/// The arguments of the constraints, in a small notation:
///
/// - `ivar`, `bvar`: an integer or a Boolean variable; `nzivar`: an integer variable without zero.
/// - `int`: an integer constant; `nat`: a non-negative one.
/// - `ivars(n)`, `bvars(n)`, `ints(n)`, `nats(n)`, `bools(n)`: arrays whose length is `n`, which is
///   the same for every argument of one constraint; `m` is a second, independent length.
/// - `table(n)`: the tuples of a table over `n` variables, flattened.
/// - `set`: a set of integers.
const SIGNATURES: &[(&str, &str)] = &[
    ("int_lin_eq", "ints(n) ivars(n) int"),
    ("int_lin_le", "ints(n) ivars(n) int"),
    ("int_lin_ne", "ints(n) ivars(n) int"),
    ("int_lin_eq_reif", "ints(n) ivars(n) int bvar"),
    ("int_lin_le_reif", "ints(n) ivars(n) int bvar"),
    ("int_lin_ne_reif", "ints(n) ivars(n) int bvar"),
    ("int_lin_eq_imp", "ints(n) ivars(n) int bvar"),
    ("int_lin_ge_imp", "ints(n) ivars(n) int bvar"),
    ("int_lin_gt_imp", "ints(n) ivars(n) int bvar"),
    ("int_lin_le_imp", "ints(n) ivars(n) int bvar"),
    ("int_lin_lt_imp", "ints(n) ivars(n) int bvar"),
    ("int_lin_ne_imp", "ints(n) ivars(n) int bvar"),
    ("int_eq", "ivar ivar"),
    ("int_ne", "ivar ivar"),
    ("int_le", "ivar ivar"),
    ("int_lt", "ivar ivar"),
    ("int_eq_reif", "ivar ivar bvar"),
    ("int_ne_reif", "ivar ivar bvar"),
    ("int_le_reif", "ivar ivar bvar"),
    ("int_lt_reif", "ivar ivar bvar"),
    ("int_eq_imp", "ivar ivar bvar"),
    ("int_ne_imp", "ivar ivar bvar"),
    ("int_ge_imp", "ivar ivar bvar"),
    ("int_gt_imp", "ivar ivar bvar"),
    ("int_le_imp", "ivar ivar bvar"),
    ("int_lt_imp", "ivar ivar bvar"),
    ("int_plus", "ivar ivar ivar"),
    ("int_times", "ivar ivar ivar"),
    ("int_div", "ivar nzivar ivar"),
    ("int_abs", "ivar ivar"),
    ("int_max", "ivar ivar ivar"),
    ("int_min", "ivar ivar ivar"),
    ("array_int_maximum", "ivar ivars(n)"),
    ("array_int_minimum", "ivar ivars(n)"),
    ("array_int_element", "ivar ints(n) ivar"),
    ("array_var_int_element", "ivar ivars(n) ivar"),
    ("pumpkin_all_different", "ivars(n)"),
    ("pumpkin_table_int", "ivars(n) table(n)"),
    ("pumpkin_table_int_reif", "ivars(n) table(n) bvar"),
    ("pumpkin_cumulative", "ivars(n) nats(n) nats(n) nat"),
    ("pumpkin_disjunctive_strict", "ivars(n) nats(n)"),
    ("array_bool_and", "bvars(n) bvar"),
    ("array_bool_or", "bvars(n) bvar"),
    ("array_bool_element", "ivar bools(n) bvar"),
    ("array_var_bool_element", "ivar bvars(n) bvar"),
    ("pumpkin_bool_xor", "bvar bvar"),
    ("pumpkin_bool_xor_reif", "bvar bvar bvar"),
    ("bool2int", "bvar ivar"),
    ("bool_lin_eq", "ints(n) bvars(n) ivar"),
    ("bool_lin_le", "ints(n) bvars(n) int"),
    ("bool_and", "bvar bvar bvar"),
    ("bool_clause", "bvars(n) bvars(m)"),
    ("bool_eq", "bvar bvar"),
    ("bool_eq_reif", "bvar bvar bvar"),
    ("bool_not", "bvar bvar"),
    ("set_in_reif", "ivar set bvar"),
];

/// The names of the constraints that random examples are generated for.
pub fn constraint_names() -> impl Iterator<Item = &'static str> {
    SIGNATURES.iter().map(|&(name, _)| name)
}

/// A random example of one of the constraints in `names`, or of any constraint when `names` is
/// empty.
pub fn random_example(rng: &mut SmallRng, names: &[String], origin: String) -> Option<Example> {
    let candidates = SIGNATURES
        .iter()
        .filter(|(name, _)| names.is_empty() || names.iter().any(|wanted| wanted == name))
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        return None;
    }
    let &(name, signature) = candidates[rng.random_range(0..candidates.len())];

    let mut generator = Generator {
        rng,
        declarations: String::new(),
        variables: 0,
        length: 0,
        second_length: 0,
    };
    generator.length = generator.rng.random_range(1..=4);
    generator.second_length = generator.rng.random_range(0..=3);

    let arguments = signature
        .split(' ')
        .map(|argument| generator.argument(argument))
        .collect::<Vec<_>>()
        .join(", ");

    let mut source = generator.declarations;
    writeln!(source, "constraint {name}({arguments});").expect("writing to a string succeeds");
    source.push_str("solve satisfy;\n");

    Some(Example {
        origin,
        constraint_name: name.to_owned(),
        source,
        occurrences: 1,
    })
}

struct Generator<'a> {
    rng: &'a mut SmallRng,
    declarations: String,
    variables: usize,
    length: usize,
    second_length: usize,
}

impl Generator<'_> {
    fn argument(&mut self, argument: &str) -> String {
        let (kind, length) = match argument.split_once('(') {
            Some((kind, "n)")) => (kind, Some(self.length)),
            Some((kind, _)) => (kind, Some(self.second_length)),
            None => (argument, None),
        };

        match (kind, length) {
            ("ivar", None) => self.integer_variable(false),
            ("nzivar", None) => self.integer_variable(true),
            ("bvar", None) => self.boolean_variable(),
            ("int", None) => self.rng.random_range(-6..=6).to_string(),
            ("nat", None) => self.rng.random_range(0..=6).to_string(),
            ("set", None) => self.set(),
            ("ivars", Some(length)) => {
                self.array(length, |generator| generator.integer_variable(false))
            }
            ("bvars", Some(length)) => self.array(length, Generator::boolean_variable),
            ("ints", Some(length)) => self.array(length, Generator::coefficient),
            ("nats", Some(length)) => self.array(length, |generator| {
                generator.rng.random_range(0..=4).to_string()
            }),
            ("bools", Some(length)) => self.array(length, |generator| {
                generator.rng.random_bool(0.5).to_string()
            }),
            ("table", Some(length)) => {
                let tuples = self.rng.random_range(0..=5);
                self.array(tuples * length, |generator| {
                    generator.rng.random_range(-3..=3).to_string()
                })
            }
            _ => unreachable!("unknown argument kind {argument}"),
        }
    }

    fn array(&mut self, length: usize, mut element: impl FnMut(&mut Self) -> String) -> String {
        let elements = (0..length).map(|_| element(self)).collect::<Vec<_>>();
        format!("[{}]", elements.join(", "))
    }

    /// A non-zero coefficient most of the time; a zero one is legal FlatZinc as well.
    fn coefficient(&mut self) -> String {
        if self.rng.random_bool(0.05) {
            return "0".to_owned();
        }
        let magnitude = self.rng.random_range(1..=4);
        if self.rng.random_bool(0.5) {
            magnitude.to_string()
        } else {
            (-magnitude).to_string()
        }
    }

    fn integer_variable(&mut self, without_zero: bool) -> String {
        let name = format!("x{}", self.variables);
        self.variables += 1;

        let lower_bound = self.rng.random_range(-5..=5);
        let upper_bound = lower_bound + self.rng.random_range(0..=7);
        let values = (lower_bound..=upper_bound)
            .filter(|&value| !without_zero || value != 0)
            .collect::<Vec<_>>();

        let is_sparse = without_zero && values.len() != (upper_bound - lower_bound + 1) as usize
            || self.rng.random_bool(0.25);
        let domain = if values.is_empty() {
            "1..1".to_owned()
        } else if is_sparse {
            let kept = values
                .iter()
                .filter(|_| self.rng.random_bool(0.7))
                .map(i32::to_string)
                .collect::<Vec<_>>();
            if kept.is_empty() {
                format!("{0}..{0}", values[0])
            } else {
                format!("{{{}}}", kept.join(", "))
            }
        } else {
            format!("{lower_bound}..{upper_bound}")
        };

        writeln!(self.declarations, "var {domain}: {name};").expect("writing to a string succeeds");
        name
    }

    fn boolean_variable(&mut self) -> String {
        let name = format!("b{}", self.variables);
        self.variables += 1;
        writeln!(self.declarations, "var bool: {name};").expect("writing to a string succeeds");
        name
    }

    fn set(&mut self) -> String {
        let lower_bound = self.rng.random_range(-4..=4);
        if self.rng.random_bool(0.5) {
            format!(
                "{lower_bound}..{}",
                lower_bound + self.rng.random_range(0..=4)
            )
        } else {
            let values = (lower_bound..lower_bound + 6)
                .filter(|_| self.rng.random_bool(0.5))
                .map(|value| value.to_string())
                .collect::<Vec<_>>();
            format!("{{{}}}", values.join(", "))
        }
    }
}

/// A mutant of `example` with one small change: an integer domain narrowed, widened, given a hole
/// or fixed, or a constant of the constraint or of a parameter changed. A constant keeps its sign,
/// so a non-negative duration stays non-negative.
pub fn mutate(rng: &mut SmallRng, example: &Example, origin: String) -> Option<Example> {
    let lines = example.source.lines().collect::<Vec<_>>();

    // Every integer literal that may be changed, with the line it is on and whether it is the
    // bound of a variable domain.
    let mut sites = vec![];
    for (line_index, line) in lines.iter().enumerate() {
        let is_variable = line.trim_start().starts_with("var ");
        let (start, end) = match classify(line) {
            Statement::Declaration { .. } if is_variable => match domain_range(line) {
                Some(range) => range,
                None => continue,
            },
            // A parameter: only its value, not the index set of an array.
            Statement::Declaration { .. } if !line.contains("var ") => match line.find('=') {
                Some(equals) => (equals, line.len()),
                None => continue,
            },
            Statement::Constraint { .. } => (0, line.len()),
            _ => continue,
        };
        for literal in integer_literals(&line[start..end]) {
            let literal = (literal.0 + start, literal.1 + start, literal.2);
            sites.push((line_index, literal, is_variable));
        }
    }
    if sites.is_empty() {
        return None;
    }

    let (line_index, (start, end, value), is_variable) = sites[rng.random_range(0..sites.len())];
    let line = lines[line_index];

    let replacement = if is_variable && rng.random_bool(0.4) {
        // Replace the whole domain of the variable by a sparse or a fixed one around the value.
        let (domain_start, domain_end) = domain_range(line)?;
        let domain = &line[domain_start..domain_end];
        let (lower_bound, upper_bound) = domain.split_once("..")?;
        let lower_bound = lower_bound.trim().parse::<i64>().ok()?;
        let upper_bound = upper_bound.trim().parse::<i64>().ok()?;
        let new_domain = if rng.random_bool(0.5) || lower_bound == upper_bound {
            let fixed = rng.random_range(lower_bound..=upper_bound);
            format!("{fixed}..{fixed}")
        } else {
            let hole = rng.random_range(lower_bound..=upper_bound);
            let values = (lower_bound..=upper_bound)
                .filter(|&value| value != hole)
                .map(|value| value.to_string())
                .collect::<Vec<_>>();
            format!("{{{}}}", values.join(", "))
        };
        format!(
            "{}{new_domain}{}",
            &line[..domain_start],
            &line[domain_end..]
        )
    } else {
        let change = rng.random_range(1..=3) * if rng.random_bool(0.5) { 1 } else { -1 };
        let mut new_value = value + change;
        if value >= 0 && new_value < 0 || value < 0 && new_value >= 0 {
            new_value = value - change;
        }
        format!("{}{new_value}{}", &line[..start], &line[end..])
    };

    if is_variable && !domain_is_non_empty(&replacement) {
        return None;
    }

    let mut source = String::new();
    for (index, line) in lines.iter().enumerate() {
        let line = if index == line_index {
            replacement.as_str()
        } else {
            line
        };
        writeln!(source, "{line}").expect("writing to a string succeeds");
    }

    Some(Example {
        origin,
        constraint_name: example.constraint_name.clone(),
        source,
        occurrences: 1,
    })
}

/// The range of the domain `lb..ub` in a variable declaration `var lb..ub: name`.
fn domain_range(line: &str) -> Option<(usize, usize)> {
    let start = line.find("var ")? + 4;
    let end = start + line[start..].find(':')?;
    Some((start, end))
}

fn domain_is_non_empty(line: &str) -> bool {
    let Some((start, end)) = domain_range(line) else {
        return true;
    };
    match line[start..end].split_once("..") {
        Some((lower_bound, upper_bound)) => match (
            lower_bound.trim().parse::<i64>(),
            upper_bound.trim().parse::<i64>(),
        ) {
            (Ok(lower_bound), Ok(upper_bound)) => lower_bound <= upper_bound,
            _ => true,
        },
        None => true,
    }
}
