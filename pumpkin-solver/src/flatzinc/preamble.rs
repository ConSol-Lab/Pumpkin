//! The preamble of a croc scaffold: the preprocessing the compiler did, stated for the proof
//! processor, which applies it in place of deciding it.

use std::fmt::Write as _;
use std::io::BufRead;

pub use pumpkin_core::proof::SCAFFOLD_NOGOODS_MARKER;

/// The preprocessing a solver states in the preamble of its scaffold.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Preamble {
    pub views: Vec<View>,
    pub parameters: Vec<String>,
    pub constants: Vec<(String, i64)>,
    pub literals: Vec<LiteralDefinition>,
    pub skipped: Vec<u64>,
    pub decompositions: Vec<Decomposition>,
    /// The objective variable when it is a view, with the constraint that relates it to its
    /// representative.
    pub objective: Option<(String, u64)>,
    pub first_free: Option<u64>,
    pub breaking: Vec<BreakingItem>,
    pub steps: Vec<String>,
}

/// An item of the symmetry and dominance breaking steps.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BreakingItem {
    /// A key of the order the steps use.
    Key(String),
    /// The identifier a step takes, with the constraint posted under it.
    Identifier {
        identifier: u64,
        posted: Option<Posted>,
    },
    /// A line of a step.
    Step(String),
}

/// A constraint posted under the identifier of a breaking step.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Posted {
    Clause(Vec<Atom>),
    /// The first variables are lexicographically at most the second.
    LexLessOrEqual(Vec<String>, Vec<String>),
}

/// `[variable comparator value]`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Atom {
    pub variable: String,
    pub comparator: Comparator,
    pub value: i64,
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{} {} {}]",
            self.variable,
            self.comparator.as_str(),
            self.value
        )
    }
}

impl Atom {
    /// The atoms `[x <= 3] [y >= 1] ...` the text lists, separated by whitespace.
    pub fn parse_list(text: &str) -> Option<Vec<Atom>> {
        let mut atoms = vec![];
        let mut rest = text.trim();
        while !rest.is_empty() {
            let inner = rest.strip_prefix('[')?;
            let end = inner.find(']')?;
            let fields: Vec<&str> = inner[..end].split_whitespace().collect();
            let [variable, comparator, value] = fields.as_slice() else {
                return None;
            };
            atoms.push(Atom {
                variable: (*variable).to_owned(),
                comparator: Comparator::parse(comparator)?,
                value: value.parse().ok()?,
            });
            rest = inner[end + 1..].trim_start();
        }
        Some(atoms)
    }
}

/// `variable = scale * representative + offset`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct View {
    pub variable: String,
    pub representative: String,
    pub scale: i64,
    pub offset: i64,
}

/// The boolean is the literal of `[variable comparator value]`, defined by the constraint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LiteralDefinition {
    pub boolean: String,
    pub variable: String,
    pub comparator: Comparator,
    pub value: i64,
    pub constraint: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparator {
    LessOrEqual,
    GreaterOrEqual,
    Equal,
    NotEqual,
}

impl Comparator {
    pub fn as_str(self) -> &'static str {
        match self {
            Comparator::LessOrEqual => "<=",
            Comparator::GreaterOrEqual => ">=",
            Comparator::Equal => "==",
            Comparator::NotEqual => "!=",
        }
    }

    fn parse(text: &str) -> Option<Comparator> {
        match text {
            "<=" => Some(Comparator::LessOrEqual),
            ">=" => Some(Comparator::GreaterOrEqual),
            "==" => Some(Comparator::Equal),
            "!=" => Some(Comparator::NotEqual),
            _ => None,
        }
    }
}

/// A model constraint posted as its decomposition, with the fresh variables it introduces.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Decomposition {
    pub kind: DecompositionKind,
    pub constraint: u64,
    pub fresh_variables: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DecompositionKind {
    /// A table, with one selector per row.
    Table,
}

/// A line of a preamble that cannot be read.
#[derive(Debug, thiserror::Error)]
pub enum PreambleError {
    #[error("failed to read the preamble: {0}")]
    Io(#[from] std::io::Error),

    #[error("preamble line {line}: {message}")]
    Syntax { line: usize, message: String },

    #[error("the scaffold ends before the line '{SCAFFOLD_NOGOODS_MARKER}' that ends its preamble")]
    NoMarker,
}

impl Preamble {
    /// Whether the preamble states any preprocessing.
    pub fn is_empty(&self) -> bool {
        *self == Preamble::default()
    }

    /// Read the preamble at the head of a croc scaffold, up to and including the marker line,
    /// leaving the reader at the first line of the nogoods. It reads line by line, so the
    /// scaffold may be a pipe or a file still being written.
    pub fn read_head_of_scaffold(reader: &mut impl BufRead) -> Result<Preamble, PreambleError> {
        Preamble::read_lines(reader, true)
    }

    /// Read a preamble that makes up the whole of the reader, without the marker line.
    pub fn read(mut reader: impl BufRead) -> Result<Preamble, PreambleError> {
        Preamble::read_lines(&mut reader, false)
    }

    fn read_lines(
        reader: &mut impl BufRead,
        until_marker: bool,
    ) -> Result<Preamble, PreambleError> {
        let mut preamble = Preamble::default();
        let mut in_steps = false;
        let mut index = 0;
        let mut buffer = String::new();
        loop {
            buffer.clear();
            if reader.read_line(&mut buffer)? == 0 {
                if until_marker {
                    return Err(PreambleError::NoMarker);
                }
                return Ok(preamble);
            }
            index += 1;
            let line = buffer.strip_suffix('\n').unwrap_or(&buffer);
            let line = line.strip_suffix('\r').unwrap_or(line).to_owned();
            if until_marker && line == SCAFFOLD_NOGOODS_MARKER {
                return Ok(preamble);
            }
            if in_steps {
                preamble.steps.push(line);
                continue;
            }
            if let Some(rest) = line.strip_prefix("key ") {
                preamble.breaking.push(BreakingItem::Key(rest.to_owned()));
                continue;
            }
            if let Some(rest) = line.strip_prefix("step ") {
                preamble.breaking.push(BreakingItem::Step(rest.to_owned()));
                continue;
            }
            let fields: Vec<&str> = line.split_whitespace().collect();
            let error = |message: &str| PreambleError::Syntax {
                line: index,
                message: format!("{message}: '{line}'"),
            };
            let number = |text: &str| -> Result<u64, PreambleError> {
                text.parse()
                    .map_err(|_| error("expected a constraint number"))
            };
            let integer = |text: &str| -> Result<i64, PreambleError> {
                text.parse().map_err(|_| error("expected an integer"))
            };
            match fields.as_slice() {
                [] => {}
                [first, ..] if first.starts_with('%') => {}
                ["view", variable, representative, scale, offset] => preamble.views.push(View {
                    variable: (*variable).to_owned(),
                    representative: (*representative).to_owned(),
                    scale: integer(scale)?,
                    offset: integer(offset)?,
                }),
                ["parameter", name] => preamble.parameters.push((*name).to_owned()),
                ["constant", variable, value] => preamble
                    .constants
                    .push(((*variable).to_owned(), integer(value)?)),
                ["literal", boolean, variable, comparator, value, constraint] => {
                    preamble.literals.push(LiteralDefinition {
                        boolean: (*boolean).to_owned(),
                        variable: (*variable).to_owned(),
                        comparator: Comparator::parse(comparator)
                            .ok_or_else(|| error("expected one of <=, >=, ==, !="))?,
                        value: integer(value)?,
                        constraint: number(constraint)?,
                    })
                }
                ["skip", constraint] => preamble.skipped.push(number(constraint)?),
                ["decomposition", "table", constraint, fresh_variables @ ..] => {
                    preamble.decompositions.push(Decomposition {
                        kind: DecompositionKind::Table,
                        constraint: number(constraint)?,
                        fresh_variables: fresh_variables
                            .iter()
                            .map(|&name| name.to_owned())
                            .collect(),
                    })
                }
                ["objective", variable, constraint] => {
                    preamble.objective = Some(((*variable).to_owned(), number(constraint)?))
                }
                ["first", identifier] => preamble.first_free = Some(number(identifier)?),
                ["breaking", identifier, rest @ ..] => {
                    let posted = match rest {
                        [] => None,
                        ["clause", ..] => {
                            let atoms = line
                                .split_once("clause")
                                .and_then(|(_, atoms)| Atom::parse_list(atoms))
                                .ok_or_else(|| error("expected atoms"))?;
                            Some(Posted::Clause(atoms))
                        }
                        ["lex_less_or_equal", variables @ ..] => {
                            let mut sides = variables.split(|&field| field == ";");
                            let (Some(first), Some(second), None) =
                                (sides.next(), sides.next(), sides.next())
                            else {
                                return Err(error("expected two lists separated by ';'"));
                            };
                            let names =
                                |side: &[&str]| side.iter().map(|&name| name.to_owned()).collect();
                            Some(Posted::LexLessOrEqual(names(first), names(second)))
                        }
                        _ => return Err(error("unknown posted constraint")),
                    };
                    preamble.breaking.push(BreakingItem::Identifier {
                        identifier: number(identifier)?,
                        posted,
                    })
                }
                ["proof"] => in_steps = true,
                _ => return Err(error("unknown item")),
            }
        }
    }

    pub fn render(&self) -> String {
        let mut text = String::new();
        for view in &self.views {
            writeln!(
                text,
                "view {} {} {} {}",
                view.variable, view.representative, view.scale, view.offset
            )
            .unwrap();
        }
        for parameter in &self.parameters {
            writeln!(text, "parameter {parameter}").unwrap();
        }
        for (variable, value) in &self.constants {
            writeln!(text, "constant {variable} {value}").unwrap();
        }
        for literal in &self.literals {
            writeln!(
                text,
                "literal {} {} {} {} {}",
                literal.boolean,
                literal.variable,
                literal.comparator.as_str(),
                literal.value,
                literal.constraint
            )
            .unwrap();
        }
        for constraint in &self.skipped {
            writeln!(text, "skip {constraint}").unwrap();
        }
        for decomposition in &self.decompositions {
            let kind = match decomposition.kind {
                DecompositionKind::Table => "table",
            };
            write!(text, "decomposition {kind} {}", decomposition.constraint).unwrap();
            for name in &decomposition.fresh_variables {
                write!(text, " {name}").unwrap();
            }
            text.push('\n');
        }
        if let Some((variable, constraint)) = &self.objective {
            writeln!(text, "objective {variable} {constraint}").unwrap();
        }
        if let Some(first_free) = self.first_free {
            writeln!(text, "first {first_free}").unwrap();
        }
        for item in &self.breaking {
            match item {
                BreakingItem::Key(line) => writeln!(text, "key {line}").unwrap(),
                BreakingItem::Step(line) => writeln!(text, "step {line}").unwrap(),
                BreakingItem::Identifier { identifier, posted } => {
                    write!(text, "breaking {identifier}").unwrap();
                    match posted {
                        None => {}
                        Some(Posted::Clause(atoms)) => {
                            text.push_str(" clause");
                            for atom in atoms {
                                write!(text, " {atom}").unwrap();
                            }
                        }
                        Some(Posted::LexLessOrEqual(first, second)) => {
                            write!(
                                text,
                                " lex_less_or_equal {} ; {}",
                                first.join(" "),
                                second.join(" ")
                            )
                            .unwrap();
                        }
                    }
                    text.push('\n');
                }
            }
        }
        writeln!(text, "proof").unwrap();
        for step in &self.steps {
            writeln!(text, "{step}").unwrap();
        }
        text
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_rendered_preamble_reads_back() {
        let preamble = Preamble {
            views: vec![View {
                variable: "y".into(),
                representative: "x".into(),
                scale: -2,
                offset: 3,
            }],
            parameters: vec!["view_parameter_0".into()],
            constants: vec![("z".into(), 4)],
            literals: vec![LiteralDefinition {
                boolean: "b".into(),
                variable: "x".into(),
                comparator: Comparator::LessOrEqual,
                value: 2,
                constraint: 3,
            }],
            skipped: vec![1, 2],
            decompositions: vec![Decomposition {
                kind: DecompositionKind::Table,
                constraint: 4,
                fresh_variables: vec!["_t4_0".into(), "_t4_1".into()],
            }],
            objective: Some(("y".into(), 1)),
            first_free: Some(7),
            breaking: vec![
                BreakingItem::Key("p x smaller [x <= -1] c:0 l:initial_domain".into()),
                BreakingItem::Identifier {
                    identifier: 7,
                    posted: Some(Posted::LexLessOrEqual(vec!["x".into()], vec!["z".into()])),
                },
                BreakingItem::Step("y 7 x:z z:x".into()),
                BreakingItem::Step("  under c:0 c:0".into()),
                BreakingItem::Identifier {
                    identifier: 8,
                    posted: Some(Posted::Clause(vec![
                        Atom {
                            variable: "x".into(),
                            comparator: Comparator::LessOrEqual,
                            value: 0,
                        },
                        Atom {
                            variable: "z".into(),
                            comparator: Comparator::GreaterOrEqual,
                            value: -1,
                        },
                    ])),
                },
                BreakingItem::Identifier {
                    identifier: 9,
                    posted: None,
                },
            ],
            steps: vec!["s y := -2*x+3 by c:1".into()],
        };
        let read = Preamble::read(preamble.render().as_bytes()).expect("the rendering reads");
        assert_eq!(read, preamble);
    }

    #[test]
    fn the_head_of_a_scaffold_ends_at_the_marker() {
        let scaffold = "view y x 1 0\nproof\ns y := x by c:1\nnogoods\na 1 [x >= 1]\nn 2 1 0\n";
        let mut reader = scaffold.as_bytes();
        let preamble = Preamble::read_head_of_scaffold(&mut reader).expect("the head reads");
        assert_eq!(preamble.views.len(), 1);
        assert_eq!(preamble.steps, vec!["s y := x by c:1".to_owned()]);
        assert_eq!(reader, "a 1 [x >= 1]\nn 2 1 0\n".as_bytes());
    }

    #[test]
    fn a_scaffold_without_the_marker_is_refused() {
        let error =
            Preamble::read_head_of_scaffold(&mut "proof\na 1 [x >= 1]\n".as_bytes()).unwrap_err();
        assert!(matches!(error, PreambleError::NoMarker));
    }

    #[test]
    fn an_unknown_item_is_refused_with_its_line() {
        let error = Preamble::read("view x y 1 0\nalias x y\n".as_bytes()).unwrap_err();
        assert!(matches!(error, PreambleError::Syntax { line: 2, .. }));
    }
}
