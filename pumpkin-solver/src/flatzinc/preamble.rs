//! The preamble of a croc scaffold: the preprocessing the compiler did, stated for the proof
//! processor.

use std::fmt::Write as _;

/// The preprocessing a solver states in the preamble of its scaffold.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Preamble {
    pub views: Vec<View>,
    pub parameters: Vec<String>,
    pub skipped: Vec<u64>,
    pub decompositions: Vec<Decomposition>,
    /// The objective variable when it is a view, with the constraint that relates it to its
    /// representative.
    pub objective: Option<(String, u64)>,
    pub first_free: Option<u64>,
    pub steps: Vec<String>,
}

/// `variable = scale * representative + offset`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct View {
    pub variable: String,
    pub representative: String,
    pub scale: i64,
    pub offset: i64,
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

impl Preamble {
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
    fn a_preamble_renders_one_item_per_line_before_its_steps() {
        let preamble = Preamble {
            views: vec![View {
                variable: "y".into(),
                representative: "x".into(),
                scale: -2,
                offset: 3,
            }],
            skipped: vec![1],
            first_free: Some(7),
            steps: vec!["s y := -2*x+3 by c:1".into()],
            ..Preamble::default()
        };
        assert_eq!(
            preamble.render(),
            "view y x -2 3\nskip 1\nfirst 7\nproof\ns y := -2*x+3 by c:1\n"
        );
    }
}
