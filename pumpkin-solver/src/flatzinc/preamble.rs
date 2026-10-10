//! The preamble of a croc scaffold: the preprocessing the compiler did, stated for the proof
//! processor.

use std::fmt::Write as _;

/// The preprocessing a solver states in the preamble of its scaffold.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Preamble {
    pub decompositions: Vec<Decomposition>,
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
        writeln!(text, "proof").unwrap();
        text
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_preamble_renders_one_item_per_line_before_its_steps() {
        let preamble = Preamble {
            decompositions: vec![Decomposition {
                kind: DecompositionKind::Table,
                constraint: 4,
                fresh_variables: vec!["_t4_0".into(), "_t4_1".into()],
            }],
        };
        assert_eq!(
            preamble.render(),
            "decomposition table 4 _t4_0 _t4_1\nproof\n"
        );
    }
}
