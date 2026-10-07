use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write;

use crate::statements::Statement;
use crate::statements::classify;
use crate::statements::identifiers;

/// A FlatZinc instance with one constraint, which the fuzzer compiles and propagates on its own.
#[derive(Clone, Debug)]
pub struct Example {
    /// Where the example comes from: a line of a FlatZinc file, a random generation or a mutation
    /// of another example.
    pub origin: String,
    /// The name of the constraint, such as `int_lin_le`.
    pub constraint_name: String,
    /// The complete instance: the declarations the constraint refers to, the constraint and a
    /// solve item.
    pub source: String,
    /// How many constraints of the sources are equal to this one up to the names of variables.
    pub occurrences: usize,
}

impl Example {
    /// The example for the constraint on line `constraint_line` of `lines`, which keeps the
    /// declarations that the constraint refers to, directly or through an array.
    pub(crate) fn from_constraint(
        origin: String,
        lines: &[&str],
        declarations: &HashMap<&str, usize>,
        constraint_line: usize,
    ) -> Option<Example> {
        let Statement::Constraint { name } = classify(lines[constraint_line]) else {
            return None;
        };

        let mut included = HashSet::new();
        let mut pending = vec![constraint_line];
        while let Some(line) = pending.pop() {
            let text = lines[line];
            for (start, end) in identifiers(text) {
                if let Some(&declaration) = declarations.get(&text[start..end])
                    && included.insert(declaration)
                {
                    pending.push(declaration);
                }
            }
        }

        let mut included = included.into_iter().collect::<Vec<_>>();
        included.sort_unstable();

        let mut source = String::new();
        for line in included {
            writeln!(source, "{}", lines[line].trim()).expect("writing to a string succeeds");
        }
        writeln!(source, "{}", lines[constraint_line].trim())
            .expect("writing to a string succeeds");
        source.push_str("solve satisfy;\n");

        Some(Example {
            origin,
            constraint_name: name.to_owned(),
            source,
            occurrences: 1,
        })
    }

    /// The source with the declared names replaced by `v0`, `v1`, ... in the order in which the
    /// constraint refers to them, and without annotations on declarations. Two examples with the
    /// same key differ only in the names of their variables.
    pub(crate) fn canonical_key(&self) -> String {
        let lines = self.source.lines().collect::<Vec<_>>();
        let declarations = lines
            .iter()
            .enumerate()
            .filter_map(|(index, line)| match classify(line) {
                Statement::Declaration { name } => Some((name, index)),
                _ => None,
            })
            .collect::<HashMap<_, _>>();
        let Some(constraint_line) = lines
            .iter()
            .position(|line| matches!(classify(line), Statement::Constraint { .. }))
        else {
            return self.source.clone();
        };

        let mut renaming: HashMap<&str, usize> = HashMap::new();
        let mut order = vec![constraint_line];
        let mut position = 0;
        while position < order.len() {
            let text = lines[order[position]];
            for (start, end) in identifiers(text) {
                let identifier = &text[start..end];
                if let Some(&declaration) = declarations.get(identifier)
                    && !renaming.contains_key(identifier)
                {
                    let _ = renaming.insert(identifier, renaming.len());
                    order.push(declaration);
                }
            }
            position += 1;
        }

        let mut key = String::new();
        for line in order {
            let text = strip_declaration_annotations(lines[line]);
            let mut last = 0;
            for (start, end) in identifiers(&text) {
                if let Some(index) = renaming.get(&text[start..end]) {
                    key.push_str(&text[last..start]);
                    write!(key, "v{index}").expect("writing to a string succeeds");
                    last = end;
                }
            }
            key.push_str(&text[last..]);
            key.push('\n');
        }
        key
    }
}

/// Removes the annotations of a declaration, such as `:: output_var`, which do not change the
/// constraint.
fn strip_declaration_annotations(line: &str) -> String {
    if !matches!(classify(line), Statement::Declaration { .. }) {
        return line.trim().to_owned();
    }

    let mut result = String::new();
    let mut rest = line.trim();
    while let Some(start) = rest.find("::") {
        result.push_str(rest[..start].trim_end());
        let after = &rest[start + 2..];
        let end = after.find(['=', ';']).unwrap_or(after.len());
        rest = &after[end..];
        if rest.starts_with('=') {
            result.push(' ');
        }
    }
    result.push_str(rest);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example(source: &str) -> Example {
        Example {
            origin: String::new(),
            constraint_name: String::new(),
            source: source.to_owned(),
            occurrences: 1,
        }
    }

    #[test]
    fn examples_equal_up_to_naming_have_the_same_key() {
        let first = example(
            "var 0..5: y;\nvar 0..5: x :: output_var;\nconstraint int_lin_le([1, -1], [x, y], 0);\nsolve satisfy;\n",
        );
        let second = example(
            "var 0..5: a;\nvar 0..5: b;\nconstraint int_lin_le([1, -1], [a, b], 0);\nsolve satisfy;\n",
        );
        let different = example(
            "var 0..5: a;\nvar 0..6: b;\nconstraint int_lin_le([1, -1], [a, b], 0);\nsolve satisfy;\n",
        );

        assert_eq!(first.canonical_key(), second.canonical_key());
        assert_ne!(first.canonical_key(), different.canonical_key());
    }

    #[test]
    fn the_declarations_of_an_array_are_included() {
        let lines = [
            "var 0..5: x;",
            "var 0..5: unused;",
            "var 0..5: y;",
            "array [1..2] of var int: xs = [x, y];",
            "constraint array_int_maximum(x, xs);",
        ];
        let declarations = lines
            .iter()
            .enumerate()
            .filter_map(|(index, line)| match classify(line) {
                Statement::Declaration { name } => Some((name, index)),
                _ => None,
            })
            .collect::<HashMap<_, _>>();

        let example = Example::from_constraint(String::new(), &lines, &declarations, 4)
            .expect("line 4 is a constraint");

        assert_eq!(
            example.source,
            "var 0..5: x;\nvar 0..5: y;\narray [1..2] of var int: xs = [x, y];\nconstraint array_int_maximum(x, xs);\nsolve satisfy;\n"
        );
    }
}
