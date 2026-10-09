//! Turns FlatZinc files into examples: one per constraint, with the declarations it refers to.

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use crate::example::Example;
use crate::statements::Statement;
use crate::statements::classify;

/// The `.fzn` files in `paths`, where a directory is searched recursively, in a fixed order.
pub fn find_instances(paths: &[PathBuf]) -> std::io::Result<Vec<PathBuf>> {
    let mut instances = vec![];
    for path in paths {
        collect_instances(path, &mut instances)?;
    }
    instances.sort();
    Ok(instances)
}

fn collect_instances(path: &Path, instances: &mut Vec<PathBuf>) -> std::io::Result<()> {
    if path.is_dir() {
        for entry in std::fs::read_dir(path)? {
            collect_instances(&entry?.path(), instances)?;
        }
    } else if path.extension().is_some_and(|extension| extension == "fzn") {
        instances.push(path.to_owned());
    }
    Ok(())
}

/// One example per constraint of the instance in `source`.
pub fn extract(origin: &str, source: &str) -> Vec<Example> {
    let lines = source.lines().collect::<Vec<_>>();
    let declarations = lines
        .iter()
        .enumerate()
        .filter_map(|(index, line)| match classify(line) {
            Statement::Declaration { name } => Some((name, index)),
            _ => None,
        })
        .collect::<HashMap<_, _>>();

    (0..lines.len())
        .filter_map(|index| {
            Example::from_constraint(
                format!("{origin}:{}", index + 1),
                &lines,
                &declarations,
                index,
            )
        })
        .collect()
}

/// Collapses the examples that are equal up to the names of their variables into the first of
/// them, counting how often each occurs.
pub fn deduplicate(examples: Vec<Example>) -> Vec<Example> {
    let mut index_of_key: HashMap<String, usize> = HashMap::new();
    let mut unique: Vec<Example> = vec![];

    for example in examples {
        let key = example.canonical_key();
        if let Some(&index) = index_of_key.get(&key) {
            unique[index].occurrences += example.occurrences;
        } else {
            let _ = index_of_key.insert(key, unique.len());
            unique.push(example);
        }
    }

    unique
}
