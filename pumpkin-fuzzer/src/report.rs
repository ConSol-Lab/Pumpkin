//! Groups the failures by cause and describes each group, with a reproducer.

use std::collections::BTreeMap;
use std::fmt::Write;
use std::path::Path;

use crate::driver::Failure;

/// The failures with the same cause, with the first one found as the representative.
#[derive(Debug)]
pub struct FailureGroup {
    pub representative: Failure,
    pub count: usize,
    pub origins: Vec<String>,
}

pub fn group(failures: Vec<Failure>) -> Vec<FailureGroup> {
    let mut groups: BTreeMap<String, FailureGroup> = BTreeMap::new();
    for failure in failures {
        let key = failure.group_key();
        match groups.get_mut(&key) {
            Some(group) => {
                group.count += 1;
                group.origins.push(failure.example.origin.clone());
                // Keep the representative with the fewest moves, which is the easiest to read.
                if failure.is_reproducible && failure.moves.len() < group.representative.moves.len()
                {
                    group.representative = failure;
                }
            }
            None => {
                let origins = vec![failure.example.origin.clone()];
                let _ = groups.insert(
                    key,
                    FailureGroup {
                        representative: failure,
                        count: 1,
                        origins,
                    },
                );
            }
        }
    }
    let mut groups = groups.into_values().collect::<Vec<_>>();
    groups.sort_by_key(|group| {
        (
            group.representative.oracle,
            group.representative.example.constraint_name.clone(),
        )
    });
    groups
}

/// Writes the instance and the moves of the representative to `directory`, and returns the
/// description of the group.
pub fn describe(group: &FailureGroup, number: usize, directory: &Path) -> std::io::Result<String> {
    let failure = &group.representative;
    let moves = failure
        .moves
        .iter()
        .map(|applied_move| format!("{applied_move}\n"))
        .collect::<String>();

    std::fs::create_dir_all(directory)?;
    let instance_path = directory.join(format!("failure-{number}.fzn"));
    let moves_path = directory.join(format!("failure-{number}.moves"));
    std::fs::write(&instance_path, &failure.example.source)?;
    std::fs::write(&moves_path, &moves)?;

    let mut text = String::new();
    let mut line = |content: String| {
        text.push_str(&content);
        text.push('\n');
    };

    line(format!(
        "=== Failure {number}: {:?} on '{}', in {} case(s)",
        failure.oracle, failure.example.constraint_name, group.count
    ));
    line(String::new());
    line(failure.message.clone());
    for log in &failure.logs {
        line(format!("  logged: {log}"));
    }
    line(String::new());
    line(format!(
        "Example: {} (one of {} equal up to naming)",
        failure.example.origin, failure.example.occurrences
    ));
    for source_line in failure.example.source.lines() {
        line(format!("    {source_line}"));
    }
    if group.origins.len() > 1 {
        let mut others = group.origins[1..]
            .iter()
            .take(5)
            .cloned()
            .collect::<Vec<_>>();
        if group.origins.len() > 6 {
            others.push(format!("and {} more", group.origins.len() - 6));
        }
        line(format!("Also found in: {}", others.join(", ")));
    }
    line(String::new());

    if failure.is_reproducible {
        line(format!(
            "Moves, shrunk from {} to {}; the last one fails:",
            failure.moves_before_shrinking,
            failure.moves.len()
        ));
    } else {
        line(
            "Moves; replaying them on a fresh state does not fail, so the failure depends on \
             something outside the moves:"
                .to_owned(),
        );
    }
    if failure.moves.is_empty() {
        line("    (none: the failure occurs when the example is compiled and propagated at the root)".to_owned());
    }
    for (index, applied_move) in failure.moves.iter().enumerate() {
        line(format!("    {}. {applied_move}", index + 1));
    }
    if !failure.domains_before.is_empty() {
        line("Domains before the last move:".to_owned());
        text_push_indented(&mut line, &failure.domains_before);
    }
    line(String::new());

    line("Reproduce:".to_owned());
    line(format!(
        "    cargo run -p pumpkin-fuzzer --profile fuzz --features checks -- --replay {} --moves {}",
        instance_path.display(),
        moves_path.display()
    ));
    line("As a regression test:".to_owned());
    let mut test = String::new();
    writeln!(test, "    #[test]").expect("writing to a string succeeds");
    writeln!(
        test,
        "    fn {}_regression() {{",
        failure.example.constraint_name.to_lowercase()
    )
    .expect("writing to a string succeeds");
    writeln!(test, "        pumpkin_fuzzer::replay(").expect("writing to a string succeeds");
    writeln!(test, "            {:?},", failure.example.source)
        .expect("writing to a string succeeds");
    writeln!(test, "            {moves:?},").expect("writing to a string succeeds");
    writeln!(test, "        );").expect("writing to a string succeeds");
    write!(test, "    }}").expect("writing to a string succeeds");
    line(test);

    Ok(text)
}

fn text_push_indented(line: &mut impl FnMut(String), text: &str) {
    for text_line in text.lines() {
        line(format!("  {text_line}"));
    }
}
