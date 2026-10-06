use std::sync::OnceLock;

/// Whether the checkers of the rule named `rule_name` are added and run.
///
/// A debugging aid to check a chosen set of rules. Both environment variables hold rule names
/// separated by commas, as the checker failures print them:
/// - `PUMPKIN_CHECK_RULES`: only these rules are checked. When it is unset or empty, every rule is
///   checked.
/// - `PUMPKIN_SKIP_RULES`: these rules are not checked.
pub(crate) fn is_rule_checked(rule_name: &str) -> bool {
    static FILTER: OnceLock<RuleFilter> = OnceLock::new();

    FILTER
        .get_or_init(|| RuleFilter {
            checked: rule_names_in("PUMPKIN_CHECK_RULES"),
            skipped: rule_names_in("PUMPKIN_SKIP_RULES"),
        })
        .is_checked(rule_name)
}

fn rule_names_in(environment_variable: &str) -> Vec<String> {
    std::env::var(environment_variable)
        .unwrap_or_default()
        .split(',')
        .map(|name| name.trim().to_owned())
        .filter(|name| !name.is_empty())
        .collect()
}

#[derive(Debug, Default)]
struct RuleFilter {
    /// When empty, every rule that is not skipped is checked.
    checked: Vec<String>,
    skipped: Vec<String>,
}

impl RuleFilter {
    fn is_checked(&self, rule_name: &str) -> bool {
        let is_selected =
            self.checked.is_empty() || self.checked.iter().any(|name| name == rule_name);
        let is_skipped = self.skipped.iter().any(|name| name == rule_name);

        is_selected && !is_skipped
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(names: &[&str]) -> Vec<String> {
        names.iter().map(|&name| name.to_owned()).collect()
    }

    #[test]
    fn every_rule_is_checked_without_a_filter() {
        assert!(RuleFilter::default().is_checked("nogood"));
    }

    #[test]
    fn only_the_listed_rules_are_checked() {
        let filter = RuleFilter {
            checked: names(&["nogood"]),
            skipped: vec![],
        };

        assert!(filter.is_checked("nogood"));
        assert!(!filter.is_checked("linear_le"));
    }

    #[test]
    fn a_skipped_rule_is_not_checked() {
        let filter = RuleFilter {
            checked: vec![],
            skipped: names(&["nogood"]),
        };

        assert!(!filter.is_checked("nogood"));
        assert!(filter.is_checked("linear_le"));
    }

    #[test]
    fn skipping_wins_over_selecting() {
        let filter = RuleFilter {
            checked: names(&["nogood"]),
            skipped: names(&["nogood"]),
        };

        assert!(!filter.is_checked("nogood"));
    }
}
